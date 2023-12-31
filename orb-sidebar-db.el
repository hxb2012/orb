;;; orb-sidebar-db.el --- 通过SQLite查询链接 -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;; 最初是从org-roam-mode.el里抄来的

;;; Commentary:

;;; Code:
(defvar orb-sidebar--current-buffer)
(defvar orb-property-refs)

(declare-function orb-sidebar-link--search "orb-sidebar-link" (s avoid-pos))
(declare-function orb-sidebar-link--normalize-search "orb-sidebar-link" (s))
(declare-function orb-sidebar-link--init-temp-buffer "orb-sidebar-link" (file))

(defun orb-sidebar-db--find-refs-file (type path)
  (if-let ((result
            (orb-db-query
             (list "SELECT file.path
FROM %s.file AS file
JOIN %s.link AS link
ON link.file_id = file.file_id
JOIN %s.property AS prop
ON link.node_id = prop.node_id
AND link.begin >= prop.begin
AND link.end <= prop.end
WHERE link.type = ?
AND link.path = ?
AND prop.key = ?"
                   type path orb-property-refs)
             t)))
      (let ((file-name (file-name-concat orb-directory (caar result))))
        (or
         (org-find-base-buffer-visiting file-name)
         file-name))
    (current-buffer)))

(defun orb-sidebar-db--link-target-section-a (fun overlay)
  (orb-db-with-transaction
    (cl-letf (((symbol-function 'orb-sidebar-link--find-refs-file)
               #'orb-sidebar-db--find-refs-file))
      (funcall fun overlay))))

(defun orb-sidebar-db--get-links (type path)
  (orb-db-query
   (list "SELECT (? || file.path), link.begin, link.end
FROM %s.link AS link
JOIN %s.file AS file
ON file.file_id = link.file_id
WHERE link.type = ?
AND link.path = ?
AND (? || file.path) != ?"
         orb-directory type path orb-directory buffer-file-name)
   t))

(defun orb-sidebar-db--get-matches (links headline)
  (pcase-let*
      ((`(,matches ,regexps ,table ,get-links) links)
       (pos (point))
       (all-matches
        (append
         (mapcar
          'cdr
          (cond
           ((numberp headline)
            (seq-filter
             (lambda (x) (and (>= (caar x) pos) (>= headline (cdar x))))
             matches))
           ((not headline)
            (seq-filter
             (lambda (x) (and (<= (caar x) pos) (< pos (cdar x))))
             matches))))
         (when (or headline
                   (or (eq pos (point-min)) (org-at-heading-p)))
           (append
            (when-let ((id (org-entry-get pos "ID")))
              (funcall get-links "id" id))
            (when-let ((id (org-entry-get pos "CUSTOM_ID")))
              (gethash id table))
            (when-let*
                ((id (org-entry-get pos orb-property-refs))
                 (link (with-temp-buffer
                         (let ((org-inhibit-startup nil))
                           (insert id)
                           (org-mode)
                           (goto-char (point-min))
                           (org-element-link-parser)))))
              (funcall
               get-links
               (org-element-property :type link)
               (org-element-property :path link))))))))
    (pcase-dolist (`(,value . ,re) regexps)
      (let ((case-fold-search
             (if (eq org-occur-case-fold-search 'smart)
                 (isearch-no-upper-case-p re t)
               org-occur-case-fold-search)))
        (when (cond
               ((numberp headline)
                (with-restriction pos headline
                  (goto-char pos)
                  (re-search-forward re nil t)))
               ((not headline) (org-in-regexp re)))
          (push value all-matches))))
    all-matches))

(defun orb-sidebar-db--compare-pos (x y)
  (let ((a (car x))
        (b (car y)))
    (cond
     ((string-lessp a b)
      t)
     ((string-equal a b)
      (< (cadr x) (cadr y))))))

(defun orb-sidebar-db--display-links (buffer links &optional highlight)
  (let ((last-pos nil)
        (last-buffer nil))
    (pcase-dolist (`(,file . ,rest) (seq-sort 'orb-sidebar-db--compare-pos links))
      (unless (and last-buffer
                   (equal file (buffer-file-name last-buffer)))
        (when (and last-buffer
                   (not (buffer-file-name last-buffer)))
          (kill-buffer last-buffer))
        (setq last-buffer (org-find-base-buffer-visiting file))
        (unless last-buffer
          (setq last-buffer (generate-new-buffer " *temp*" t))
          (with-current-buffer last-buffer
            (orb-sidebar-link--init-temp-buffer file))))
      (with-current-buffer last-buffer
        (let ((buffer-file-name file)
              (default-directory (file-name-directory file)))
          (org-with-wide-buffer
           (goto-char (car rest))
           (setq last-pos
                 (apply
                  (or highlight 'orb-sidebar-link--highlight-match)
                  buffer last-pos rest))))))
    (when (and last-buffer
               (not (buffer-file-name last-buffer)))
      (kill-buffer last-buffer))))

(defun orb-sidebar-db--resolve-search-option (option)
  (if (string-match-p "\\`[0-9]+\\'" option)
      (let ((line (string-to-number option)))
        (goto-char (point-min))
        (forward-line (1- line))
        (cons 'match (cons (point) (line-end-position))))
    (let ((path (orb-sidebar-link--normalize-search option)))
      (cond
       ((eq (string-to-char path) ?#)
        (cons 'custom-id (substring path 1)))
       ((string-match "\\`/\\(.*\\)/\\'"
                      (replace-regexp-in-string "\n[ \t]*" " " path))
        (cons 'regexp (match-string 1 path)))
       ((orb-sidebar-link--search path nil)
        (cons 'match (cons (match-beginning 0) (match-end 0))))))))

(defun orb-sidebar-db--collect ()
  (let* ((matches nil)
         (regexps nil)
         (table (make-hash-table :test 'equal))
         (levels (orb-db--file-name-levels orb-directory))
         (result
          (orb-db-query
           (list "SELECT link.search_option, (? || file.path), link.begin, link.end
FROM %s.link as link
JOIN %s.file AS file
ON file.file_id = link.file_id
WHERE link.type = 'file'
AND (? || file.path) != ?
AND coalesce(link.application, '') != 'sys'
AND link.search_option IS NOT NULL
AND ? = (
  CASE
  WHEN link.relative IS NULL THEN
    link.path
  WHEN link.relative < coalesce(json_array_length(file.levels), 0) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - coalesce(json_array_length(file.levels),0) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - coalesce(json_array_length(file.levels), 0)) || ']')) || substr(link.path, 1+3*link.relative)
  END)"
                 orb-directory
                 orb-directory buffer-file-name
                 buffer-file-name
                 orb-directory
                 (length levels)
                 orb-directory (when levels (json-encode levels)))
           t)))
    (dolist (entry result)
      (let ((value (cdr entry)))
        (pcase (orb-sidebar-db--resolve-search-option (car entry))
          (`(match . ,match)
           (push (cons match value) matches))
          (`(regexp . ,regexp)
           (push (cons value regexp) regexps))
          (`(custom-id . ,key)
           (puthash key (cons value (gethash key table)) table)))))
    (list matches regexps table 'orb-sidebar-db--get-links)))

(defun orb-sidebar-db--local-overlay-get-create (overlay)
  (if-let ((ov (overlay-get overlay 'orb-overlay)))
      ov
    (let ((start (point)))
      (insert (propertize "\f" 'display ""))
      (let* ((end (point))
             (ov (make-overlay start end)))
        (overlay-put ov 'evaporate t)
        (overlay-put overlay 'orb-overlay ov)
        ov))))

(defun orb-sidebar-db--link-back-section-a (fun overlay &optional headline collect display)
  (let* ((output-buffer (current-buffer))
         (ov (orb-sidebar-db--local-overlay-get-create overlay))
         (old-pos (overlay-get overlay 'orb-last-pos)))
    (with-restriction (overlay-start ov) (1- (overlay-end ov))
      (funcall fun overlay headline collect display))
    (orb-db-refresh
     (remove
      (buffer-file-name orb-sidebar--current-buffer)
      (mapcar 'buffer-file-name (orb-buffer-list))))
    (orb-db-with-transaction
      (let* ((tick (orb-db-tick))
             (modified (not (equal tick (overlay-get overlay 'orb-db-tick))))
             (pos (overlay-get overlay 'orb-last-pos))
             (links
              (if modified
                  (with-current-buffer orb-sidebar--current-buffer
                    (org-with-wide-buffer
                     (orb-sidebar-db--collect)))
                (overlay-get overlay 'orb-db-links))))
        (when modified
          (overlay-put overlay 'orb-db-tick tick)
          (overlay-put overlay 'orb-db-links links))
        (when (or modified (not (eq old-pos pos)))
          (goto-char (overlay-end ov))
          (with-restriction (point) (point-max)
            (delete-region (point-min) (point-max))
            (with-current-buffer orb-sidebar--current-buffer
              (org-with-wide-buffer
               (goto-char pos)
               (orb-sidebar-db--display-links
                output-buffer
                (orb-sidebar-db--get-matches
                 links
                 (when headline
                   (if (org-at-heading-p) (line-end-position) t))))))))))))

(defun orb-sidebar-db--get-todos (type path)
  (orb-db-query
   (list "SELECT (? || file.path), node.point
FROM %s.link AS link
JOIN %s.node AS node
ON node.node_id = link.node_id
JOIN %s.file AS file
ON file.file_id = node.file_id
WHERE node.level > 0
AND node.todo IS NOT NULL
AND link.type = ?
AND link.path = ?
AND (? || file.path) != ?"
         orb-directory type path orb-directory buffer-file-name)
   t))

(defun orb-sidebar-db--collect-todo ()
  (let* ((matches nil)
         (regexps nil)
         (table (make-hash-table :test 'equal))
         (table1 (make-hash-table :test 'equal))
         (levels (orb-db--file-name-levels orb-directory))
         (result
          (orb-db-query
           (list "SELECT link.search_option, (? || file.path), node.point
FROM %s.link as link
JOIN %s.node AS node
ON node.node_id = link.node_id
JOIN %s.file AS file
ON file.file_id = node.file_id
WHERE node.level > 0
AND node.todo IS NOT NULL
AND link.type = 'file'
AND (? || file.path) != ?
AND coalesce(link.application, '') != 'sys'
AND link.search_option IS NOT NULL
AND ? = (
  CASE
  WHEN link.relative IS NULL THEN
    link.path
  WHEN link.relative < coalesce(json_array_length(file.levels), 0) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - coalesce(json_array_length(file.levels), 0) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - coalesce(json_array_length(file.levels), 0)) || ']')) || substr(link.path, 1+3*link.relative)
  END)"
                 orb-directory
                 orb-directory buffer-file-name
                 buffer-file-name
                 orb-directory
                 (length levels)
                 orb-directory (when levels (json-encode levels)))
           t)))
    (dolist (entry result)
      (let ((value (cdr entry)))
        (pcase (orb-sidebar-db--resolve-search-option (car entry))
          (`(match . ,match)
           (push (cons match value) matches))
          (`(regexp . ,regexp)
           (push (cons value regexp) regexps))
          (`(custom-id . ,key)
           (puthash key (cons value (gethash key table)) table)))))
    (maphash
     (lambda (key value)
       (puthash key (seq-uniq value) table1))
     table)
    (list matches regexps table1 'orb-sidebar-db--get-todos)))

(defun orb-sidebar-db--todo-section-a (fun overlay)
  (cl-letf* (((symbol-function 'orb-sidebar-db--collect)
              #'orb-sidebar-db--collect-todo)
             (orb-sidebar-db--display-links
              (symbol-function 'orb-sidebar-db--display-links))
             ((symbol-function 'orb-sidebar-db--display-links)
              (lambda (buffer links)
                (org-compile-prefix-format 'agenda)
                (funcall orb-sidebar-db--display-links
                         buffer links 'orb-sidebar-todo--insert-item))))
    (funcall fun overlay)))

;;;###autoload
(define-minor-mode orb-sidebar-db-mode
  ""
  :group 'orb-sidebar
  :global t
  :init-value nil
  (cond
   (orb-sidebar-db-mode
    (advice-add 'orb-sidebar-link-target-section
                :around 'orb-sidebar-db--link-target-section-a)
    (advice-add 'orb-sidebar-link-back-section
                :around 'orb-sidebar-db--link-back-section-a)
    (advice-add 'orb-sidebar-todo-section
                :around 'orb-sidebar-db--todo-section-a))
   (t
    (advice-remove 'orb-sidebar-todo-section
                   'orb-sidebar-db--todo-section-a)
    (advice-remove 'orb-sidebar-link-back-section
                   'orb-sidebar-db--link-back-section-a)
    (advice-remove 'orb-sidebar-link-target-section
                   'orb-sidebar-db--link-target-section-a))))

(provide 'orb-sidebar-db)
;;; orb-sidebar-db.el ends here
