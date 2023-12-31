;;; orb-remu.el --- 通过SQLite查询反向链接 -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Commentary:

;; 最初是从org-roam-mode.el里抄来的

;;; Code:

(require 'remu-link)

(defun orb-remu--get-links (type path)
  (orb-db-select
   "
SELECT (? || file.path), link.begin, link.end
FROM temp.link AS link
JOIN temp.file AS file
ON file.file_id = link.file_id
WHERE link.type = ?
AND link.path = ?
AND (? || file.path) != ?
UNION
SELECT (? || file.path), link.begin, link.end
FROM main.link AS link
JOIN main.file AS file
ON file.file_id = link.file_id
WHERE link.type = ?
AND link.path = ?
AND (? || file.path) != ?
AND NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)"
   (list orb-directory type path orb-directory buffer-file-name
         orb-directory type path orb-directory buffer-file-name)))

(defun orb-remu--get-matches (links headline)
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
                ((id (org-entry-get pos remu-link-property-refs))
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

(defun orb-remu--compare-pos (x y)
  (let ((a (car x))
        (b (car y)))
    (cond
     ((string-lessp a b)
      t)
     ((string-equal a b)
      (< (cadr x) (cadr y))))))

(defun orb-remu--display-links (buffer links &optional highlight)
  (let ((last-pos nil)
        (last-buffer nil))
    (pcase-dolist (`(,file . ,rest) (seq-sort 'orb-remu--compare-pos links))
      (unless (and last-buffer
                   (equal file (buffer-file-name last-buffer)))
        (when (and last-buffer
                   (not (buffer-file-name last-buffer)))
          (kill-buffer last-buffer))
        (setq last-buffer (org-find-base-buffer-visiting file))
        (unless last-buffer
          (setq last-buffer (generate-new-buffer " *temp*" t))
          (with-current-buffer last-buffer
            (remu-link--init-temp-buffer file))))
      (with-current-buffer last-buffer
        (let ((buffer-file-name file)
              (default-directory (file-name-directory file)))
          (org-with-wide-buffer
           (goto-char (car rest))
           (setq last-pos
                 (apply
                  (or highlight 'remu-link--highlight-match)
                  buffer last-pos rest))))))
    (when (and last-buffer
               (not (buffer-file-name last-buffer)))
      (kill-buffer last-buffer))))

(defun orb-remu--resolve-search-option (option)
  (if (string-match-p "\\`[0-9]+\\'" option)
      (let ((line (string-to-number option)))
        (goto-char (point-min))
        (forward-line (1- line))
        (cons 'match (cons (point) (line-end-position))))
    (let ((path (remu-link--normalize-search option)))
      (cond
       ((eq (string-to-char path) ?#)
        (cons 'custom-id (substring path 1)))
       ((string-match "\\`/\\(.*\\)/\\'"
                      (replace-regexp-in-string "\n[ \t]*" " " path))
        (cons 'regexp (match-string 1 path)))
       ((remu-link--search path nil)
        (cons 'match (cons (match-beginning 0) (match-end 0))))))))

(defun orb-remu--collect ()
  (let* ((matches nil)
         (regexps nil)
         (table (make-hash-table :test 'equal))
         (levels (orb--file-name-levels orb-directory))
         (result
          (orb-db-select
           "
SELECT link.search_option, (? || file.path), link.begin, link.end
FROM temp.link as link
JOIN temp.file AS file
ON file.file_id = link.file_id
WHERE link.type = 'file'
AND (? || file.path) != ?
AND coalesce(link.application, '') != 'sys'
AND link.search_option IS NOT NULL
AND ? = (
  CASE
  WHEN link.relative IS NULL THEN
    link.path
  WHEN link.relative < json_array_length(file.levels) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - json_array_length(file.levels) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - json_array_length(file.levels)) || ']')) || substr(link.path, 1+3*link.relative)
  END)
UNION
SELECT link.search_option, (? || file.path), link.begin, link.end
FROM main.link as link
JOIN main.file AS file
ON file.file_id = link.file_id
WHERE link.type = 'file'
AND (? || file.path) != ?
AND coalesce(link.application, '') != 'sys'
AND link.search_option IS NOT NULL
AND ? = (
  CASE
  WHEN link.relative IS NULL THEN
    link.path
  WHEN link.relative < json_array_length(file.levels) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - json_array_length(file.levels) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - json_array_length(file.levels)) || ']')) || substr(link.path, 1+3*link.relative)
  END)
AND NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)"
           (list
            orb-directory
            orb-directory
            buffer-file-name
            buffer-file-name
            orb-directory
            (length levels)
            orb-directory
            (when levels
              (json-encode levels))
            orb-directory
            orb-directory
            buffer-file-name
            buffer-file-name
            orb-directory
            (length levels)
            orb-directory
            (when levels
              (json-encode levels))))))
    (dolist (entry result)
      (let ((value (cdr entry)))
        (pcase (orb-remu--resolve-search-option (car entry))
          (`(match . ,match)
           (push (cons match value) matches))
          (`(regexp . ,regexp)
           (push (cons value regexp) regexps))
          (`(custom-id . ,key)
           (puthash key (cons value (gethash key table)) table)))))
    (list matches regexps table 'orb-remu--get-links)))

(defun orb-remu-local-overlay-get-create (overlay)
  (if-let ((ov (overlay-get overlay 'orb-overlay)))
      ov
    (let ((start (point)))
      (insert (propertize "\f" 'display ""))
      (let* ((end (point))
             (ov (make-overlay start end)))
        (overlay-put ov 'evaporate t)
        (overlay-put overlay 'orb-overlay ov)
        ov))))

;;;###autoload
(defun orb-remu-backlink-section (overlay &optional headline local collect display)
  (let* ((output-buffer (current-buffer))
         (ov (orb-remu-local-overlay-get-create overlay))
         (old-pos (overlay-get overlay 'remu-last-pos)))
    (with-restriction (overlay-start ov) (1- (overlay-end ov))
      (if local
          (funcall local overlay)
        (remu-link-back-section overlay headline)))
    (orb-db-refresh
     (remove
      (buffer-file-name remu--current-buffer)
      (mapcar 'buffer-file-name (orb-buffer-list))))
    (orb-db-with-transaction
      (let* ((tick (orb-db-tick))
             (modified (not (equal tick (overlay-get overlay 'orb-tick))))
             (pos (overlay-get overlay 'remu-last-pos))
             (links
              (if modified
                  (with-current-buffer remu--current-buffer
                    (org-with-wide-buffer
                     (if collect
                         (funcall collect)
                       (orb-remu--collect))))
                (overlay-get overlay 'orb-links))))
        (when (or modified (not (eq old-pos pos)))
          (goto-char (overlay-end ov))
          (with-restriction (point) (point-max)
            (delete-region (point-min) (point-max))
            (with-current-buffer remu--current-buffer
              (org-with-wide-buffer
               (goto-char pos)
               (funcall
                (or display 'orb-remu--display-links)
                output-buffer
                (orb-remu--get-matches
                 links
                 (when headline
                   (if (org-at-heading-p) (line-end-position) t))))))))))))

;;;###autoload
(defun orb-remu-backlink-headline-section (overlay)
  (orb-remu-backlink-section overlay t))

(defun orb-remu--get-todos (type path)
  (orb-db-select
   "
SELECT (? || file.path), node.point
FROM temp.link AS link
JOIN temp.node AS node
ON node.node_id = link.node_id
JOIN temp.file AS file
ON file.file_id = node.file_id
WHERE node.level > 0
AND node.todo IS NOT NULL
AND link.type = ?
AND link.path = ?
AND (? || file.path) != ?
UNION
SELECT (? || file.path), node.point
FROM main.link AS link
JOIN main.node AS node
ON node.node_id = link.node_id
JOIN main.file AS file
ON file.file_id = node.file_id
WHERE node.level > 0
AND node.todo IS NOT NULL
AND link.type = ?
AND link.path = ?
AND (? || file.path) != ?
AND NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)"
   (list orb-directory type path orb-directory buffer-file-name
         orb-directory type path orb-directory buffer-file-name)))

(defun orb-remu--collect-todo ()
  (let* ((matches nil)
         (regexps nil)
         (table (make-hash-table :test 'equal))
         (table1 (make-hash-table :test 'equal))
         (levels (orb--file-name-levels orb-directory))
         (result
          (orb-db-select
           "
SELECT link.search_option, (? || file.path), node.point
FROM temp.link as link
JOIN temp.node AS node
ON node.node_id = link.node_id
JOIN temp.file AS file
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
  WHEN link.relative < json_array_length(file.levels) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - json_array_length(file.levels) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - json_array_length(file.levels)) || ']')) || substr(link.path, 1+3*link.relative)
  END)
UNION
SELECT link.search_option, (? || file.path), node.point
FROM main.link as link
JOIN main.node AS node
ON node.node_id = link.node_id
JOIN main.file AS file
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
  WHEN link.relative < json_array_length(file.levels) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - json_array_length(file.levels) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - json_array_length(file.levels)) || ']')) || substr(link.path, 1+3*link.relative)
  END)
AND NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)"
           (list
            orb-directory
            orb-directory
            buffer-file-name
            buffer-file-name
            orb-directory
            (length levels)
            orb-directory
            (when levels
              (json-encode levels))
            orb-directory
            orb-directory
            buffer-file-name
            buffer-file-name
            orb-directory
            (length levels)
            orb-directory
            (when levels
              (json-encode levels))))))
    (dolist (entry result)
      (let ((value (cdr entry)))
        (pcase (orb-remu--resolve-search-option (car entry))
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
    (list matches regexps table1 'orb-remu--get-todos)))

(defun orb-remu--display-todo-links (buffer links)
  (org-compile-prefix-format 'agenda)
  (orb-remu--display-links buffer links 'remu-todo--insert-item))

;;;###autoload
(defun orb-remu-todo-section (overlay)
  (orb-remu-backlink-section
   overlay t
   'remu-todo-section
   'orb-remu--collect-todo
   'orb-remu--display-todo-links))

(provide 'orb-remu)
;;; orb-remu.el ends here
