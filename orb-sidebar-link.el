;;; orb-sidebar-link.el --- 链接  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;; 最初是从org-roam-mode.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb)

(defvar orb-sidebar--current-buffer)

(declare-function org-element-link-parser "org-element" ())
(declare-function org-element-property "org-element" (property element))

(defun orb-sidebar-link--trim-link (link)
  (let* ((type (org-element-property :type link))
         (props (list
                 :type type
                 :path (org-element-property :path link))))
    (when (equal type "file")
      (plist-put props :application (org-element-property :application link))
      (plist-put props :search-option (org-element-property :search-option link)))
    (list 'link props)))

(defun orb-sidebar-link--parse-link ()
  (with-current-buffer orb-sidebar--current-buffer
    (when-let ((face (get-text-property (point) 'face)))
      (when (or (eq 'org-link face)
                (and (listp face) (memq 'org-link face)))
        (if-let ((range (org-in-regexp org-link-any-re)))
            (save-excursion
              (goto-char (car range))
              (cons range (orb-sidebar-link--trim-link (org-element-link-parser))))
          (when (eq (get-text-property (point) 'org-linked-text) t)
            (let ((start (previous-single-property-change (1+ (point)) 'org-linked-text))
                  (end (next-single-property-change (point) 'org-linked-text)))
              (cons
               (cons start end)
               (list
                'link
                (list
                 :type "radio"
                 :path (buffer-substring-no-properties start end)))))))))))

(defun orb-sidebar-link--normalize-search (s)
  (replace-regexp-in-string "\n[ \t]*" " " s))

(defun orb-sidebar-link--find-refs-file (_type _path)
  (current-buffer))

(defun orb-sidebar-link--find-link-file (link)
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (or
     (pcase type
       ("radio"
        (list (current-buffer) type path))
       ("custom-id"
        (list (current-buffer) 'search (orb-sidebar-link--normalize-search (concat "#" path))))
       ("coderef"
        (list (current-buffer) 'search (orb-sidebar-link--normalize-search (format "(%s)" path))))
       ("fuzzy"
        (list (current-buffer) 'search (orb-sidebar-link--normalize-search path)))
       ("id"
        (let ((file (org-id-find-id-file path)))
          (if-let ((buffer (org-find-base-buffer-visiting file)))
              (list buffer 'id path)
            (when (file-exists-p file)
              (list file 'id path)))))
       ((and "file"
             (guard (not (equal "sys" (org-element-property :application link))))
             (let `,option (org-element-property :search-option link))
             (guard option)
             (let `,search-option
               (if (string-match-p "\\`[0-9]+\\'" option)
                   (list 'line (string-to-number option))
                 (list 'search (orb-sidebar-link--normalize-search option)))))
        (cond
         ((string-match-p "[*?{]" (file-name-nondirectory path)) nil)
         ((equal path "") (cons (current-buffer) search-option))
         (t
          (when-let*
              ((file
                (pcase (substitute-in-file-name (expand-file-name path))
                  ((pred file-remote-p) nil)
                  ((and (pred file-directory-p) file)
                   (when org-open-directory-means-index-dot-org
                     (concat (file-name-as-directory file) "index.org")))
                  (file
                   file)))
               (mode (assoc-default file auto-mode-alist 'string-match-p)))
            (when (and (symbolp mode)
                       (provided-mode-derived-p mode 'org-mode))
              (if-let ((buffer (org-find-base-buffer-visiting file)))
                  (cons buffer search-option)
                (when (file-exists-p file)
                  (cons file search-option)))))))))
     (list (orb-sidebar-link--find-refs-file type path)
           'refs (format "%s:%s" type path)))))

(defun orb-sidebar-link--insert-file (output-buffer)
  (let* ((file-name (buffer-file-name))
         (title (or (org-get-title) (file-name-base file-name)))
         (s (concat
             (propertize title 'face 'org-document-title)
             " (" (file-name-nondirectory file-name) ")"))
         (overlay
          (with-current-buffer output-buffer
            (goto-char (point-max))
            (orb-sidebar-section-make
             :name (propertize
                    s
                    'help-echo
                    (with-current-buffer orb-sidebar--current-buffer
                      (file-relative-name file-name)))
             :open t))))
    (overlay-put overlay 'orb-file file-name)
    overlay))

(defun orb-sidebar-link--get-heading-overlay (pos)
  (let ((overlay (get-text-property pos 'orb-section)))
    (if (overlayp overlay)
        overlay
      (goto-char pos)
      (orb-sidebar-section-make :open t))))

(defun orb-sidebar-link--insert-section (pos s)
  (goto-char (point-max))
  (let ((start (point)))
    (insert (propertize s 'orb-section t 'orb-pos pos))
    (insert "\n")
    start))

(defun orb-sidebar-link--insert (file-overlay heading paragraph pos s)
  (with-current-buffer (overlay-buffer file-overlay)
    (orb-sidebar-section--with-restriction
        (if heading (orb-sidebar-link--get-heading-overlay heading) file-overlay)
      (when (and paragraph
                 (eq (point-min) (point-max)))
        (goto-char (point-max))
        (insert "\n"))
      (orb-sidebar-link--insert-section pos s))))

(defun orb-sidebar-link--copy-paragraph (file-overlay &optional heading)
  (let ((pos (point)))
    (orb-sidebar-link--insert
     file-overlay heading
     (if heading
         (>= pos
             (save-excursion
               (org-end-of-meta-data t)
               (point)))
       (not (or (org-at-keyword-p) (org-at-property-drawer-p))))
     pos
     (concat
      (buffer-substring
       pos
       (save-excursion
         (org-forward-paragraph)
         (skip-chars-backward " \t\n")
         (point)))
      "\n"))))

(defun orb-sidebar-link--copy-heading (file-overlay &optional parent)
  (orb-sidebar-link--insert
   file-overlay parent nil (point)
   (buffer-substring
    (point)
    (save-excursion
      (org-end-of-line)
      (point)))))

(defun orb-sidebar-link--get-heading (file-overlay &optional last-org-pos last-output-pos)
  (let ((pos (point)))
    (cond
     ((eq pos last-org-pos) last-output-pos)
     ((eq pos (point-min)) nil)
     ((and last-org-pos (< pos last-org-pos))
      (with-current-buffer (overlay-buffer file-overlay)
        (goto-char last-output-pos)
        (while (let ((match (text-property-search-backward 'orb-pos)))
                 (not (eq (prop-match-value match) pos))))
        (point)))
     (t
      (orb-sidebar-link--copy-heading
       file-overlay
       (save-excursion
         (when (org-up-heading-safe)
           (orb-sidebar-link--get-heading file-overlay last-org-pos last-output-pos))))))))

(defun orb-sidebar-link--highlight-match (buffer last-pos start end)
  (let* ((last-file-overlay (when last-pos (car last-pos)))
         (last-file-name
          (when last-file-overlay
            (overlay-get last-file-overlay 'orb-file)))
         (file-changed (not (equal last-file-name buffer-file-name)))
         (file-overlay
          (if file-changed
              (orb-sidebar-link--insert-file buffer)
            last-file-overlay))
         (last-org-pos (unless file-changed (cadr last-pos)))
         (last-output-pos (unless file-changed (cddr last-pos)))
         (org-start
          (save-excursion
            (org-backward-paragraph)
            (skip-chars-forward " \t\n")
            (point)))
         (output-start
          (cond
           ((eq org-start last-org-pos)
            last-output-pos)
           ((org-before-first-heading-p)
            (goto-char org-start)
            (orb-sidebar-link--copy-paragraph file-overlay))
           ((eq org-start (save-excursion (org-back-to-heading) (point)))
            (goto-char org-start)
            (orb-sidebar-link--get-heading file-overlay last-org-pos last-output-pos))
           (t
            (goto-char org-start)
            (orb-sidebar-link--copy-paragraph
             file-overlay
             (save-excursion
               (org-back-to-heading)
               (orb-sidebar-link--get-heading file-overlay last-org-pos last-output-pos))))))
         (offset (- output-start org-start))
         (overlay
          (with-current-buffer buffer
            (let ((beg (+ start offset))
                  (end (+ end offset)))
              (put-text-property beg end 'orb-item start)
              (make-overlay beg end)))))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'match)
    (overlay-put overlay 'orb-pos start)
    (cons file-overlay (cons org-start output-start))))

(defun orb-sidebar-link--search-radio-target (path)
  (ignore-errors
    (cl-letf (((symbol-function 'org-fold-show-context) #'identity))
      (org-link--search-radio-target path)
      t)))

(defun orb-sidebar-link--search (s avoid-pos)
  (ignore-errors
    (cl-letf* ((org-link-search-must-match-exact-headline nil)
               (split-string (symbol-function 'split-string))
               ((symbol-function 'split-string)
                (lambda (string &optional separators omit-nulls trim)
                  (save-match-data
                    (funcall split-string string separators omit-nulls trim)))))
      (org-link-search s avoid-pos t)
      t)))

(defun orb-sidebar-link--display-target (output-buffer range type path &optional same-buffer)
  (org-with-wide-buffer
   (cond
    ((equal type "radio")
     (when (orb-sidebar-link--search-radio-target path)
       (orb-sidebar-link--highlight-match output-buffer nil (match-beginning 0) (match-end 0))))
    ((eq type 'line)
     (goto-char (point-min))
     (forward-line (1- path))
     (orb-sidebar-link--highlight-match output-buffer nil (point) (line-end-position)))
    ((eq type 'id)
     (org-find-property "ID" path)
     (goto-char (match-beginning 0))
     (orb-sidebar-link--highlight-match output-buffer nil (point) (match-end 0)))
    ((eq type 'refs)
     (let ((regexp (org-re-property orb-property-refs nil nil path))
           (last-pos nil))
       (goto-char (point-min))
       (while (let ((case-fold-search t))
                (re-search-forward regexp nil t))
         (unless (and same-buffer (eq (match-end 0) (cdr range)))
           (setq last-pos
                 (save-excursion
                   (orb-sidebar-link--highlight-match
                    output-buffer last-pos (match-beginning 0) (match-end 0))))))))
    ((string-match "\\`/\\(.*\\)/\\'" path)
     (let* ((regexp (match-string 1 path))
            (orb-sidebar-case-fold-search
             (if (eq org-occur-case-fold-search 'smart)
                 (isearch-no-upper-case-p regexp t)
               org-occur-case-fold-search))
            (last-pos nil))
       (goto-char (point-min))
       (while
           (let ((case-fold-search orb-sidebar-case-fold-search))
             (re-search-forward regexp nil t))
         (setq last-pos
               (save-excursion
                 (orb-sidebar-link--highlight-match
                  output-buffer last-pos (match-beginning 0) (match-end 0)))))))
    ((string-match-p "\\`(\\(.*\\))\\'" path)
     (when (orb-sidebar-link--search path (when same-buffer (+ 2 (car range))))
       (let ((start (match-beginning 2))
             (end (match-end 2)))
         (goto-char end)
         (orb-sidebar-link--highlight-match output-buffer nil start end))))
    ((orb-sidebar-link--search path (when same-buffer (+ 2 (car range))))
     (goto-char (match-beginning 0))
     (orb-sidebar-link--highlight-match output-buffer nil (point) (match-end 0))))))

(defun orb-sidebar-link--init-temp-buffer (file)
  (insert-file-contents file)
  (org-mode)
  (font-lock-ensure))

;;;###autoload
(defun orb-sidebar-link-target-section (overlay)
  (pcase (orb-sidebar-link--parse-link)
    (`(,range . ,link)
     (pcase-let*
         ((output-buffer (current-buffer))
          (`(,file ,type ,path)
           (with-current-buffer orb-sidebar--current-buffer
             (orb-sidebar-link--find-link-file link)))
          (tick
           (cons file
                 (cond
                  ((bufferp file)
                   (buffer-chars-modified-tick file))
                  ((stringp file)
                   (file-attribute-modification-time (file-attributes file)))))))
       (unless (and
                (equal link (overlay-get overlay 'orb-link))
                (equal tick (overlay-get overlay 'orb-tick)))
         (delete-region (point-min) (point-max))
         (overlay-put overlay 'orb-link link)
         (overlay-put overlay 'orb-tick tick)
         (cond
          ((bufferp file)
           (let ((same-buffer (eq file orb-sidebar--current-buffer)))
             (with-current-buffer file
               (orb-sidebar-link--display-target output-buffer range type path same-buffer))))
          ((stringp file)
           (with-temp-buffer
             (let ((buffer-file-name file)
                   (default-directory (file-name-directory file)))
               (orb-sidebar-link--init-temp-buffer file)
               (orb-sidebar-link--display-target output-buffer range type path))))
          (t
           (insert (format "%s" link)))))))
    ((guard (overlay-get overlay 'orb-link))
     (delete-region (point-min) (point-max))
     (overlay-put overlay 'orb-link nil)
     (overlay-put overlay 'orb-tick nil))))

(defun orb-sidebar-link--resolve-local-link (range link)
  (if (equal "id" (org-element-property :type link))
      (cons 'id (org-element-property :path link))
    (pcase-let ((`(,file ,type ,path) (orb-sidebar-link--find-link-file link)))
      (when (equal file (current-buffer))
        (cond
         ((eq type 'refs)
          (cons 'refs path))
         ((eq type 'line)
          (goto-char (point-min))
          (forward-line (1- path))
          (cons 'match (cons (point) (line-end-position))))
         ((eq (string-to-char path) ?#)
          (cons 'custom-id (substring path 1)))
         ((string-match "\\`/\\(.*\\)/\\'"
                        (replace-regexp-in-string "\n[ \t]*" " " path))
          (cons 'regexp (match-string 1 path)))
         ((orb-sidebar-link--search path (+ 2 (car range)))
          (cons 'match (cons (match-beginning 0) (match-end 0)))))))))

(defun orb-sidebar-link--collect-links (table &optional skip-radio data)
  (let ((matches nil)
        (regexps nil))
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (let* ((range (list (point) (match-end 0)))
               (value (or data range)))
          (pcase (org-with-wide-buffer
                  (orb-sidebar-link--resolve-local-link
                   range (orb-sidebar-link--trim-link (org-element-link-parser))))
            (`(match . ,match)
             (push (cons match value) matches))
            (`(regexp . ,regexp)
             (push (cons value regexp) regexps))
            ((and `(,_ . ,_) `,key)
             (puthash key (cons value (gethash key table)) table))))))
    (unless skip-radio
      (let ((targets (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while-let ((match (text-property-search-forward 'org-linked-text)))
          (let* ((start (prop-match-beginning match))
                 (end (prop-match-end match))
                 (path (buffer-substring-no-properties start end))
                 (data (or data (list start end)))
                 (face (get-text-property start 'face)))
            (when (and
                   (or (eq 'org-link face)
                       (and (listp face) (memq 'org-link face))))
              (pcase (gethash path targets)
                ((and `(,_ . ,_) `,match)
                 (push (cons match data) matches))
                (`nil
                 (puthash
                  path
                  (if (save-excursion
                        (org-with-wide-buffer (orb-sidebar-link--search-radio-target path)))
                      (let ((match (cons (match-beginning 0) (match-end 0))))
                        (push (cons match data) matches)
                        match)
                    :error)
                  targets))))))))
    (cons matches regexps)))

(defun orb-sidebar-link--collect (&optional skip-radio)
  (let ((table (make-hash-table :test 'equal)))
    (org-with-wide-buffer
     (pcase-let ((`(,matches . ,regexps) (orb-sidebar-link--collect-links table skip-radio)))
       (list matches regexps table)))))

(defun orb-sidebar-link--get-matches (links headline)
  (pcase-let*
      ((`(,matches ,regexps ,table) links)
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
              (gethash (cons 'id id) table))
            (when-let ((id (org-entry-get pos "CUSTOM_ID")))
              (gethash (cons 'custom-id id) table))
            (when-let ((id (org-entry-get pos orb-property-refs)))
              (gethash (cons 'refs id) table)))))))
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

(defun orb-sidebar-link--display-links (buffer links &optional highlight)
  (let ((last-pos nil))
    (dolist (entry (seq-sort-by 'car '< links))
      (goto-char (car entry))
      (setq last-pos
            (apply
             (or highlight 'orb-sidebar-link--highlight-match)
             buffer last-pos entry)))))

(defun orb-sidebar-link--insert-outline-path (output-buffer)
  (unless (org-before-first-heading-p)
    (let ((outline-path (org-format-outline-path (org-get-outline-path t))))
      (with-current-buffer output-buffer
        (insert outline-path)
        (insert "\n")))))

;;;###autoload
(defun orb-sidebar-link-back-section (overlay &optional headline collect display)
  (let* ((output-buffer (current-buffer))
         (tick (buffer-chars-modified-tick orb-sidebar--current-buffer))
         (modified (not (eq tick (overlay-get overlay 'orb-last-tick))))
         (links
          (if modified
              (with-current-buffer orb-sidebar--current-buffer
                (org-with-wide-buffer
                 (if collect
                     (funcall collect)
                   (orb-sidebar-link--collect headline))))
            (overlay-get overlay 'orb-links)))
         (pos
          (with-current-buffer orb-sidebar--current-buffer
            (org-with-wide-buffer
             (when headline
               (org-back-to-heading-or-point-min))
             (point))))
         (old-pos (overlay-get overlay 'orb-last-pos))
         (moved (not (eq pos old-pos))))
    (when modified
      (overlay-put overlay 'orb-last-tick tick)
      (overlay-put overlay 'orb-links links))
    (when moved
      (overlay-put overlay 'orb-last-pos pos))
    (when (or modified moved)
      (delete-region (point-min) (point-max))
      (with-current-buffer orb-sidebar--current-buffer
        (org-with-wide-buffer
         (goto-char pos)
         (when headline
           (orb-sidebar-link--insert-outline-path output-buffer))
         (funcall (or display 'orb-sidebar-link--display-links)
                  output-buffer
                  (orb-sidebar-link--get-matches
                   links
                   (when headline
                     (if (org-at-heading-p) (line-end-position) t)))))))))

;;;###autoload
(defun orb-sidebar-link-back-headline-section (overlay)
  (orb-sidebar-link-back-section overlay t))

(provide 'orb-sidebar-link)
;;; orb-sidebar-link.el ends here
