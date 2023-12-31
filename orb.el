;;; orb.el --- 保存Org节点到SQLite  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>, Michael Albinus <michael.albinus@gmx.de>, Eric Schulte, Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: 洪筱冰 <hxb@localhost.localdomain>
;; URL: https://github.com/hxb2012/orb
;; Keywords: Org
;; Package-Requires: ((emacs "29.0") (org "9.6"))
;; Version: 0.0.1

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

;;; Commentary:

;;; 最初是从org-roam.el里抄来的

;;; Code:

(require 'org)
(require 'org-attach)

(defgroup orb nil
  "caching nodes of Org files in SQLite database"
  :group 'org
  :prefix "orb-")

(defcustom orb-directory (expand-file-name org-directory)
  "Default path to Orb files.
Should be absolute path
All Org files, at any level of nesting, are considered."
  :type 'directory
  :group 'orb)

(defcustom orb-file-exclude-regexp (regexp-quote org-attach-id-dir)
  "Directory matching this regular expression are excluded from Orb."
  :type '(choice
          (string :tag "Regular expression matching directories to ignore")
          (const :tag "Include everything" nil))
  :group 'orb)

(defcustom orb-extra-links-elements '(node-property keyword)
  "The list of Org element types to include for parsing by Orb.
By default, when parsing Org's AST, links within keywords and
property drawers are not parsed as links. Sometimes however, it
is desirable to parse and cache these links (e.g. hiding links in
a property drawer)."
  :group 'orb
  :type '(set (const :tag "keywords" keyword)
              (const :tag "property drawers" node-property)))

(defconst orb--file-name-nondirectory-match-regexp "^[^.#]\\(.*[^~]\\)?$\\|^[#].*[^#~]$")

(defconst orb--timestamp-regexp
  (concat "\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[hdwmy]>\\)\\|"
          "\\(<%%\\(([^>\n]+)\\)>\\)\\|"
          org-ts-regexp-both)
  "Regexp matching any timestamp type object.")

;;; Library
(defun orb--file-name-nondirectory-p (filename)
  (and
   (let ((mode (assoc-default filename auto-mode-alist 'string-match-p)))
     (and (symbolp mode)
          (provided-mode-derived-p mode 'org-mode)))
   (string-match-p orb--file-name-nondirectory-match-regexp filename)
   t))

;;;###autoload
(defun orb-file-p (abs-path)
  "Return t if ABS-PATH is an Orb file, nil otherwise.

ABS-PATH is an Orb file if:
- It's located somewhere under `orb-directory'
- It doesn't match excluded regexp (`orb-file-exclude-regexp')"
  (and
   (string-prefix-p
    (file-name-as-directory orb-directory)
    abs-path)
   (not
    (string-match-p
     orb-file-exclude-regexp
     (file-relative-name abs-path orb-directory)))
   (or
    (directory-name-p abs-path)
    (orb--file-name-nondirectory-p (file-name-nondirectory abs-path)))))


;;;###autoload
(defun orb-list-files (&optional abs-path)
  "Return a list of all Orb files under `abs-path' (default
`orb-directory').  See `orb-file-p' for how each file is
determined to be as part of Orb."
  (seq-filter
   'orb-file-p
   (directory-files-recursively
    (or abs-path orb-directory)
    orb--file-name-nondirectory-match-regexp
    nil
    (lambda (x) (orb-file-p (file-name-as-directory x)))
    nil)))

;;;###autoload
(defun orb-buffer-p (&optional buffer)
  "Return t if BUFFER is an Orb file.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (not (buffer-base-buffer))
           (orb-file-p (buffer-file-name))))))

;;;###autoload
(defun orb-buffer-list ()
  "Return a list of buffers that are under `orb-directory'"
  (seq-filter 'orb-buffer-p (buffer-list)))

(defun orb--file-hash (file-path)
  "Compute the hash of FILE-PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'sha256 (current-buffer))))

(defun orb--file-name-levels (file-name)
  (let ((levels nil)
        (start 0))
    (while-let ((next (string-search "/" file-name start)))
      (setq start (1+ next))
      (push start levels))
    levels))

(defun orb--collect-buffer ()
  (let ((file-name (file-relative-name buffer-file-name orb-directory)))
    (list
     file-name
     (when-let ((levels (orb--file-name-levels file-name)))
       (json-encode levels))
     (when-let ((keywords (org-collect-keywords '("TITLE" "CATEGORY" "COLUMNS") '("TITLE" "CATEGORY" "COLUMNS"))))
       (json-encode keywords))
     (when org-file-tags
       (json-encode org-file-tags))
     (json-encode org-todo-keywords-1)
     (json-encode org-done-keywords))))

(defun orb--get-category-pair (pom)
  (org-with-point-at pom
    (let* ((local (org--property-local-values "CATEGORY" nil))
           (value (and local (mapconcat #'identity
                                        (delq nil local)
                                        (org--property-get-separator "CATEGORY")))))
      (when value
        (list (cons "CATEGORY" value))))))

(defun orb--property-timestamp-position (prop element)
  (when-let ((time-stamp (org-element-property prop element)))
    (org-element-property :begin time-stamp)))

(defun orb--map-nodes (fun)
  "Call FUN for every nodes in current buffer."
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (let ((pom (point)))
      (funcall
       fun
       pom
       (org-outline-level)
       nil
       nil
       nil
       nil
       (append
        (orb--get-category-pair pom)
        (assoc-delete-all "CATEGORY" (org-entry-properties pom 'standard)))
       nil
       nil
       nil
       nil
       (when (org-is-habit-p pom) (org-habit-parse-todo pom)))))

  (org-map-region
   (lambda ()
     (let* ((pom (point))
            (pos (line-beginning-position))
            (element (org-element-at-point pom))
            (heading
             (replace-regexp-in-string
              "^\\[[0-9]+/[0-9]+\\] *\\|^\\[%[0-9]+\\] *" ""
              (org-get-heading t t t t)))
            (text (org-get-heading t t nil nil)))
       (funcall
        fun
        pos
        (org-outline-level)
        (unless (equal heading "COMMENT")
          heading)
        (if (equal heading "COMMENT")
            "COMMENT"
          (unless (equal heading text)
            text))
        (org-element-property :todo-keyword element)
        (org-get-tags element t)
        (append
         (orb--get-category-pair pom)
         (assoc-delete-all "CATEGORY" (org-entry-properties pom 'standard)))
        (orb--property-timestamp-position :scheduled element)
        (orb--property-timestamp-position :deadline element)
        (orb--property-timestamp-position :closed element)
        (when (org-entry-blocked-p) 1)
        (when (org-is-habit-p pom) (org-habit-parse-todo pom)))))
   (point-min) (point-max)))


(defun orb--map-links (fun)
  "Call FUN for every links in current buffer."
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil :no-error)
    (goto-char (match-beginning 0))
    (save-match-data
      (let* ((element (org-element-context))
             (type (org-element-type element))
             (prop
              (unless (eq type 'link)
                (org-element-property :key element)))
             (link
              (cond
               ((eq type 'link)
                element)
               ;; Links in property drawers and lines starting with #+. Recall that, as for Org Mode v9.4.4, the
               ;; org-element-type of links within properties drawers is "node-property" and for lines starting with
               ;; #+ is "keyword".
               ((member type orb-extra-links-elements)
                (org-element-link-parser)))))
        (when link
          (let ((link-type (org-element-property :type link))
                (path (org-element-property :path link)))
            (funcall
             fun
             (org-element-property :begin link)
             (org-element-property :end link)
             link-type
             path
             (org-element-property :search-option link)
             (org-element-property :application link)
             (when (and (equal link-type "file")
                        (not (file-name-absolute-p path)))
               (seq-position (file-name-split path) ".."
                             (lambda (x y) (not (equal x y)))))
             prop)))))
    (goto-char (match-end 0))))

(defun orb--map-blocks (fun)
  "Call FUN for every date-range in current buffer."
  (goto-char (point-min))
  (while (re-search-forward org-tr-regexp-both nil t)
    (catch :skip
      (when (org-in-src-block-p)
        (throw :skip t))
      (let ((start-time (buffer-substring-no-properties (1- (match-beginning 1)) (1+ (match-end 1))))
            (end-time (buffer-substring-no-properties (1- (match-beginning 2)) (1+ (match-end 2)))))
        (save-match-data
          (condition-case _err
              (org-time-string-to-time start-time)
            (error
             (throw :skip t)))
          (condition-case _err
              (org-time-string-to-time end-time)
            (error
             (throw :skip t))))
        (funcall
         fun
         (match-beginning 0)
         (match-end 0)
         start-time
         (save-match-data
           (float-time (org-time-string-to-time start-time)))
         nil
         end-time
         (save-match-data
           (float-time (org-time-string-to-time end-time))))))))

(defun orb--map-timestamps (fun)
  "Call FUN for every timestamp in current buffer."
  (goto-char (point-min))
  (while (re-search-forward orb--timestamp-regexp nil t)
    ;; Skip date ranges, scheduled and deadlines, which are handled
    ;; specially.  Also skip time-stamps before first headline as
    ;; there would be no entry to add to the agenda.  Eventually,
    ;; ignore clock entries.
    (catch :skip
      (save-match-data
        (when (or (org-at-date-range-p)
                  (org-at-clock-log-p)
                  (not (org-at-timestamp-p 'agenda)))
          (throw :skip nil)))
      (let ((start-time (match-string 0))
            (repeat (match-string 1))
            (sexp-entry (match-string 2)))
        (if (or repeat sexp-entry)
            (funcall
             fun
             (match-beginning 0)
             (match-end 0)
             start-time
             nil)
          (save-match-data
            (condition-case _err
                (org-time-string-to-time start-time)
              (error
               (throw :skip t))))
          (funcall
           fun
           (match-beginning 0)
           (match-end 0)
           start-time
           (save-match-data
             (float-time (org-time-string-to-time start-time)))
           (when (and (string-prefix-p "[" (match-string 0))
                      (org-at-date-range-p t))
             1)))))))

(defun orb--map-sexps (fun)
  "Call FUN for every sexp in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^&?%%(" nil t)
    (catch :skip
      (when (org-in-src-block-p)
        (throw :skip t))
      (let ((beg (match-beginning 0))
            (b (1- (match-end 0))))
        (goto-char b)
        (forward-sexp 1)
        (let ((sexp (buffer-substring b (point)))
              (sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
                             (buffer-substring
                              (match-beginning 1)
                              (save-excursion
                                (goto-char (match-end 1))
                                (skip-chars-backward "[:blank:]")
                                (point)))
                           "")))
          (funcall fun beg sexp sexp-entry))))))


(defun orb--map-progress (fun)
  "Call FUN for every progress in current buffer"
  (let* ((parts
          (list
           (concat "\\<" org-closed-string)
           (concat "\\<" org-clock-string)
           (format "- +State \"%s\".*?" org-todo-regexp)))
         (parts-re (mapconcat #'identity parts "\\|"))
         (regexp (concat
                  "\\(" parts-re "\\)"
                  " *"
                  org-ts-regexp-inactive)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (unless (org-in-src-block-p)
        (let* ((pom (match-beginning 0))
               (timestr (match-string 0))
               (ts (1- (match-beginning 3)))
               (closedp (string-prefix-p org-closed-string timestr))
               (statep (string-prefix-p "-" timestr))
               (clockp (not (or closedp statep)))
               (rest (buffer-substring (match-end 0) (line-end-position)))
               (extra
                (save-excursion
                  (cond
                   (statep
                    (and (looking-at ".*\\\\\n[ \t]*\\([^-\n \t].*?\\)[ \t]*$")
                         (match-string 1)))
                   (clockp
                    (and (looking-at ".*\n[ \t]*-[ \t]+\\([^-\n \t].*?\\)[ \t]*$")
                         (match-string 1)))))))
          (funcall
           fun
           pom
           (if (not clockp)
               timestr
             (if (string-match "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\].*?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
                               rest)
                 (concat (substring timestr 0 -1)
                         "-" (match-string 1 rest) "]"
                         (match-string 2 rest))
               (concat timestr "-")))
           ts
           extra))))))

(provide 'orb)
;;; orb.el ends here
