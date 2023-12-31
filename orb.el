;;; orb.el --- 保存Org节点到SQLite  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>, Michael Albinus <michael.albinus@gmx.de>, Eric Schulte <schulte.eric@gmail.com>, Carsten Dominik <carsten.dominik@gmail.com>, Gonçalo Santos (aka. weirdNox@GitHub), c1-g <char1iegordon@protonmail.com>
;; Maintainer: "洪筱冰" <hxb@localhost.localdomain>
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

;; 最初是从org-roam.el里抄来的

;;; Commentary:

;;; Code:

(require 'org)

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

(defcustom orb-file-exclude-regexp nil
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

(defcustom orb-property-refs "ORB_REFS"
  ""
  :group 'orb
  :type 'string)

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
   (not (and orb-file-exclude-regexp
             (string-match-p
              orb-file-exclude-regexp
              (file-relative-name abs-path orb-directory))))
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

(provide 'orb)
;;; orb.el ends here
