;;; orb-capture-noter.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

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

;; 最初是从org-capture.el里抄来的

;;; Commentary:

(require 'orb-capture)
(require 'orb-noter)

(defvar org-capture-link-is-already-stored)

(defcustom orb-capture-noter-target-alist nil
  "orb capture noter"
  :type '(alist :key-type string :value-type sexp)
  :group 'orb-capture)

(add-to-list 'orb-capture-targets '("noter" . orb-capture-noter-target-alist))

(defun orb-capture-noter--goto (path)
  (catch 'found
    (dolist (file (seq-uniq (mapcar 'caddr (orb-capture--get-targets "noter"))))
      (let* ((buffer (or (org-find-base-buffer-visiting file)
                         (find-file-noselect file)))
             (rel (file-relative-name path (file-name-parent-directory file)))
             (marker
              (with-current-buffer buffer
                (org-with-wide-buffer
                 (when-let ((pos
                             (or (org-find-property
                                  orb-noter-property-doc (format "[[file:%s]]" path))
                                 (org-find-property
                                  orb-noter-property-doc (format "[[file:%s]]" rel)))))
                   (copy-marker pos))))))
        (when marker
          (org-goto-marker-or-bmk marker)
          (throw 'found t))))))

;;;###autoload
(defun orb-capture-noter ()
  (unless (orb-capture-noter--goto buffer-file-name)
    (pcase-let ((`(,title ,props ,initial) (orb-extract))
                (org-capture-link-is-already-stored t))
      (setq org-store-link-plist
            (list :link (concat "file:" buffer-file-name)
                  :description title
                  :annotation title
                  :initial initial
                  :props (string-join
                          (mapcar (lambda (x) (format ":%s: %s\n" (car x) (cdr x)))
                                  props)
                          "")))
      (orb-capture
       "noter"
       (concat
        "* %?%:description\n:PROPERTIES:\n:CAPTURED: %u\n:"
        orb-noter-property-doc
        ": [[%:link]]\n%:props:END:\n\n%i\n")
       :immediate-finish t
       :jump-to-captured t
       :empty-lines 1))))

(provide 'orb-capture-noter)
;;; orb-capture-noter.el ends here
