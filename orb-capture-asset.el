;;; orb-capture-asset.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Code:
(require 'orb-capture)
(require 'orb-asset)

(defvar org-capture-link-is-already-stored)

(defcustom orb-capture-asset-target-alist nil
  "orb capture asset"
  :type '(alist :key-type string :value-type sexp)
  :group 'orb-capture)

(add-to-list 'orb-capture-targets '("asset" . orb-capture-asset-target-alist))

(defcustom orb-capture-asset-copy-to-default nil
  ""
  :type 'boolean
  :group 'orb-capture)

(defcustom orb-capture-asset-delete-after-capture 'ask
  ""
  :type '(choice
          (const :tag "Always delete origin file" t)
          (const :tag "Ask" ask)
          (const :tag "Do no delete origin file" nil))
  :group 'orb-capture)

(defun orb-capture-asset--make-copies (src digest)
  (let* ((path (orb-asset--path digest (file-name-extension src t)))
         (device (file-attribute-device-number (file-attributes src)))
         (directories
          (seq-filter (lambda (pair) (file-directory-p (cdr pair)))
                      orb-asset-external-directories))
         (crm-separator "|")
         (initial
          (string-join
           (mapcar 'car
                   (seq-filter
                    (lambda (pair)
                      (let* ((dir (cdr pair))
                             (dst (file-name-concat dir path)))
                        (or (orb-asset--validate dst digest)
                            (equal device (file-attribute-device-number
                                           (file-attributes dir))))))
                    directories))
           crm-separator))
         (completion-extra-properties
          (list
           :annotation-function
           (lambda (k) (format " (%s)" (alist-get k directories "" nil 'equal)))))
         (copies (when directories
                   (seq-uniq
                    (completing-read-multiple
                     "External directories: " directories nil t initial)))))
    (dolist (key copies)
      (let* ((dir (alist-get key directories nil nil 'equal))
             (dst (file-name-concat dir path)))
        (unless (file-directory-p dir)
          (user-error "Directory not exist: %S" dir))
        (make-directory (file-name-directory dst) t)
        (unless (orb-asset--validate dst digest)
          (orb-asset--copy src orb-asset-directory path)
          (unless (orb-asset--validate dst digest)
            (user-error "Error when copy %S to %S" src dst)))))
    (let ((dst (file-name-concat orb-asset-directory path)))
      (make-directory (file-name-directory dst) t)
      (unless (orb-asset--validate dst digest)
        (if (and copies (not orb-capture-asset-copy-to-default))
            (let* ((key (car copies))
                   (dir (alist-get key directories nil nil 'equal))
                   (src (file-name-concat dir path)))
              (make-symbolic-link src dst))
          (orb-asset--copy src orb-asset-directory path)
          (unless (orb-asset--validate dst digest)
            (user-error "Error when copy %S to %S" src dst)))))
    (when copies
      (concat crm-separator (string-join copies crm-separator) crm-separator))))

(defun orb-capture-asset--dired ()
  (let* ((path (dired-get-filename))
         (filename (file-name-nondirectory path))
         (title (file-name-sans-extension filename)))
    (list path title (cons "FILENAME" filename))))

(defun orb-capture-asset--extract ()
  (if (derived-mode-p 'dired-mode)
      (let* ((path (dired-get-filename))
             (filename (file-name-nondirectory path))
             (title (file-name-sans-extension filename)))
        (list title (cons "FILENAME" filename)))
    (pcase-let ((`(,title . ,extra) (orb-extract))
                (filename (file-name-nondirectory buffer-file-name)))
      (append (list title (cons "FILENAME" filename))
              extra))))

;;;###autoload
(defun orb-capture-asset ()
  (interactive)
  (let ((path (if (derived-mode-p 'dired-mode) (dired-get-filename) buffer-file-name)))
    (if-let ((id (orb-asset--get-id path)))
        (org-id-goto id)
      (if-let ((digest (orb-asset--digest path)))
          (let* ((id (orb-asset--id digest (file-name-extension path t))))
            (if (org-id-find id)
                (org-id-goto id)
              (pcase-let ((`(,title ,props ,initial) (orb-capture-asset--extract))
                          (copies (orb-capture-asset--make-copies path digest))
                          (org-overriding-default-time
                           (file-attribute-modification-time (file-attributes path)))
                          (org-capture-link-is-already-stored t))
                (setq org-store-link-plist
                      (list :link id
                            :description title
                            :annotation title
                            :initial initial
                            :props (string-join
                                    (mapcar
                                     (lambda (x) (format ":%s: %s" (car x) (cdr x)))
                                     (if copies
                                         (cons (cons "COPIES" copies) props)
                                       props))
                                    "\n")))
                (when (orb-capture
                       "asset"
                       "* %?%:description\n:PROPERTIES:\n:CAPTURED: %u\n:ID: %:link\n%:props\n:END:\n\n%i\n"
                       :immediate-finish t
                       :jump-to-captured t
                       :empty-lines 1)
                  (when (or (eq orb-capture-asset-delete-after-capture t)
                            (and orb-capture-asset-delete-after-capture
                                 (y-or-n-p (format "Delete file %S" path))))
                    (delete-file path))))))
        (user-error "failed to open %S" path)))))

(provide 'orb-capture-asset)
;;; orb-capture-asset.el ends here
