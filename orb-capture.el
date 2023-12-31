;;; orb-capture.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

(defvar org-capture-link-is-already-stored)
(defvar org-capture-entry)

(declare-function org-capture-target-buffer "org-capture")
(declare-function org-capture-put-target-region-and-position "org-capture")
(declare-function org-capture-expand-file "org-capture")
(declare-function org-capture-put "org-capture")

(defgroup orb-capture nil
  "Orb-Capture"
  :tag "Orb-Capture"
  :group 'org)

(defcustom orb-capture-targets nil
  ""
  :type '(alist :key-type string :value-type variable)
  :group 'orb-capture)

(defun orb-capture--completing-read (prompt collection)
  (let ((completion-extra-properties
         (list
          :annotation-function
          (lambda (k)
            (format " (%s)" (car (alist-get k collection nil nil 'equal)))))))
    (assoc (completing-read prompt collection) collection)))

(defun orb-capture--set-target-location (prompt target-alist)
  (pcase-let
      ((`(,_key ,_desc ,file . ,outline-path)
        (if (cdr target-alist)
            (orb-capture--completing-read prompt target-alist)
          (car target-alist))))
    (let* ((date
            (when (string-search "%" file)
              (org-read-date nil t nil "Capture Date:")))
          (path (string-trim-right (format-time-string file date))))
      (when date
        (org-capture-put :default-time date))
      (if outline-path
          (let ((m (org-find-olp (cons (org-capture-expand-file path) outline-path))))
            (set-buffer (marker-buffer m))
            (org-capture-put-target-region-and-position)
            (widen)
            (goto-char m)
            (set-marker m nil))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)
        (goto-char (point-min))
        nil))))

(defun orb-capture--get-targets (key)
  (symbol-value (cdr (assoc key orb-capture-targets))))

(defun orb-capture-set-target-location ()
  (orb-capture--set-target-location
   "capture target: "
   (orb-capture--get-targets (car org-capture-entry))))

(defun orb-capture (key template &rest rest)
  (unless (and (boundp 'org-capture-link-is-already-stored)
               org-capture-link-is-already-stored)
    (let ((annotation
           (ignore-errors
             (let ((org-id-link-to-org-use-id t))
               (org-store-link nil)))))
      (plist-put org-store-link-plist :annotation annotation))
    (when (derived-mode-p 'org-mode)
      (plist-put org-store-link-plist
                 :category (org-get-category))))
  (let ((org-capture-link-is-already-stored t)
        (org-capture-entry
         (append
          (list key key 'entry '(function orb-capture-set-target-location) template)
          rest)))
    (org-capture)))

;;;###autoload
(defun orb-capture-collect (kind)
  (goto-char (point-min))
  (let* ((targets nil)
         (file-name (buffer-file-name))
         (file-title (org-get-title))
         (case-fold-search t)
         (key-prop (concat "CAPTURE_" (upcase kind)))
         (file-prop (concat key-prop "_TARGET"))
         (title-prop (concat key-prop "_TITLE"))
         (re (org-re-property key-prop)))
    (while (re-search-forward re nil t)
      (when (org-at-property-p)
        (let ((key (org-entry-get nil key-prop))
              (file
               (when-let
                   ((f (or (org-entry-get nil file-prop)
                           (org-entry-get nil "CAPTURE_TARGET"))))
                 (expand-file-name f)))
              (outline-path (cons file-title
                                  (unless (org-before-first-heading-p)
                                    (org-get-outline-path t))))
              (title (or (org-entry-get nil title-prop)
                         (org-entry-get nil "CAPTURE_TITLE"))))
          (push
           (list
            kind key
            (cons
             (string-join
              (if title (cons title outline-path) outline-path)
              "·")
             (cons (or file file-name)
                   (unless file (cdr outline-path)))))
           targets))))
    targets))

(provide 'orb-capture)
;;; orb-capture.el ends here
