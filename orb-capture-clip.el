;;; orb-capture-clip.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

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
(require 'orb)
(require 'orb-capture)

(defvar org-capture-entry)
(defvar org-protocol-reverse-list-of-files)
;;;###autoload(autoload 'org--protocol-detect-protocol-server "org-protocol")

(declare-function html2org "html2org" (start end &optional replace url))

(defcustom orb-capture-clip-target-alist nil
  "orb capture clip"
  :type '(alist :key-type string :value-type sexp)
  :group 'orb-capture)

(add-to-list 'orb-capture-targets '("clip" . orb-capture-clip-target-alist))

;;;###autoload
(defun orb-capture-clip--load-protocol (proc files &rest _rest)
  (let ((dir (process-get proc 'server-client-directory)))
    (when (seq-some
           (lambda (f)
             (string-prefix-p "org-protocol:/"
                              (file-relative-name (car f) dir)))
           files)
      (advice-remove 'server-execute 'orb-capture-clip--load-protocol)
      (require 'org-protocol))))

(defun orb-capture-clip--protocol (fun info)
  (require 'org-capture)
  (cl-letf* ((org-capture (symbol-function 'org-capture))
             ((symbol-function 'org-capture)
              (lambda (_goto _keys)
                (let ((org-capture-entry
                       (list
                        "clip" "clip"
                        'entry '(function orb-capture-set-target-location)
                        (concat "* %:description
:PROPERTIES:
:"                              orb-property-refs
                                ": %:link
:CAPTURED: %u
:ID: %(org-id-new)
:END:

%?
%i")
                        :hook 'orb-capture-clip--hook
                        :jump-to-capture nil
                        :empty-lines 1)))
                  (funcall org-capture nil nil)))))
    (funcall fun info)))

(advice-add 'org-protocol-capture :around 'orb-capture-clip--protocol)

;;;###autoload
(defun orb-capture-clip--detect-protocol ()
  (setq command-line-args-left
        (org--protocol-detect-protocol-server
         (lambda (files _client)
           (mapcar 'car
                   (if org-protocol-reverse-list-of-files
                       (reverse files)
                     files)))
         (mapcar 'list command-line-args-left)
         nil)))

(defun orb-capture-clip--hook ()
  (html2org (point) (point-max) t (org-entry-get nil orb-property-refs)))

(provide 'orb-capture-clip)
;;; orb-capture-clip.el ends here
