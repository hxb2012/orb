;;; orb-export.el --- 改善org-export -*- coding: utf-8; lexical-binding: t; -*-

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

;; 最初是从org-roam-export.el里抄来的

;;; Commentary:

;;; Code:

(declare-function org-element-type "org-element" (element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-export-get-reference "ox" (datum info))

(defun orb-export--org-html--reference (datum info &optional named-only)
  "Orb's patch for `org-html--reference' to support ID link export.
See `org-html--reference' for DATUM, INFO and NAMED-ONLY."
  (let* ((type (org-element-type datum))
         (user-label
          (org-element-property
           (pcase type
             ((or `headline `inlinetask) :CUSTOM_ID)
             ((or `radio-target `target) :value)
             (_ :name))
           datum))
         (user-label
          (or user-label
              (when-let ((path (org-element-property :ID datum)))
                ;; see `org-html-link' for why we use "ID-"
                ;; (search for "ID-" in ox-html.el)
                (concat "ID-" path)))))
    (cond
     ((and user-label
           (or (plist-get info :html-prefer-user-labels)
               (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))

(defun orb-export--org-html-template (fun contents info)
  (let ((result (funcall fun contents info))
        (id (when (plist-get info :with-title)
              (save-excursion
                (goto-char (point-min))
                (when (org-before-first-heading-p)
                  (org-id-get))))))
    (if id
        (save-match-data
          (string-match (regexp-quote "<h1 class=\"title\">") result)
          (replace-match (format "<h1 id=\"ID-%s\" class=\"title\">" id) nil nil result))
      result)))

;;;###autoload
(define-minor-mode orb-export-mode
  "Global minor mode to tweak `org-export'"
  :group 'orb
  :global t
  :init-value nil
  (cond
   (orb-export-mode
    (advice-add 'org-html--reference :override 'orb-export--org-html--reference)
    (advice-add 'org-html-template :around 'orb-export--org-html-template))
   (t
    (advice-remove 'org-html-template 'orb-export--org-html-template)
    (advice-remove 'org-html--reference 'orb-export--org-html--reference))))

(provide 'orb-export)
;;; orb-export.el ends here
