;;; orb-noter-nov.el --- Noter Nov  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

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

;; 最初是从org-noter-nov.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb-noter)

(declare-function nov-render-document "nov" ())
(defvar nov-documents-index)

(defun orb-noter-nov--insert-note ()
  (when (not (region-active-p))
    (user-error "region not active"))
  (let ((start (region-beginning))
        (end (region-end)))
    (cons
     (list nov-documents-index start end)
     (buffer-substring-no-properties start end))))

(defun orb-noter-nov--goto-location (location)
  (pcase-let ((`(,index ,start ,end) location))
    (unless (equal index nov-documents-index)
      (setq nov-documents-index index)
      (nov-render-document))
    (goto-char start)
    (push-mark end nil t)))

(defun orb-noter-nov--compare-location (comp loc1 loc2)
  (let ((i1 (car loc1))
        (i2 (car loc2)))
    (if (= i1 i2)
        (let ((s1 (cadr loc1))
              (s2 (cadr loc2)))
          (if (= s1 s2)
              (funcall comp (caddr loc1) (caddr loc2))
            (funcall comp s1 s2)))
      (funcall comp i1 i2))))

(defun orb-noter-nov--get-current-view ()
  (list nov-documents-index (window-start) (window-end nil t)))

(defun orb-noter-nov--relative-position-to-view (location view)
  (pcase-let ((`(,il ,sl _) location)
              (`(,iv ,sv ,ev) view))
    (cond
     ((< il iv) 'before)
     ((> il iv) 'after)
     ((< sl sv) 'before)
     ((>= sl ev) 'after)
     (t 'inside))))

(defun orb-noter-nov--make-document-buffer (document name)
  (with-current-buffer (make-indirect-buffer document name nil t)
    (setq-local buffer-file-name (buffer-local-value 'buffer-file-name document))
    (setq-local orb-noter--insert-note 'orb-noter-nov--insert-note)
    (setq-local orb-noter--goto-location 'orb-noter-nov--goto-location)
    (setq-local orb-noter--get-current-view 'orb-noter-nov--get-current-view)
    (funcall (buffer-local-value 'major-mode document))
    (add-hook 'window-scroll-functions 'orb-noter--window-scroll-h nil t)
    (current-buffer)))

(advice-add 'nov-render-document :after 'orb-noter--display-page-a)

;;;###autoload
(defun orb-noter:nov-mode()
  '( :make-document-buffer orb-noter-nov--make-document-buffer
     :relative-position-to-view orb-noter-nov--relative-position-to-view
     :compare-location orb-noter-nov--compare-location))

(provide 'orb-noter-nov)
;;; orb-noter-nov.el ends here
