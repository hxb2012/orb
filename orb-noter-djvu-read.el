;;; orb-noter-djvu-read.el --- Noter DJVU  -*- coding: utf-8; lexical-binding: t; -*-

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

;; 最初是从org-noter-djvu.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb-noter)

(declare-function djvu-goto-page "djvu" (&optional arg1 arg2))
(defvar djvu-doc)

(defun orb-noter-djvu-read--insert-note ()
  (when (not (region-active-p))
    (user-error "region not active"))
  (let ((start (region-beginning))
        (end (region-end)))
    (cons
     (list (buffer-local-value 'djvu-doc-page djvu-doc) start end)
     (buffer-substring-no-properties start end))))

(defun orb-noter-djvu-read--goto-location (location)
  (pcase-let ((`(,page ,start ,end) location))
    (unless (equal page (buffer-local-value 'djvu-doc-page djvu-doc))
      (djvu-goto-page page))
    (goto-char start)
    (push-mark end nil t)))

(defun orb-noter-djvu-read--compare-location (comp loc1 loc2)
  (let ((i1 (car loc1))
        (i2 (car loc2)))
    (if (= i1 i2)
        (let ((s1 (cadr loc1))
              (s2 (cadr loc2)))
          (if (= s1 s2)
              (funcall comp (caddr loc1) (caddr loc2))
            (funcall comp s1 s2)))
      (funcall comp i1 i2))))

(defun orb-noter-djvu-read--get-current-view ()
  (list (buffer-local-value 'djvu-doc-page djvu-doc) (window-start) (window-end nil t)))

(defun orb-noter-djvu-read--relative-position-to-view (location view)
  (pcase-let ((`(,il ,sl _) location)
              (`(,iv ,sv ,ev) view))
    (cond
     ((< il iv) 'before)
     ((> il iv) 'after)
     ((< sl sv) 'before)
     ((>= sl ev) 'after)
     (t 'inside))))

(defun orb-noter-djvu-read--make-document-buffer (document name)
  (with-current-buffer (make-indirect-buffer document name t t)
    (setq-local orb-noter--insert-note 'orb-noter-djvu-read--insert-note)
    (setq-local orb-noter--goto-location 'orb-noter-djvu-read--goto-location)
    (setq-local orb-noter--get-current-view 'orb-noter-djvu-read--get-current-view)
    (add-hook 'window-scroll-functions 'orb-noter--window-scroll-h nil t)
    (current-buffer)))

(advice-add 'djvu-init-page :after 'orb-noter--display-page-a)

;;;###autoload
(defun orb-noter:djvu-read-mode ()
  '( :make-document-buffer orb-noter-djvu-read--make-document-buffer
     :relative-position-to-view orb-noter-djvu-read--relative-position-to-view
     :compare-location orb-noter-djvu-read--compare-location))

(provide 'orb-noter-djvu-read)
;;; orb-noter-djvu-read.el ends here
