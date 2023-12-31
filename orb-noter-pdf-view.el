;;; orb-noter-pdf-view.el --- Noter PDFTools  -*- coding: utf-8; lexical-binding: t; -*-

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

;; 最初是从org-noter-pdf.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb-noter)
(require 'orb-noter-doc-view)

(declare-function image-mode-window-get "image" (prop &optional winprops))
(declare-function pdf-view-display-region "pdf-view" (&optional region rectangle-p selection-style))
(declare-function pdf-view--push-mark "pdf-view" ())
(declare-function pdf-view-goto-page "pdf-view" (page &optional window))
(declare-function pdf-view-active-region "pdf-view" (&optional deactivate-p))
(declare-function pdf-view-active-region-p "pdf-view" ())
(declare-function pdf-view-active-region-text "pdf-view" ())

(defvar pdf-view-active-region)

(defun orb-noter-pdf-view--insert-note ()
  (when (not (pdf-view-active-region-p))
    (user-error "region not active"))
  (let ((region (pdf-view-active-region)))
    (when (length> region 1)
      (user-error "more than one rectangle selected"))
    (cons (cons (image-mode-window-get 'page)
                (car (pdf-view-active-region)))
          (string-join (pdf-view-active-region-text) " "))))

(defun orb-noter-pdf-view--goto-location (location)
  (pdf-view-goto-page (car location))
  (setq pdf-view-active-region (list (cdr location)))
  (pdf-view--push-mark)
  (pdf-view-display-region))

(defun orb-noter-pdf-view--compare-location (comp loc1 loc2)
  (pcase-let ((`(,p1 ,l1 ,t1 ,r1 ,b1) loc1)
              (`(,p2 ,l2 ,t2 ,r2 ,b2) loc2))
    (if (= p1 p2)
        (if (= t1 t2)
            (if (= l1 l2)
                (if (= b1 b2)
                    (funcall comp r1 r2)
                  (funcall comp b1 b2))
              (funcall comp l1 l2))
          (funcall comp t1 t2))
      (funcall comp p1 p2))))

(defun orb-noter-pdf-view--relative-position-to-view (location view)
  (pcase-let ((`(,pl ,ll ,tl _ _) location)
              (`(,page ,left ,top ,width ,height) view))
    (cond
     ((< pl page) 'before)
     ((> pl page) 'after)
     ((< tl top) 'before)
     ((>= tl (+ top height)) 'after)
     ((< ll left) 'before)
     ((>= ll (+ left width)) 'after)
     (t 'inside))))

(defun orb-noter-pdf-view--make-document-buffer (document name)
  (with-current-buffer (make-indirect-buffer document name nil t)
    (setq-local buffer-file-name (buffer-local-value 'buffer-file-name document))
    (setq-local orb-noter--insert-note 'orb-noter-pdf-view--insert-note)
    (setq-local orb-noter--goto-location 'orb-noter-pdf-view--goto-location)
    (setq-local orb-noter--get-current-view 'orb-noter-doc-view--get-current-view)
    (funcall (buffer-local-value 'major-mode document))
    (current-buffer)))

(advice-add 'pdf-view-redisplay :after 'orb-noter--display-page-a)

;;;###autoload
(defun orb-noter:pdf-view-mode ()
 '( :make-document-buffer orb-noter-pdf-view--make-document-buffer
    :relative-position-to-view orb-noter-pdf-view--relative-position-to-view
    :compare-location orb-noter-pdf-view--compare-location))

(provide 'orb-noter-pdf-view)
;;; orb-noter-pdf-view.el ends here
