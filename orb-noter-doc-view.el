;;; orb-noter-doc-view.el --- Noter DocView  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Gonçalo Santos

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

;; 最初是从org-noter.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb-noter)

(declare-function image-set-window-hscroll "image" (ncol))
(declare-function image-set-window-vscroll "image" (vscroll))
(declare-function image-get-display-property "image" ())
(declare-function image-display-size "image" (spec &optional pixels frame))
(declare-function image-mode-window-get "image" (prop &optional winprops))
(declare-function doc-view--current-cache-dir "doc-view" ())
(declare-function doc-view-goto-page "doc-view" (page))
(declare-function doc-view-reset-slice "doc-view" ())

(defvar doc-view-single-page-converter-function)
(defvar doc-view--image-file-pattern)
(defvar doc-view-cache-directory)

(defun orb-noter-doc-view--get-page-slice ()
  (if-let ((slice (image-mode-window-get 'slice)))
      (pcase-let ((`(,left ,top ,width ,height) slice)
                  (`(,w . ,h) (image-size (image-mode-window-get 'image) t)))
        (list (/ (float left) w)
              (/ (float top) h)
              (/ (float width) w)
              (/ (float height) h)))
      '(0.0 0.0 1.0 1.0)))

(defun orb-noter-doc-view--from-slice (pos)
  (pcase-let ((`(,left ,top ,width ,height) (orb-noter-doc-view--get-page-slice)))
    (cons
     (+ left (* width (car pos)))
     (+ top (* height (cdr pos))))))

(defun orb-noter-doc-view--to-slice (pos)
  (pcase-let ((`(,left ,top ,width ,height) (orb-noter-doc-view--get-page-slice))
              (`(,x . ,y) pos))
    (when (and (<= left x) (<= x (+ left width))
               (<= top y) (<= y (+ top height)))
      (cons
       (/ (- x left) width)
       (/ (- y top) height)))))

(defun orb-noter-doc-view--offset ()
  (if-let ((s (overlay-get (image-mode-window-get 'overlay) 'before-string)))
    (string-pixel-width s)
    0))

(defun orb-noter-doc-view--posn-to-location (posn)
  (pcase-let* ((`(,px . ,py) (posn-x-y posn))
               (`(,w . ,h) (image-display-size (image-get-display-property) t))
               (x (- (+ px (* (frame-char-width) (window-hscroll)))
                     (orb-noter-doc-view--offset)))
               (y (+ py (window-vscroll nil t))))
    (when (and (<= 0 x) (< x w) (<= 0 y) (< y h))
      (pcase-let ((page (image-mode-window-get 'page))
                  (`(,sx . ,sy) (orb-noter-doc-view--from-slice
                          (cons (/ (float x) w) (/ (float y) h)))))
        (list page sx sy)))))

(defun orb-noter-doc-view--insert-note ()
  (let (event)
    (while (not (and (eq 'mouse-1 (car event))
                     (eq (selected-window) (posn-window (event-start event)))))
      (setq event (read-event "Click where you want the start of the note to be!")))
    (if-let ((location (orb-noter-doc-view--posn-to-location (event-start event))))
        (cons location "")
      (user-error "position outside of the image"))))

(defun orb-noter-doc-view--scroll-into-view (x y)
  (pcase-let* ((`(,sx . ,sy)
                (if-let ((pos (orb-noter-doc-view--to-slice (cons x y))))
                    pos
                  (doc-view-reset-slice)
                  (cons x y)))
               (height (window-body-height))
               (width (window-body-width))
               (`(,w . ,h) (image-display-size (image-get-display-property)))
               (col (* sx w))
               (row (* sy h))
               (left (window-hscroll))
               (top (window-vscroll))
               (min-left (max 0 (- (1+ (ceiling col)) width)))
               (min-top (max 0 (- (1+ (ceiling row)) height)))
               (max-left (min (floor col) (max 0 (- (1+ (floor w)) width))))
               (max-top (min (floor row) (max 0 (- (1+ (floor h)) height))))
               (new-left (cond ((< left min-left) min-left)
                               ((> left max-left) max-left)))
               (new-top (cond ((< top min-top) min-top)
                              ((> top max-top) max-top))))
    (when new-top
      (image-set-window-vscroll
       (if (< emacs-major-version 27)
           new-top
         (* new-top (frame-char-height)))))
    (when new-left
      (image-set-window-hscroll new-left))
    (let* (use-system-tooltips
           (edges (window-absolute-body-pixel-edges))
           (cw (frame-char-width))
           (dx (round (+ (nth 0 edges)
                         (orb-noter-doc-view--offset)
                         (* (- (- col 1) left) cw))))
           (dy (round (+ (nth 1 edges)
                         (* (- (- row 0.5) top) (frame-char-height)))))
           (tooltip-frame-parameters
            `((border-width . 0)
              (internal-border-width . 0)
              (alpha-background . 0)
              (left . ,dx)
              (top . ,dy))))
      (tooltip-show
       (propertize
        "\u2192"
        'face '(:foreground "orange red"))))))

(defun orb-noter-doc-view--goto-location (location)
  (let* ((converter doc-view-single-page-converter-function)
         (win (selected-window))
         (doc-view-single-page-converter-function
          (lambda (file image page callback)
            (funcall
             converter file image page
             (lambda ()
               (when (funcall callback)
                 (with-selected-window win
                   (apply 'orb-noter-doc-view--scroll-into-view
                          (cdr location)))))))))
    (doc-view-goto-page (car location))
    (let* ((page (image-mode-window-get 'page))
           (file (expand-file-name
                  (format doc-view--image-file-pattern page)
                  (doc-view--current-cache-dir))))
      (when (file-exists-p file)
        (apply 'orb-noter-doc-view--scroll-into-view (cdr location))))))

(defun orb-noter-doc-view--compare-location (comp loc1 loc2)
  (let ((p1 (car loc1))
        (p2 (car loc2)))
    (if (= p1 p2)
        (let ((y1 (caddr loc1))
              (y2 (caddr loc2)))
          (if (= y1 y2)
              (funcall comp (cadr loc1) (cadr loc2))
            (funcall comp y1 y2)))
      (funcall comp p1 p2))))

(defun orb-noter-doc-view--get-current-view ()
  (cons (image-mode-window-get 'page)
        (orb-noter-doc-view--get-page-slice)))

(defun orb-noter-doc-view--relative-position-to-view (location view)
  (pcase-let ((`(,pl ,x ,y) location)
              (`(,pv ,left ,top ,width ,height) view))
    (cond
     ((< pl pv) 'before)
     ((> pl pv) 'after)
     ((< y top) 'before)
     ((>= y (+ top height) 'after))
     ((< x left) 'before)
     ((>= x (+ left width)) 'after)
     (t 'inside))))

(defun orb-noter-doc-view--make-document-buffer (document name)
  (with-current-buffer (make-indirect-buffer document name nil t)
    (setq-local buffer-file-name (buffer-local-value 'buffer-file-name document))
    (setq-local orb-noter--insert-note 'orb-noter-doc-view--insert-note)
    (setq-local orb-noter--goto-location 'orb-noter-doc-view--goto-location)
    (setq-local orb-noter--get-current-view 'orb-noter-doc-view--get-current-view)
    (setq-local doc-view-saved-settings
                `((doc-view-cache-directory
                   . ,(concat doc-view-cache-directory "-noter"))))
    (funcall (buffer-local-value 'major-mode document))
    (current-buffer)))

(advice-add 'doc-view-insert-image :after 'orb-noter--display-page-a)

;;;###autoload
(defun orb-noter:doc-view-mode ()
  '( :make-document-buffer orb-noter-doc-view--make-document-buffer
     :relative-position-to-view orb-noter-doc-view--relative-position-to-view
     :compare-location orb-noter-doc-view--compare-location))

(provide 'orb-noter-doc-view)
;;; orb-noter-doc-view.el ends here
