;;; orb-sidebar-section.el --- 折叠时不刷新  -*- coding: utf-8; lexical-binding: t; -*-

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

;; 最初是从org-roam-mode.el里抄来的

;;; Commentary:

;;; Code:

(defvar orb-sidebar--current-buffer nil)
(put 'orb-sidebar--current-buffer 'permanent-local t)

(defvar orb-sidebar-section--redisplay nil)
(put 'orb-sidebar-section--redisplay 'permanent-local t)

(define-fringe-bitmap 'orb-sidebar-section-collapsed [48 24 12 6 12 24 48 0])
(define-fringe-bitmap 'orb-sidebar-section-expanded [0 0 65 99 54 28 8 0])

(defun orb-sidebar-section--update-display (overlay open)
  (overlay-put
   overlay
   'before-string
   (propertize
    "*orb-sidebar-section fringe*"
    'display
    (list
     'left-fringe
     (if open
         'orb-sidebar-section-expanded
       'orb-sidebar-section-collapsed))))
  (overlay-put overlay 'display (unless open "...\n")))

(defun orb-sidebar-section--make (start body-start props)
  (let ((body-end (point)))
    (insert (propertize "\f" 'display ""))
    (let ((overlay (make-overlay body-start body-end nil nil t)))
      (overlay-put overlay 'evaporate t)
      (put-text-property start body-start 'orb-section overlay)
      (when-let ((redisplay (plist-get props :redisplay)))
        (put-text-property start body-start 'orb-section-redisplay redisplay)
        (overlay-put overlay 'orb-section-destroy t))
      (orb-sidebar-section--update-display overlay (plist-get props :open))
      overlay)))

;;;###autoload
(defun orb-sidebar-section-make (&rest props)
  (if-let ((name (plist-get props :name)))
      (let ((start (point)))
        (insert name)
        (let ((body-start (point)))
          (if-let ((face (plist-get props :face)))
              (insert (propertize "\n" 'face face))
            (insert "\n"))
          (orb-sidebar-section--make start body-start props)))
    (let ((start (point))
          (body-start (line-end-position)))
      (if (< body-start (point-max))
          (goto-char (1+ body-start))
        (if-let ((face (get-text-property start 'face)))
            (insert (propertize "\n" 'face face))
          (insert "\n")))
      (orb-sidebar-section--make start body-start props))))

(defun orb-sidebar-section--get-overlay (match force)
  (let* ((start (prop-match-beginning match))
         (end (prop-match-end match))
         (ov (get-text-property start 'orb-section)))
    (if (and force
             (overlay-get ov 'orb-section-destroy))
        (let ((overlay (make-overlay (overlay-start ov) (overlay-end ov) nil nil t)))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'orb-section-destroy t)
          (overlay-put overlay 'display (overlay-get ov 'display))
          (overlay-put overlay 'before-string (overlay-get ov 'before-string))
          (delete-overlay ov)
          (put-text-property start end 'orb-section overlay)
          overlay)
      ov)))

;;;###autoload
(defmacro orb-sidebar-section--with-restriction (overlay &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((start-var (gensym))
        (end-var (gensym)))
    `(let ((,start-var (overlay-start ,overlay))
           (,end-var (overlay-end ,overlay)))
       (goto-char ,start-var)
       (save-excursion
         (with-restriction (1+ ,start-var) ,end-var
           (unwind-protect
               (progn ,@body)
             (goto-char (point-max))
             (when (and (> (point-max) (point-min))
                        (/= (char-after (1- (point-max))) ?\n))
               (insert "\n"))))))))

(defun orb-sidebar-section--redisplay (redisplay overlay)
  (orb-sidebar-section--with-restriction overlay
    (funcall redisplay overlay))
  (overlay-put overlay 'orb-section-ready t))

(defun orb-sidebar-section-redisplay (redisplay match &optional force)
  (let ((inhibit-read-only t))
    (save-excursion
      (let* ((overlay (orb-sidebar-section--get-overlay match force)))
        (orb-sidebar-section--redisplay redisplay overlay)
        (with-restriction (point) (overlay-end overlay)
          (while-let ((m (text-property-search-forward 'orb-section-redisplay)))
            (let ((ov (orb-sidebar-section--get-overlay m force)))
              (if (overlay-get ov 'display)
                  (progn
                    (overlay-put ov 'orb-section-ready nil)
                    (goto-char (overlay-end ov)))
                (orb-sidebar-section--redisplay (prop-match-value m) ov)))))))))

(defun orb-sidebar-section--section-at (&optional pos)
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char (1+ pos))
      (text-property-search-backward 'orb-section))))

(defun orb-sidebar-section-redisplay-section (&optional force)
  (interactive "P")
  (let* ((m (orb-sidebar-section--section-at))
         (pos (prop-match-beginning m))
         (overlay (prop-match-value m))
         (redisplay (get-text-property pos 'orb-section-redisplay)))
    (unless (and (overlayp overlay) redisplay)
      (user-error "Section has no redisplay"))
    (orb-sidebar-section-redisplay redisplay m force)
    (let ((overlay (get-text-property pos 'orb-section)))
      (unless (overlay-get overlay 'display)
        (orb-sidebar-section--update-display overlay t)))))

(defun orb-sidebar-section-redisplay-buffer (&optional force)
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (orb-sidebar-section-redisplay
       orb-sidebar-section--redisplay
       (text-property-search-forward 'orb-section)
       force))))

(defun orb-sidebar-section-visit ()
  (interactive)
  (when-let ((pos (car (get-char-property-and-overlay (point) 'orb-pos))))
    (let ((buffer
           (if-let ((file (car (get-char-property-and-overlay (point) 'orb-file))))
               (find-file-noselect file)
             orb-sidebar--current-buffer)))
      (select-window (display-buffer buffer 'display-buffer-reuse-window))
      (goto-char pos))))

(defun orb-sidebar-section-toggle ()
  (interactive)
  (let* ((m (orb-sidebar-section--section-at))
         (pos (prop-match-beginning m))
         (overlay (prop-match-value m)))
    (when (overlayp overlay)
      (let ((open (overlay-get overlay 'display))
            (redisplay (get-text-property pos 'orb-section-redisplay)))
        (when (and open redisplay
                   (not (overlay-get overlay 'orb-section-ready)))
          (orb-sidebar-section-redisplay redisplay m))
        (orb-sidebar-section--update-display overlay open)))))

(defun orb-sidebar-section--search-backward (prop)
  (catch :ok
    (while-let
        ((match (text-property-search-backward prop nil nil t)))
      (if-let
          ((starts
            (mapcar
             'overlay-start
             (seq-filter
              (lambda (o) (overlay-get o 'display))
              (overlays-at (point))))))
          (goto-char (seq-min starts))
        (throw :ok (prop-match-beginning match))))
    nil))

(defun orb-sidebar-section--search-forward (prop)
  (catch :ok
    (while-let
        ((match (text-property-search-forward prop nil nil t)))
      (goto-char (prop-match-beginning match))
      (if-let
          ((ends
            (mapcar
             'overlay-end
             (seq-filter
              (lambda (o) (overlay-get o 'display))
              (overlays-at (point))))))
          (goto-char (seq-max ends))
        (throw :ok (point))))
    nil))

(defun orb-sidebar-section-prev ()
  (interactive)
  (if-let ((pos (save-excursion (orb-sidebar-section--search-backward 'orb-section))))
      (goto-char pos)
    (user-error "Beginning of Buffer")))

(defun orb-sidebar-section-next ()
  (interactive)
  (if-let ((pos (save-excursion (orb-sidebar-section--search-forward 'orb-section))))
      (goto-char pos)
    (user-error "End of Buffer")))

(defun orb-sidebar-section-backward ()
  (interactive)
  (if-let ((pos (save-excursion (orb-sidebar-section--search-backward 'orb-item))))
      (goto-char pos)
    (user-error "Beginning of Buffer")))

(defun orb-sidebar-section-forward ()
  (interactive)
  (if-let ((pos (save-excursion (orb-sidebar-section--search-forward 'orb-item))))
      (goto-char pos)
    (user-error "End of Buffer")))

(defvar orb-sidebar-section-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "RET" 'orb-sidebar-section-visit)
    (keymap-set map "<mouse-1>" 'orb-sidebar-section-visit)
    (keymap-set map "TAB" 'orb-sidebar-section-toggle)
    (keymap-set map "p" 'orb-sidebar-section-prev)
    (keymap-set map "n" 'orb-sidebar-section-next)
    (keymap-set map "f" 'orb-sidebar-section-forward)
    (keymap-set map "b" 'orb-sidebar-section-backward)
    (keymap-set map "r" 'orb-sidebar-section-redisplay-section)
    map)
  "Parent keymap for all keymaps of modes derived from `orb-sidebar-section-mode'.")

;;;###autoload
(define-derived-mode orb-sidebar-section-mode special-mode "Orb-Sidebar-Section"
  ""
  :group 'orb-sidebar-section
  (buffer-disable-undo)
  (let* ((inhibit-read-only t)
         (overlay (orb-sidebar-section-make :name "root" :open t))
         (end (overlay-start overlay))
         (start (1+ end)))
    (put-text-property (point-min) start 'display "")
    (overlay-put overlay 'orb-sidebar-section-destroy nil)
    (goto-char start)
    (narrow-to-region start (overlay-end overlay))))

;;;###autoload
(defun orb-sidebar-section-make-sections (overlay &rest sections)
  (unless (overlay-get overlay 'orb-section-ready)
    (overlay-put overlay 'orb-section-destroy nil)
    (delete-region (point-min) (point-max))
    (dolist (section sections)
      (apply 'orb-sidebar-section-make section))))


(provide 'orb-sidebar-section)
;;; orb-sidebar-section.el ends here
