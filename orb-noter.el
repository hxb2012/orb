;; orb-noter.el --- Noter  -*- coding: utf-8; lexical-binding: t; -*-

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
(defvar org-capture-link-is-already-stored)
(defvar org-capture-entry)

(defgroup orb-noter nil
  "A synchronized, external annotator"
  :group 'orb)

(defcustom orb-noter-property-doc "NOTER_DOC"
  "Name of the property that specifies the document."
  :group 'orb-noter
  :type 'string)

(defcustom orb-noter-property-loc "NOTER_LOC"
  "Name of the property that specifies the location of the current note."
  :group 'orb-noter
  :type 'string)

(defcustom orb-noter--find-doc-node-functions '(orb-noter--find-doc-node)
  ""
  :group 'orb-noter
  :type 'hook)

(defcustom orb-noter--find-doc-buffer-functions '(orb-noter--find-doc-buffer)
  ""
  :group 'orb-noter
  :type 'hook)

(defvar orb-noter--window nil)
(put 'orb-noter--window 'permanent-local t)

(defvar orb-noter--timer nil)
(put 'orb-noter--timer 'permanent-local t)

(defvar orb-noter--note-level nil)
(put 'orb-noter--note-level 'permanent-local t)

(defvar orb-noter--notes nil)
(put 'orb-noter--notes 'permanent-local t)

(defvar orb-noter--current nil)
(put 'orb-noter--current 'permanent-local t)

(defvar orb-noter--insert-note nil)
(put 'orb-noter--insert-note 'permanent-local t)

(defvar orb-noter--get-current-view nil)
(put 'orb-noter--get-current-view 'permanent-local t)

(defvar orb-noter--relative-position-to-view nil)
(put 'orb-noter--relative-position-to-view 'permanent-local t)

(defvar orb-noter--goto-location nil)
(put 'orb-noter--goto-location 'permanent-local t)

(defvar orb-noter--compare-location nil)
(put 'orb-noter--compare-location 'permanent-local t)

(defun orb-noter--notes-get-location (&optional pom)
  (when-let ((loc (org-entry-get pom orb-noter-property-loc)))
    (car (read-from-string loc))))

(defun orb-noter--notes-check ()
  (unless (eq (selected-window) orb-noter--window)
    (user-error "called outside Org-Noter Notes buffer")))

(defun orb-noter--notes-first-node ()
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (while (< orb-noter--note-level (org-outline-level))
    (org-end-of-subtree)
    (org-next-visible-heading 1)))

(defun orb-noter--notes-back-to-note ()
  (org-back-to-heading-or-point-min)
  (while (< orb-noter--note-level (org-outline-level))
    (org-up-heading-or-point-min))
  (unless (bobp)
    (point)))

(defun orb-noter--notes-goto-location (location)
  (with-selected-window (window-parameter orb-noter--window 'orb-noter--sibling)
    (funcall orb-noter--goto-location location)))

(defun orb-noter--notes-goto-prev-note ()
  (when (orb-noter--notes-back-to-note)
    (let (location)
      (while (and (not location)
                  (org-get-previous-sibling))
        (setq location (orb-noter--notes-get-location)))
      (when location
        (orb-noter--notes-goto-location location)
        (point)))))

(defun orb-noter--notes-goto-next-note ()
  (let (location)
    (if (orb-noter--notes-back-to-note)
        (org-get-next-sibling)
      (orb-noter--notes-first-node))
    (unless (eobp)
      (setq location (orb-noter--notes-get-location))
      (while (and (not location)
                  (org-get-next-sibling))
        (setq location (orb-noter--notes-get-location)))
      (when location
        (orb-noter--notes-goto-location location)
        (point)))))

(defun orb-noter--doc-map-notes (func)
  (with-selected-window orb-noter--window
    (save-excursion
      (orb-noter--notes-first-node)
      (while (not (eobp))
        (when-let ((location (orb-noter--notes-get-location)))
          (funcall func location))
        (org-get-next-sibling)))))

(defun orb-noter--doc-location-change-handler (window)
  (with-selected-window window
    (let ((notes nil)
          (view (funcall orb-noter--get-current-view)))
      (orb-noter--doc-map-notes
       (lambda (location)
         (when (eq 'inside
                   (funcall orb-noter--relative-position-to-view location view))
           (push (cons location (point)) notes))))
      (setq notes (vconcat (nreverse notes)))
      (setq-local orb-noter--notes notes)
      (setq-local orb-noter--current
                  (with-selected-window orb-noter--window
                    (if-let ((location (orb-noter--notes-get-location)))
                      (pcase (funcall orb-noter--relative-position-to-view
                                      location view)
                        ('before 0)
                        ('after (1- (length notes)))
                        (_
                         (seq-position notes location
                                       (lambda (x v)
                                         (equal (car x) v)))))
                      0))))
    (force-mode-line-update t)))

(defun orb-noter--doc-scroll-handler ()
  (when (or (not orb-noter--timer)
            (timer--triggered orb-noter--timer))
    (setq-local orb-noter--timer
                (run-at-time 0 nil
                             'orb-noter--doc-location-change-handler
                             (selected-window)))))

(defun orb-noter--window-scroll-h (window _start)
  (with-selected-window window
    (orb-noter--doc-scroll-handler)))

(defun orb-noter--display-page-a (&rest _ignore)
  (when orb-noter--window
    (orb-noter--doc-scroll-handler)))

(defun orb-noter--doc-sync-note ()
  (interactive)
  (if (length> orb-noter--notes 0)
    (pcase-let ((`(,location . ,pos) (elt orb-noter--notes orb-noter--current)))
      (with-selected-window orb-noter--window
        (goto-char pos)
        (orb-noter--notes-goto-location location)))
    (user-error "No note selected")))

(defun orb-noter--doc-prev-note ()
  (interactive)
  (when (or (length= orb-noter--notes 0) (zerop orb-noter--current))
      (user-error "There is no previous note"))
  (setq-local orb-noter--current (1- orb-noter--current))
  (orb-noter--doc-sync-note))

(defun orb-noter--doc-next-note ()
  (interactive)
  (when (or (length= orb-noter--notes 0)
            (length= orb-noter--notes (1+ orb-noter--current) ))
      (user-error "There is no next note"))
  (setq-local orb-noter--current (1+ orb-noter--current))
  (orb-noter--doc-sync-note))

(defun orb-noter--doc-insert-note ()
  (interactive)
  (pcase-let ((`(,location . ,text) (funcall orb-noter--insert-note)))
    (select-window orb-noter--window)
    (orb-noter--notes-first-node)
    (while (and
            (if-let ((loc (orb-noter--notes-get-location)))
                (funcall orb-noter--compare-location '<= loc location)
              t)
            (> (point-max) (point)))
      (org-get-next-sibling))
    (let* ((level (1+ (save-excursion (goto-char (point-min)) (org-outline-level))))
           (width (- (- fill-column level) 1))
           (line (string-join (split-string text nil t) " "))
           (single-line (< (string-width line) width))
           (title (if single-line line
                    (truncate-string-to-width line width nil nil t)))
           (body (if single-line ""
                   (concat "#+BEGIN_QUOTE\n" text "\n#+END_QUOTE\n\n")))
           (org-capture-link-is-already-stored t)
           (pom)
           (org-capture-entry
            (list "note" "note" 'plain '(here)
                  (concat (make-string level ?*) " " title
                          "%?\n:PROPERTIES:\n:CAPTURED: %u\n:"
                          orb-noter-property-loc
                          ": "
                          (format "%s" location)
                         "\n:END:\n\n" body)
                  :before-finalize
                  (lambda ()
                    (setq pom (point)))
                  :no-save t
                  :immediate-finish t
                  :empty-lines 1)))
      (save-restriction
        (org-capture 0))
      (goto-char pom))))

(defun orb-noter--notes-sync-note ()
  (interactive)
  (orb-noter--notes-check)
  (if-let* ((pom (save-excursion (orb-noter--notes-back-to-note)))
            (location (orb-noter--notes-get-location pom)))
      (orb-noter--notes-goto-location location)
    (user-error "No note selected")))

(defun orb-noter--notes-prev-note ()
  (interactive)
  (orb-noter--notes-check)
  (if-let ((pom (save-excursion (orb-noter--notes-goto-prev-note))))
      (goto-char pom)
    (user-error "There is no previous note")))

(defun orb-noter--notes-next-note ()
  (interactive)
  (orb-noter--notes-check)
  (if-let ((pom (save-excursion (orb-noter--notes-goto-next-note))))
      (goto-char pom)
    (user-error "There is no next note")))

(defun orb-noter--mode-line-text ()
  (let ((number-of-notes (length orb-noter--notes)))
    (cond
     ((= number-of-notes 0) (propertize " 0 notes" 'face 'warning))
     ((= number-of-notes 1) (propertize " 1 note" 'face 'success))
     (t (propertize (format " %d notes" number-of-notes) 'face 'success)))))

(defvar orb-noter--doc-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "M-i" 'orb-noter--doc-insert-note)
    (keymap-set map "M-p" 'orb-noter--doc-prev-note)
    (keymap-set map "M-." 'orb-noter--doc-sync-note)
    (keymap-set map "M-n" 'orb-noter--doc-next-note)
    map)
  "")

(define-minor-mode orb-noter--doc-mode
  "Minor mode for the document buffer."
  :lighter (:eval (orb-noter--mode-line-text)))

(defvar orb-noter--notes-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "M-p" 'orb-noter--notes-prev-note)
    (keymap-set map "M-." 'orb-noter--notes-sync-note)
    (keymap-set map "M-n" 'orb-noter--notes-next-note)
    map)
  "")

(define-minor-mode orb-noter--notes-mode
  "Minor mode for the notes buffer.")

(defun orb-noter--find-doc-node ()
  (org-with-wide-buffer
   (org-back-to-heading-or-point-min)
   (while (not (or (org-entry-get nil orb-noter-property-doc)
                   (eq (point) (point-min))))
     (org-up-heading-or-point-min))
   (when (org-entry-get nil orb-noter-property-doc)
     (point))))

(defun orb-noter--find-doc-buffer (pom)
  (when-let ((s (org-entry-get pom orb-noter-property-doc)))
    (let* ((link
            (with-temp-buffer
              (let ((org-inhibit-startup nil))
                (insert s)
                (org-mode)
                (goto-char (point-min))
                (org-element-link-parser)))))
      (when (equal (org-element-property :type link) "file")
        (find-file (org-element-property :path link))))))

(defun orb-noter--delete-window (window)
  (let ((sibling (window-parameter window 'orb-noter--sibling))
        (ignore-window-parameters t))
    (delete-window window)
    (run-at-time 0 nil
                 (lambda ()
                   (with-selected-window sibling
                     (kill-buffer))))))

(defun orb-noter--create-session (pom)
  (let* ((buffer (current-buffer))
         (display-name
          (org-with-wide-buffer
           (goto-char pom)
           (or (org-get-heading t t t t)
               (org-get-title)
               (file-name-nondirectory buffer-file-name))))
         (document
          (or (run-hook-with-args-until-success
               'orb-noter--find-doc-buffer-functions pom)
              (user-error "Document file not found")))
         (mode (buffer-local-value 'major-mode document))
         (symbol (intern (format "orb-noter:%s" mode)))
         (funcs (if (fboundp symbol)
                    (funcall symbol)
                  (user-error "Major mode %S not supported" mode)))
         (frame-name (format "Emacs Org-noter - %s" display-name))
         (document-buffer-name
          (generate-new-buffer-name (concat "Org-noter: " display-name)))
         (notes-buffer
          (make-indirect-buffer
           (or (buffer-base-buffer buffer) buffer)
           (generate-new-buffer-name (concat "Notes of " display-name))
           t))
         (frame (make-frame
                 `((name . ,frame-name) (fullscreen . maximized)))))
    (with-selected-frame frame
      (delete-other-windows)
      (let ((document-window (selected-window))
            (notes-window (split-window-right)))
        (set-window-parameter document-window 'orb-noter--sibling notes-window)
        (set-window-parameter notes-window 'orb-noter--sibling document-window)
        (dolist (win (list document-window notes-window))
          (set-window-parameter win 'delete-window 'orb-noter--delete-window)
          (set-window-parameter win 'no-delete-other-windows t))
        (with-current-buffer notes-buffer
          (setq-local orb-noter--window notes-window)
          (setq-local orb-noter--compare-location (plist-get funcs :compare-location))
          (setq-local orb-noter--relative-position-to-view
                      (plist-get funcs :relative-position-to-view))
          (goto-char pom)
          (setq-local orb-noter--note-level (1+ (org-outline-level)))
          (org-narrow-to-subtree)
          (orb-noter--notes-mode t))
        (set-window-buffer notes-window notes-buffer)
        (let ((document-buffer (funcall (plist-get funcs :make-document-buffer)
                                        document document-buffer-name)))
          (with-current-buffer document-buffer
            (setq-local orb-noter--window notes-window)
            (orb-noter--doc-mode t))
          (set-window-buffer document-window document-buffer))
        (set-window-dedicated-p document-window t)
        (set-window-dedicated-p notes-window t)))
    notes-buffer))

;;;###autoload
(defun orb-noter ()
  (interactive)
  (when (if (eq major-mode 'org-mode) orb-noter--notes-mode orb-noter--doc-mode)
    (user-error "Already in a noter session"))
  (unless (eq major-mode 'org-mode)
    (orb-capture-noter))
  (let* ((pos (point))
         (notes-buffer
          (let ((pom (or (run-hook-with-args-until-success
                          'orb-noter--find-doc-node-functions)
                         (user-error "No document node found"))))
            (or (find-buffer-visiting
                 buffer-file-name
                 (lambda (buf) (with-current-buffer buf
                                 (and orb-noter--notes-mode (eq (point-min) pom)))))
                (orb-noter--create-session pom))))
         (window (buffer-local-value 'orb-noter--window notes-buffer)))
    (redisplay t)
    (select-frame-set-input-focus (window-frame window))
    (select-window window)
    (goto-char pos)
    (orb-noter--notes-sync-note)))

(provide 'orb-noter)
;;; orb-noter.el ends here
