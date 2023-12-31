;;; orb-sidebar.el --- Org Mode侧栏  -*- coding: utf-8; lexical-binding: t; -*-

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

(require 'orb-sidebar-section)

(defgroup orb-sidebar nil
  "Orb-Sidebar"
  :group 'orb)

(defcustom orb-sidebar-sections
  '(
    (:name "Contents" :redisplay orb-sidebar-toc-section)
    (:name "Link Target" :redisplay orb-sidebar-link-target-section)
    (:name "Headline Backlink" :redisplay orb-sidebar-link-back-headline-section)
    (:name "Backlink" :redisplay orb-sidebar-link-back-section)
    (:name "TODO" :redisplay orb-sidebar-todo-section)
    (:name "Debug" :redisplay orb-sidebar-debug-section)
   )
  ""
  :type '(list (plist :value-type sexp))
  :group 'orb-sidebar)

(defgroup orb-sidebar-faces nil
  "Faces used by Orb-Sidebar"
  :group 'orb-sidebar
  :group 'faces)

(defface orb-sidebar-section-header
  '((((class color) (background light))
     :extend t
     :background "grey80"
     :foreground "grey30"
     :weight bold)
    (((class color) (background dark))
     :extend t
     :background "grey25"
     :foreground "grey70"
     :weight bold))
  "Face for section header."
  :group 'orb-sidebar-faces)

(defvar orb-sidebar-buffer "*orb-sidebar*"
  "The persistent orb-sidebar buffer name. Must be surround with
\"*\".  The content inside of this buffer will be automatically
updated to the nearest node at point that comes from the current
buffer.  To toggle its display use `orb-sidebar-toggle' command.")

(define-derived-mode orb-sidebar-mode orb-sidebar-section-mode "Orb-Sidebar"
  "Major mode for displaying relevant information about orb-sidebar
nodes.  This mode is used by special orb-sidebar buffers, which render
the information in a section-like manner (see `orb-sidebar-sections'),
with which the user can interact with."
  :group 'orb-sidebar
  (add-to-invisibility-spec '(org-link))
  (setq-local orb-sidebar-section--redisplay 'orb-sidebar--root-redisplay))

(defun orb-sidebar--root-redisplay (overlay)
  (apply
   'orb-sidebar-section-make-sections
   overlay
   (mapcar
    (lambda (sec)
      (list
       :name (propertize (plist-get sec :name) 'face 'orb-sidebar-section-header)
       :redisplay (plist-get sec :redisplay)
       :open (plist-get sec :open)
       :face 'orb-sidebar-section-header))
    orb-sidebar-sections)))

;;;###autoload
(defun orb-sidebar ()
  "Toggle display of the persistent `orb-sidebar-buffer'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let ((window (get-buffer-window orb-sidebar-buffer)))
    (if (and window
             (eq (current-buffer)
                 (with-current-buffer orb-sidebar-buffer orb-sidebar--current-buffer)))
        (quit-window nil window)
      (orb-sidebar-buffer-redisplay)
      (display-buffer orb-sidebar-buffer))))

(defun orb-sidebar-buffer-redisplay ()
  (let ((buffer (current-buffer))
        (sidebar-buffer (get-buffer-create orb-sidebar-buffer)))
    (with-current-buffer sidebar-buffer
      (let ((created orb-sidebar--current-buffer)
            (changed (not (eq buffer orb-sidebar--current-buffer))))
        (when changed
          (orb-sidebar-buffer-remove-hook)
          (setq-local orb-sidebar--current-buffer buffer)
          (with-current-buffer buffer
            (add-hook 'post-command-hook 'orb-sidebar-buffer--redisplay-h nil t))
          (unless created
            (orb-sidebar-mode)
            (add-hook 'kill-buffer-hook 'orb-sidebar-buffer-remove-hook t)))
        (orb-sidebar-section-redisplay-buffer (and created changed))))))

(defun orb-sidebar-buffer-remove-hook ()
  (when (and orb-sidebar--current-buffer
             (buffer-live-p orb-sidebar--current-buffer))
    (with-current-buffer orb-sidebar--current-buffer
      (remove-hook 'post-command-hook 'orb-sidebar-buffer--redisplay-h t))))

(defun orb-sidebar-buffer--redisplay-h ()
  (when (get-buffer-window orb-sidebar-buffer)
    (orb-sidebar-buffer-redisplay)))

(provide 'orb-sidebar)
;;; orb-sidebar.el ends here
