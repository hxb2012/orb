;;; orb-sidebar-debug.el --- 调试信息  -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Commentary:

;; 最初是从org-roam-mode.el里抄来的

;;; Code:

(defvar orb-sidebar--current-buffer)

(defun orb-sidebar-debug--buffer (&optional _overlay)
  (delete-region (point-min) (point-max))
  (insert
   (with-current-buffer orb-sidebar--current-buffer
     (format
      "Line: %s\nColumn: %s\nPosition: %s\n"
      (line-number-at-pos)
      (current-column)
      (point)))))

(defun orb-sidebar-debug--text-properties (&optional _overlay)
  (delete-region (point-min) (point-max))
  (let ((buffer (current-buffer))
        (pos (with-current-buffer orb-sidebar--current-buffer (point))))
    (insert "Text content at position " (format "%d" pos) ":\n")
    (ignore-errors
      (with-current-buffer orb-sidebar--current-buffer
        (describe-text-properties (point) buffer)))))

(defun orb-sidebar-debug--org-element (&optional _overlay)
  (delete-region (point-min) (point-max))
  (insert
   (pp-to-string
    (with-current-buffer orb-sidebar--current-buffer
      (org-element-context)))))

;;;###autoload
(defun orb-sidebar-debug-section (overlay)
  (orb-sidebar-section-make-sections
   overlay
   (list
    :name (propertize "Buffer" 'face 'orb-sidebar-section-header)
    :redisplay 'orb-sidebar-debug--buffer
    :open t
    :face 'orb-sidebar-section-header)
   (list
    :name (propertize "Text Properties" 'face 'orb-sidebar-section-header)
    :redisplay 'orb-sidebar-debug--text-properties
    :open t
    :face 'orb-sidebar-section-header)
   (list
    :name (propertize "Org Element" 'face 'orb-sidebar-section-header)
    :redisplay 'orb-sidebar-debug--org-element
    :open t
    :face 'orb-sidebar-section-header)))

(provide 'orb-sidebar-debug)
;;; orb-sidebar-debug.el ends here
