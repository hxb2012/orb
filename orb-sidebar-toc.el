;;; orb-sidebar-toc.el --- 目录  -*- coding: utf-8; lexical-binding: t; -*-

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

(defun orb-sidebar-toc--insert (level heading pos)
  (insert
   (concat (make-string level ? )
           (propertize heading 'orb-section t 'orb-pos pos)
           "\n")))

(defun orb-sidebar-toc--insert-heading (buffer)
  (let ((heading (buffer-substring (line-beginning-position) (line-end-position)))
        (level (org-current-level))
        (pos (line-beginning-position)))
    (with-current-buffer buffer
      (orb-sidebar-toc--insert level heading pos))))

;;;###autoload
(defun orb-sidebar-toc-section (overlay)
  (let* ((sidebar-buffer (current-buffer))
         (buffer orb-sidebar--current-buffer)
         (tick (buffer-chars-modified-tick buffer))
         (pos (with-current-buffer buffer (point))))
    (unless (eq tick (overlay-get overlay 'orb-last-tick))
      (delete-region (point-min) (point-max))
      (orb-sidebar-toc--insert 0 (buffer-name buffer) 1)
      (with-current-buffer buffer
         (org-map-region
          (lambda () (orb-sidebar-toc--insert-heading sidebar-buffer))
          (point-min)
          (point-max)))
      (overlay-put overlay 'orb-last-tick tick))
    (when-let ((ov (overlay-get overlay 'orb-highlight)))
      (delete-overlay ov))
    (goto-char (point-max))
    (while-let ((prop-match (text-property-search-backward 'orb-pos)))
      (when-let ((value (prop-match-value prop-match)))
        (when (<= value pos)
          (let ((ov (make-overlay (prop-match-beginning prop-match) (prop-match-end prop-match))))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'face 'secondary-selection)
            (overlay-put overlay 'orb-highlight ov))
          (goto-char (point-min)))))))

(provide 'orb-sidebar-toc)
;;; orb-sidebar-toc.el ends here
