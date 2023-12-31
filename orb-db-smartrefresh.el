;;; orb-db-smartrefresh.el -- 避免不必要的刷新 -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;; 最初是从org-roam-db.el里抄来的

;;; Commentary:

;;; Code:

;; (require 'orb)

(defvar orb-db-smartrefresh--changed-files nil)

(defvar orb-db-smartrefresh--last-modified-tick nil)
(put 'orb-db-smartrefresh--last-modified-tick 'permanent-local t)

;;;###autoload
(define-minor-mode orb-db-smartrefresh-mode
  ""
  :group 'orb
  :global t
  :init-value nil
  (cond
   (orb-db-smartrefresh-mode
    (dolist (buf (orb-buffer-list))
      (with-current-buffer buf
        (orb-db-smartrefresh--find-file-h)))
    (add-hook 'find-file-hook 'orb-db-smartrefresh--find-file-h)
    (advice-add 'set-visited-file-name :around 'orb-db-smartrefresh--rename-buffer-a)
    (advice-add 'orb-db-refresh :around 'orb-db-smartrefresh--db-refresh-a))
   (t
    (advice-remove 'orb-db-refresh 'orb-db-smartrefresh--db-refresh-a)
    (advice-remove 'set-visited-file-name 'orb-db-smartrefresh--rename-buffer-a)
    (remove-hook 'find-file-hook 'orb-db-autosync--find-file-h)
    (dolist (buf (orb-buffer-list))
      (with-current-buffer buf
        (remove-hook 'kill-buffer-hook 'orb-db-smartrefresh--update-on-kill-h t)
        (remove-hook 'post-command-hook 'orb-db-smartrefresh--update-on-command-h t)
        (setq-local orb-db-smartrefresh--last-modified-tick nil))))))

(defun orb-db-smartrefresh--db-refresh-a (fun &optional file-list)
  (let ((abs-path-list
         (seq-filter
          'orb-file-p
          (seq-uniq (mapcar 'expand-file-name file-list)))))
    (when-let ((files
                (if file-list
                    (seq-intersection abs-path-list orb-db-smartrefresh--changed-files)
                  orb-db-smartrefresh--changed-files)))
      (if file-list
          (funcall fun files)
        (funcall fun))
      (setq orb-db-smartrefresh--changed-files
            (seq-difference
             orb-db-smartrefresh--changed-files
             files)))))

(defun orb-db-smartrefresh--rename-buffer-a (fun filename &optional no-query along-with-file)
  (let ((old-file-name (when (orb-buffer-p) buffer-file-name))
        (result (funcall fun filename no-query along-with-file))
        (buffer-p (orb-buffer-p)))
    (cond
     ((and old-file-name buffer-p)
      (orb-db-rename-file "main" old-file-name buffer-file-name))
     (old-file-name
      (add-to-list 'orb-db-smartrefresh--changed-files old-file-name)
      (remove-hook 'kill-buffer-hook 'orb-db-smartrefresh--update-on-kill-h t)
      (remove-hook 'post-command-hook 'orb-db-smartrefresh--update-on-command-h t)
      (setq-local orb-db-smartrefresh--last-modified-tick nil))
     (buffer-p
      (setq-local orb-db-smartrefresh--last-modified-tick (buffer-chars-modified-tick))
      (add-hook 'post-command-hook 'orb-db-smartrefresh--update-on-command-h nil t)
      (add-hook 'kill-buffer-hook 'orb-db-smartrefresh--update-on-kill-h nil t)))
    result))

(defun orb-db-smartrefresh--find-file-h ()
  "Setup the current buffer if it visits an Org file under orb-directory"
  (when (orb-buffer-p)
    (setq-local orb-db-smartrefresh--last-modified-tick (buffer-chars-modified-tick))
    (add-hook 'post-command-hook 'orb-db-smartrefresh--update-on-command-h nil t)
    (add-hook 'kill-buffer-hook 'orb-db-smartrefresh--update-on-kill-h nil t)))

(defun orb-db-smartrefresh--update-on-kill-h ()
  (add-to-list 'orb-db-smartrefresh--changed-files buffer-file-name))

(defun orb-db-smartrefresh--update-on-command-h ()
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick orb-db-smartrefresh--last-modified-tick)
      (setq-local orb-db-smartrefresh--last-modified-tick tick)
      (add-to-list 'orb-db-smartrefresh--changed-files buffer-file-name))))

(provide 'orb-db-smartrefresh)
;;; orb-db-smartrefresh.el ends here
