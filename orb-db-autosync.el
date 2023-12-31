;;; orb-db-autosync.el -- 保存文件时同步到SQLite -*- coding: utf-8; lexical-binding: t; -*-

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

;;;###autoload
(define-minor-mode orb-db-autosync-mode
  "Global minor mode to keep your Orb session automatically synchronized.
Through the session this will continue to setup your buffers,
keep track of the related changes, maintain cache consistency and
incrementally update the currently active database.

If you need to manually trigger resync of the currently active
database, see `orb-db-sync' command."
  :group 'orb
  :global t
  :init-value nil
  (cond
   (orb-db-autosync-mode
    (dolist (buf (orb-buffer-list))
      (with-current-buffer buf
        (orb-db-autosync--find-file-h)))
    (add-hook 'find-file-hook 'orb-db-autosync--find-file-h)
    (advice-add 'rename-file :around 'orb-db-autosync--rename-file-a)
    (advice-add 'delete-file :before 'orb-db-autosync--delete-file-a)
    (orb-db-sync))
   (t
    (advice-remove 'delete-file 'orb-db-autosync--delete-file-a)
    (advice-remove 'rename-file 'orb-db-autosync--rename-file-a)
    (remove-hook 'find-file-hook 'orb-db-autosync--find-file-h)
    (dolist (buf (orb-buffer-list))
      (with-current-buffer buf
        (remove-hook 'after-save-hook 'orb-db-autosync--update-on-save-h t))))))

(defun orb-db-autosync--delete-file-a (file &optional _trash)
  "Maintain cache consistency when file deletes."
  (let ((abs-path (expand-file-name file)))
    (when (orb-file-p abs-path)
      (orb-db-delete-file abs-path))))

(defun orb-db-autosync--rename-file-a (fun old-file new-file-or-dir &rest args)
  "Maintain cache consistency of file rename."
  (let* ((old-abs-path
          (expand-file-name
           (let ((old-file (directory-file-name old-file)))
             (if (file-directory-p old-file)
                 (file-name-as-directory old-file)
               old-file))))
         (old-file-name
          (file-name-nondirectory (directory-file-name old-abs-path)))
         (new-abs-path
          (expand-file-name
           (let ((new-file-or-dir
                  (if (directory-name-p new-file-or-dir)
                      (file-name-concat new-file-or-dir old-file-name)
                    new-file-or-dir)))
             (if (directory-name-p old-abs-path)
               (file-name-as-directory new-file-or-dir)
               new-file-or-dir))))
         (result
          (apply fun old-file new-file-or-dir args)))
    (if (orb-file-p old-abs-path)
        (if (orb-file-p new-abs-path)
            (orb-db-rename-file "main" old-abs-path new-abs-path)
          (orb-db-delete-file old-abs-path))
      (when (orb-file-p new-abs-path)
        (orb-db-add-file new-abs-path)))
    result))


(defun orb-db-autosync--find-file-h ()
  "Setup the current buffer if it visits an Org file under orb-directory"
  (when (orb-buffer-p)
    (add-hook 'after-save-hook 'orb-db-autosync--update-on-save-h nil t)))

(defun orb-db-autosync--update-on-save-h ()
  "Update the database for the current file after saving buffer."
  (orb-db-update-file))

(provide 'orb-db-autosync)
;;; orb-db-autosync.el ends here
