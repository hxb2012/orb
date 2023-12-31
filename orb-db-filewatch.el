;;; orb-db-filewatch.el --- 文件变化时同步到SQLite  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;; 最初是从filenotify.el里抄来的

;;; Commentary:

;;; Code:

(require 'orb)

(defvar orb-db-filewatch-functions nil)

(defvar orb-db-filewatch--desc-alist nil)

(defvar orb-db-filewatch--pending-move-alist nil)

(defun orb-db-filewatch--get-move-desc (cookie)
  (when-let ((desc (cdr (assoc cookie orb-db-filewatch--pending-move-alist))))
    (setq orb-db-filewatch--pending-move-alist
          (assoc-delete-all cookie orb-db-filewatch--pending-move-alist))
    desc))

(defun orb-db-filewatch--get-move-cookie (desc)
  (when-let ((cookie
              (car (rassoc desc orb-db-filewatch--pending-move-alist))))
    (setq orb-db-filewatch--pending-move-alist
          (assoc-delete-all cookie orb-db-filewatch--pending-move-alist))
    cookie))

(defun orb-db-filewatch--get-desc (filename)
  (car (rassoc filename orb-db-filewatch--desc-alist)))

(defun orb-db-filewatch--inotify-callback (event)
  (let* ((desc (car event))
         (actions (cadr event))
         (filename
          (file-name-concat
           (file-name-directory (cdr (assoc desc orb-db-filewatch--desc-alist)))
           (file-name-nondirectory (caddr event))))
         (cookie (cadddr event)))
    (pcase-exhaustive actions
      ('(modify)
       (run-hook-with-args 'orb-db-filewatch-functions 'modified filename))
      ('(isdir create)
       (orb-db-filewatch--add-directory (file-name-as-directory filename)))
      ('(create)
       (orb-db-filewatch--add-file filename))
      ('(isdir delete-self)
       (orb-db-filewatch--remove-directory desc))
      ('(delete-self)
       (orb-db-filewatch--remove-file desc))
      ('(isdir move-self)
       (when (orb-db-filewatch--get-move-cookie desc)
         (orb-db-filewatch--remove-directory desc t)))
      ('(move-self)
       (when (orb-db-filewatch--get-move-cookie desc)
         (orb-db-filewatch--remove-file desc)))
      ('(isdir moved-from)
       (when-let ((desc (orb-db-filewatch--get-desc (file-name-as-directory filename))))
         (push (cons cookie desc) orb-db-filewatch--pending-move-alist)))
      ('(moved-from)
       (when-let ((desc (orb-db-filewatch--get-desc filename)))
         (push (cons cookie desc) orb-db-filewatch--pending-move-alist)))
      ('(isdir moved-to)
       (orb-db-filewatch--rename-directory (orb-db-filewatch--get-move-desc cookie) (file-name-as-directory filename)))
      ('(moved-to)
       (orb-db-filewatch--rename-file (orb-db-filewatch--get-move-desc cookie) filename)))))

(defun orb-db-filewatch--watch-file (filename)
  (inotify-add-watch
   filename
   '(delete-self modify move-self)
   'orb-db-filewatch--inotify-callback))

(defun orb-db-filewatch--watch-directory (filename)
  (inotify-add-watch
   filename
   '(create delete-self move-self move onlydir)
   'orb-db-filewatch--inotify-callback))

(defun orb-db-filewatch--add-file (filename)
  (when (orb-file-p filename)
    (let ((desc (orb-db-filewatch--watch-file filename)))
      (push (cons desc filename) orb-db-filewatch--desc-alist)
      (run-hook-with-args 'orb-db-filewatch-functions 'modified filename))))

(defun orb-db-filewatch--add-directory (dirname)
  (when (orb-file-p dirname)
    (let ((desc (orb-db-filewatch--watch-directory dirname)))
      (push (cons desc dirname) orb-db-filewatch--desc-alist)
      (pcase-dolist
          (`(,filename ,type . _)
           (directory-files-and-attributes dirname t orb--file-name-nondirectory-match-regexp t))
        (cond
         ((eq type 't)
          (orb-db-filewatch--add-directory (file-name-as-directory filename)))
         ((not (stringp type))
          (orb-db-filewatch--add-file filename)))))))

(defun orb-db-filewatch--rename-file (desc filename)
  (if desc
      (let ((pair (assoc desc orb-db-filewatch--desc-alist)))
        (if (not (orb-file-p filename))
            (orb-db-filewatch--remove-file desc)
          (run-hook-with-args 'orb-db-filewatch-functions 'renamed (cdr pair) filename)
          (setcdr pair filename)))
    (orb-db-filewatch--add-file filename)))

(defun orb-db-filewatch--rename-directory (desc filename)
  (if desc
      (let ((dirname (file-name-as-directory filename))
            (srcname (cdr (assoc desc orb-db-filewatch--desc-alist))))
        (if (not (orb-file-p dirname))
            (orb-db-filewatch--remove-directory desc t)
          (run-hook-with-args 'orb-db-filewatch-functions 'renamed srcname dirname)
          (setq orb-db-filewatch--desc-alist
                (mapcar
                 (lambda (pair)
                   (if (not (string-prefix-p srcname (cdr pair)))
                       pair
                     (cons
                      (car pair)
                      (file-name-concat
                       dirname
                       (string-remove-prefix srcname (cdr pair))))))
                 orb-db-filewatch--desc-alist))))
    (orb-db-filewatch--add-directory filename)))

(defun orb-db-filewatch--remove-file (desc)
  (when-let ((filename (cdr (assoc desc orb-db-filewatch--desc-alist))))
    (run-hook-with-args 'orb-db-filewatch-functions 'deleted filename)
    (setq orb-db-filewatch--desc-alist
          (assoc-delete-all desc orb-db-filewatch--desc-alist))
    (inotify-rm-watch desc)))

(defun orb-db-filewatch--remove-directory (desc &optional recursive)
  (cond
   (recursive
    (let ((dirname (cdr (assoc desc orb-db-filewatch--desc-alist))))
      (pcase-dolist (`(,desc . ,filename) orb-db-filewatch--desc-alist)
        (when (string-prefix-p dirname filename)
          (inotify-rm-watch desc)))
      (setq
       orb-db-filewatch--desc-alist
       (seq-remove
        (lambda (pair)
          (string-prefix-p dirname (cdr pair)))
        orb-db-filewatch--desc-alist))))
   (t
    (setq orb-db-filewatch--desc-alist
          (assoc-delete-all desc orb-db-filewatch--desc-alist))
    (inotify-rm-watch desc))))

(defun orb-db-filewatch--remove-all ()
  (pcase-dolist (`(,desc . ,_) orb-db-filewatch--desc-alist)
    (inotify-rm-watch desc)
    (setq orb-db-filewatch--desc-alist nil)))


(defun orb-db-filewatch--update-db (event abs-path &optional abs-path1)
  (pcase-exhaustive event
    ('modified
     (orb-db-update-file abs-path))
    ('deleted
     (orb-db-delete-file abs-path))
    ('renamed
     (orb-db-rename-file "main" abs-path abs-path1))))

(defun orb-db-filewatch--db-sync-a ()
  (message "orb-db-filewatch-mode already enabled. Do nothing."))

;;;###autoload
(define-minor-mode orb-db-filewatch-mode
  "Global minor mode"
  :group 'orb
  :lighter " orb"
  :global t
  :init-value nil
  (cond
   (orb-db-filewatch-mode
    (orb-db-filewatch--add-directory (file-name-as-directory orb-directory))
    (add-hook 'orb-db-filewatch-functions 'orb-db-filewatch--update-db)
    (orb-db-sync-files
     (seq-remove
      'directory-name-p
      (mapcar 'cdr orb-db-filewatch--desc-alist)))
    (advice-add 'orb-db-sync :override 'orb-db-filewatch--db-sync-a))
   (t
    (advice-remove 'orb-db-sync 'orb-db-filewatch--db-sync-a)
    (remove-hook 'orb-db-filewatch-functions 'orb-db-filewatch--update-db)
    (orb-db-filewatch--remove-all))))

(provide 'orb-db-filewatch)
;;; orb-db-filewatch.el ends here
