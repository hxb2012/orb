;;; orb-asset.el --- 文件  -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Code:

(defgroup orb-asset nil
  "Orb-Asset"
  :tag "Orb-Asset"
  :group 'org-asset)

(defcustom orb-asset-directory
  (let* ((directory (expand-file-name org-directory))
         (parent (file-name-parent-directory directory))
         (base (file-name-nondirectory (directory-file-name directory))))
    (file-name-concat
     parent
     (cond ((equal (downcase base) base) "asset/")
           ((equal (upcase base) base) "ASSET/")
           (t "Asset/"))))
  "Default path to asset files.
Should be absolute path"
  :type 'directory
  :group 'orb-asset)

(defcustom orb-asset-external-directories nil
  "orb-asset external storage"
  :type '(alist :key-type string :value-type directory)
  :group 'orb-asset)

(defun orb-asset--get-id (file-name)
  (when-let ((directory
              (seq-find (apply-partially 'file-in-directory-p file-name)
                        (cons
                         orb-asset-directory
                         (mapcar 'cdr orb-asset-external-directories)))))
    (apply 'concat
           "asset:"
           (file-name-split (file-relative-name file-name directory)))))

(defun orb-asset--shell-command-to-string (command &rest args)
  (let ((buffer (generate-new-buffer " *string-output*" t)))
    (unwind-protect
        (with-current-buffer buffer
          (if (zerop (apply 'call-process command nil t nil args))
              (buffer-string)
            (message "%s" (buffer-string))
            nil))
      (kill-buffer buffer))))

(defun orb-asset--id (digest ext)
  (concat "asset:" digest ext))

(defun orb-asset--path (digest ext)
  (concat
   (file-name-concat
    (substring digest 0 2)
    (substring digest 2 4)
    (substring digest 4))
   ext))

(defun orb-asset--digest (path)
  (when-let ((s (orb-asset--shell-command-to-string "sha256sum" path)))
    (car (split-string s))))

(defun orb-asset--validate (path digest)
  (and (file-exists-p path)
       (equal (orb-asset--digest path) digest)))

(defconst orb-asset--make-copy-program
  (file-name-concat (file-name-directory load-file-name) "make-copy.py"))

(defun orb-asset--copy (src dst_dir path)
  (unless (orb-asset--shell-command-to-string
           orb-asset--make-copy-program src dst_dir path)
    (user-error "Failed to copy %S to %S" src dst_dir)))

(provide 'orb-asset)
;;; orb-asset.el ends here
