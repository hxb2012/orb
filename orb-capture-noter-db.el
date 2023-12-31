;;; orb-capture-noter-db.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

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

;; 最初是从org-capture.el里抄来的

;;; Commentary:

(defvar orb-noter-property-doc)

(defun orb-capture-noter-db--goto (path)
  (let* ((levels (orb-db--file-name-levels orb-directory))
         (result
          (orb-db-query
           (list "SELECT (? || file.path), node.point
FROM %s.link AS link
JOIN %s.file AS file
ON file.file_id = link.file_id
JOIN %s.node AS node
ON file.file_id = node.file_id
JOIN %s.property AS prop
ON prop.node_id = node.node_id
AND prop.key = ?
AND link.begin >= prop.begin
AND link.end <= prop.end
WHERE link.type = 'file'
AND coalesce(link.application, '') != 'sys'
AND link.search_option IS NULL
AND ? = (
  CASE
  WHEN link.relative IS NULL THEN
    link.path
  WHEN link.relative < coalesce(json_array_length(file.levels), 0) THEN
    ? || substr(file.path, 1, json_extract(file.levels, '$[' || link.relative || ']')) || substr(link.path, 1+3*link.relative)
  WHEN link.relative - coalesce(json_array_length(file.levels),0) < ? THEN
    substr(?, 1, json_extract(?, '$[' || (link.relative - coalesce(json_array_length(file.levels), 0)) || ']')) || substr(link.path, 1+3*link.relative)
  END)
LIMIT 1"
                 orb-directory
                 orb-noter-property-doc
                 path
                 orb-directory
                 (length levels)
                 orb-directory
                 (when levels (json-encode levels))))))
    (when result
      (pcase-let* ((`(,file ,pos) (car result))
                   (buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))))
        (org-goto-marker-or-bmk
         (with-current-buffer buffer
           (org-with-wide-buffer
            (copy-marker pos))))
        t))))

;;;###autoload
(define-minor-mode orb-capture-noter-db-mode
  ""
  :group 'orb-capture
  :global t
  :init-value nil
  (cond
   (orb-capture-noter-db-mode
    (advice-add 'orb-capture-noter--goto :override 'orb-capture-noter-db--goto))
   (t
    (advice-remove 'orb-capture-noter--goto 'orb-capture-noter-db--goto))))

(provide 'orb-capture-noter-db)
;;; orb-capture-noter-db.el ends here
