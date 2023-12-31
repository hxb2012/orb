;;; orb-id.el --- 改善org-id -*- coding: utf-8; lexical-binding: t; -*-

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

;; 最初是从org-roam-id.el, org-roam-overlay.el里抄来的

;;; Commentary:

;;; Code:

(defun orb-id--find-id-in-file-a (id file &optional markerp)
  (unless (or markerp
              (find-buffer-visiting file)
              (not (orb-file-p file)))
    (let ((result
           (orb-db-with-transaction
             (orb-db-select
              "
SELECT file.path, node.point
FROM main.file AS file
JOIN main.node AS node
ON file.file_id = node.file_id
WHERE file.path = ?
AND (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM main.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'ID'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) = ?"
              (list (file-relative-name file orb-directory) id)))))
      (when (> (length result) 1)
        (user-error "Duplicate ID %S" id))
      (when result
        (list
         (expand-file-name (caar result) orb-directory)
         (cdar result))))))

(defun orb-id--find-a (id &optional markerp)
  (orb-db-refresh)
  (unless markerp
    (when-let
        ((result
          (orb-db-with-transaction
            (orb-db-select
             "
SELECT file.path, node.point
FROM temp.file AS file
JOIN temp.node AS node
ON file.file_id = node.file_id
WHERE (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM temp.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'ID'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) = ?
UNION
SELECT file.path, node.point
FROM main.file AS file
JOIN main.node AS node
ON file.file_id = node.file_id
WHERE NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)
AND (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM main.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'ID'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) = ?
LIMIT 2
"
             (list id id)))))
      (unless (eq (cdr result) nil)
        (user-error "Duplicate ID %S" id))
      (list
       (expand-file-name (caar result) orb-directory)
       (cdar result)))))

(defun orb-id--find-id-file-a (id)
  (when-let ((pos (orb-id--find-a id)))
    (car pos)))

(defun orb-id--update-id-locations-a (&optional _files _silent))

(defun orb-id-get-title (id)
  (when-let
      ((result
        (orb-db-with-transaction
          (orb-db-select
           "
SELECT CASE WHEN node.level = 0 THEN json_extract(file.keywords, '$.TITLE') ELSE node.heading END
FROM temp.file AS file
JOIN temp.node AS node
ON file.file_id = node.file_id
WHERE (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM temp.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'ID'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) = ?
UNION
SELECT CASE WHEN node.level = 0 THEN json_extract(file.keywords, '$.TITLE') ELSE node.heading END
FROM main.file AS file
JOIN main.node AS node
ON file.file_id = node.file_id
WHERE NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)
AND (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM main.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'ID'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) = ?
LIMIT 2
"
           (list id id)))))
    (if (eq (cdr result) nil)
        (caar result)
      (message "Duplicate ID %S" id)
      nil)))

(defun orb-id--overlay-p (o)
  (eq (overlay-get o 'category) 'orb-id))

(defun orb-id--delete-overlay-h (overlay _after _start _end &optional _length)
  (delete-overlay overlay))

(defun orb-id-link-activate (start end path bracket)
  (let ((overlays (seq-filter 'orb-id--overlay-p (overlays-in start end))))
    (if (and bracket
             (text-property-any (+ start 2) (- end 2) 'invisible 'org-link))
        (mapc 'delete-overlay overlays)
      (dolist (overlay overlays)
        (unless
            (and overlay
                 (eq (overlay-start overlay) start)
                 (eq (overlay-end overlay) end))
          (delete-overlay overlay)))
      (unless (seq-filter 'overlay-buffer overlays)
        (when orb-db-smartrefresh-mode
          (orb-db-refresh))
        (when-let ((title (orb-id-get-title path)))
          (let ((overlay (make-overlay start end nil t)))
            (overlay-put overlay 'category 'orb-id)
            (overlay-put overlay 'intangible t)
            (overlay-put overlay 'evaporate t)
            (overlay-put
             overlay 'before-string
             (propertize (format "[[id:%s][" path) 'face 'org-link 'invisible 'org-link))
            (overlay-put overlay 'display title)
            (overlay-put
             overlay 'after-string
             (propertize "]]" 'face 'org-link 'invisible 'org-link))
            (overlay-put overlay 'modification-hooks '(orb-id--delete-overlay-h))))))))

;;;###autoload
(define-minor-mode orb-id-mode
  "Global minor mode to tweak `org-id' to query from cache in Orb."
  :group 'orb
  :global t
  :init-value nil
  (cond
   (orb-id-mode
    (advice-add 'org-id-find-id-file :before-until 'orb-id--find-id-file-a)
    (advice-add 'org-id-find-id-in-file :before-until 'orb-id--find-id-in-file-a)
    (advice-add 'org-id-find :before-until 'orb-id--find-a)
    (advice-add 'org-id-update-id-locations :override 'orb-id--update-id-locations-a)
    (org-link-set-parameters "id" :activate-func 'orb-id-link-activate))
   (t
    (org-link-set-parameters "id" :activate-func nil)
    (advice-remove 'org-id-update-id-locations 'orb-id--update-id-locations-a)
    (advice-remove 'org-id-find 'orb-id--find-a)
    (advice-remove 'org-id-find-id-in-file 'orb-id--find-id-in-file-a)
    (advice-remove 'org-id-find-id-file 'orb-id--find-id-file-a))))

(provide 'orb-id)
;;; orb-id.el ends here
