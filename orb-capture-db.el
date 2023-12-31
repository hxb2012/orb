;;; orb-capture-db.el --- 通过SQLite查询Capture -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; 最初是从org-capture.el里抄来的

;;; Commentary:

;;; Code:

(defun orb-capture-db--add-agenda-files (files)
  (orb-db-refresh)
  (orb-db-with-transaction
    (let ((result
           (orb-db-query
            (list "SELECT (? || file.path)
FROM %s.file AS file
WHERE EXISTS (
  SELECT 1
  FROM %s.node AS node
  WHERE node.file_id = file.file_id
  AND (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'CAPTURE_TODO'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) IS NOT NULL)"
                  orb-directory)
            t)))
      (seq-uniq (append files (mapcar 'car result))))))

(defun orb-capture-db--agenda-files-a (fun &optional unrestricted archives)
  (let ((org-agenda-files
         (cond
          ((and (not unrestricted) (get 'org-agenda-files 'org-restrict))
           org-agenda-files)
          ((stringp org-agenda-files)
           (orb-capture-db--add-agenda-files (org-read-agenda-file-list)))
          ((listp org-agenda-files)
           (orb-capture-db--add-agenda-files org-agenda-files))
          (t org-agenda-files))))
    (funcall fun unrestricted archives)))


(defun orb-capture-db--get-targets-a (fun key)
  (orb-db-refresh)
  (orb-db-with-transaction
    (let ((result
           (orb-db-query
            (list "SELECT
  file.path,
  json_extract(file.keywords, '$.TITLE'),
  (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = ('CAPTURE_' || ?)
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property),
  coalesce(
    (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = ('CAPTURE_' || ? || '_TARGET')
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property),
    (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'CAPTURE_TARGET'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property)),
  coalesce(
    (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = ('CAPTURE_' || ? || '_TITLE')
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property),
    (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'CAPTURE_TITLE'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property)),
  (
WITH RECURSIVE
  property(parent_id, level, value) AS NOT MATERIALIZED (
    SELECT node.parent_id, node.level, node.heading
    UNION ALL
    SELECT parent.parent_id, parent.level, parent.heading
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id)
SELECT json_group_array(value)
FROM (
  SELECT value
  FROM property
  WHERE property.level > 0
  ORDER BY property.level ASC)) AS outline_path
FROM %s.file AS file
JOIN %s.node AS node
ON file.file_id = node.file_id
WHERE (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = ('CAPTURE_' || ?)
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ')
FROM property) IS NOT NULL"
                  (upcase key)
                  (upcase key)
                  (upcase key)
                  (upcase key))
            t)))
      (seq-uniq
       (append
        (funcall fun key)
        (mapcar
         (lambda (row)
           (pcase-let*
               ((`(,path ,file-title ,key ,target ,title ,olp) row)
                (file-name (file-name-concat orb-directory path))
                (outline-path (cons file-title
                                    (json-parse-string olp
                                                       :array-type 'list
                                                       :null-object nil)))
                (file (when target (expand-file-name target (file-name-directory file-name)))))
             (cons key
                   (cons
                    (string-join
                     (if title (cons title outline-path) outline-path)
                     "·")
                    (cons (or file file-name)
                          (unless file (cdr outline-path)))))))
         result))))))

;;;###autoload
(define-minor-mode orb-capture-db-mode
  ""
  :group 'orb-capture
  :global t
  :init-value nil
  (cond
   (orb-capture-db-mode
    (advice-add 'org-agenda-files :around 'orb-capture-db--agenda-files-a)
    (advice-add 'orb-capture--get-targets
                :around 'orb-capture-db--get-targets-a))
   (t
    (advice-remove 'orb-capture--get-targets 'orb-capture-db--get-targets-a)
    (advice-remove 'org-agenda-files 'orb-capture-db--agenda-files-a))))

(provide 'orb-capture-db)
;;; orb-capture-db.el ends here
