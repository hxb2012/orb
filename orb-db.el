;;; orb-db.el --- SQLite操作 -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Commentary:

;; 最初是从org-roam-db.el里抄来的

;;; Code:

(require 'org)
(require 'org-habit)
(require 'sqlite)
(require 'orb)

(defcustom orb-db-location nil
  "The path to file where the Orb database is stored."
  :type 'string
  :group 'orb)

(defconst orb-db--user-version 1)

(defvar orb-db--connection nil)

;;;###autoload
(defun orb-db ()
  "Entrypoint to the Orb sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless orb-db--connection
    (when (and orb-db-location
               (file-exists-p orb-db-location)
               (let ((conn (sqlite-open orb-db-location)))
                 (unwind-protect
                     (let ((result
                            (with-sqlite-transaction conn
                              (sqlite-select
                               conn
                               "select user_version from pragma_user_version"))))
                       (/= orb-db--user-version (caar result)))
                   (sqlite-close conn))))
      (delete-file orb-db-location))
    (let ((conn (sqlite-open orb-db-location)))
      (orb-db--init conn)
      (setq orb-db--connection conn)))
  orb-db--connection)

;;;###autoload
(defmacro orb-db-with-transaction (&rest body)
  "Execute BODY in transaction for orb-db"
  (declare (indent 0) (debug (form body)))
  (let ((db-var (gensym))
        (transaction-p-var (gensym))
        (error-var (gensym)))
    `(let* ((,db-var (orb-db))
            (,error-var t)
            (,transaction-p-var (sqlite-transaction ,db-var)))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,error-var nil))
         (if ,error-var
             (when ,transaction-p-var
               (sqlite-rollback ,db-var))
           (when ,transaction-p-var
             (sqlite-commit ,db-var)))))))

;;;###autoload
(defun orb-db-select (query &optional values return-type)
  "Execute SQLite select from orb-db"
  (let ((result (sqlite-select (orb-db) query values return-type)))
    result))

;;;###autoload
(defun orb-db-query (query &optional with-temp)
  (let ((sql
         (if with-temp
             (concat
              (format-spec (car query) '((?s . "temp")))
              " UNION "
              (format-spec (car query) '((?s . "main")))
              "
AND NOT EXISTS (
  SELECT 1
  FROM temp.file AS tempfile
  WHERE tempfile.path = file.path)")
           (format-spec (car query) '((?s . "main")))))
        (params
         (if with-temp
             (append (cdr query) (cdr query))
           (cdr query))))
    (orb-db-select sql params)))

;;;###autoload
(defun orb-db-concat-query (&rest list)
  (cons
   (apply 'concat (mapcar (lambda (x) (if (stringp x) x (car x))) list))
   (apply 'append (mapcar (lambda (x) (unless (stringp x) (cdr x))) list))))

;;;###autoload
(defun orb-db-tick ()
  (car (orb-db-select "SELECT total_changes(), data_version FROM pragma_data_version")))

(defun orb-db--init (conn)
  (sqlite-pragma conn "foreign_keys = true")

  (with-sqlite-transaction conn
    (sqlite-execute
     conn
     "
CREATE TABLE IF NOT EXISTS file (
  file_id INTEGER PRIMARY KEY AUTOINCREMENT,
  path TEXT UNIQUE,
  levels JSON,
  hash TEXT NOT NULL,
  mtime INTEGER NOT NULL,
  keywords JSON,
  tags JSON,
  todo_keywords JSON,
  done_keywords JSON
)")

    (sqlite-execute
     conn
     "
CREATE TEMP TABLE IF NOT EXISTS file (
  file_id INTEGER PRIMARY KEY AUTOINCREMENT,
  path TEXT UNIQUE,
  levels JSON,
  modified_tick INTEGER,
  keywords JSON,
  tags JSON,
  todo_keywords JSON,
  done_keywords JSON
)")

    (dolist (database '("" "TEMP "))
      (sqlite-execute
       conn
       (format "
CREATE %sTABLE IF NOT EXISTS node (
  node_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER REFERENCES file(file_id) ON DELETE CASCADE,
  point INTEGER NOT NULL,
  parent_id INTEGER REFERENCES node(node_id),
  level INTEGER NOT NULL,
  heading TEXT,
  text TEXT,
  todo TEXT,
  tags JSON,
  properties JSON,
  scheduled INTEGER,
  deadline INTEGER,
  closed INTEGER,
  timestamp INTEGER,
  timestamp_ia INTEGER,
  blocked INTEGER,
  habit JSON
)" database))

      (sqlite-execute
       conn
       (format "
CREATE %sTABLE IF NOT EXISTS link (
  file_id INTEGER REFERENCES file(file_id) ON DELETE CASCADE,
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  property TEXT,
  begin INTEGER NOT NULL,
  end INTEGER NOT NULL,
  type TEXT NOT NULL,
  path TEXT NOT NULL,
  relative INTEGER,
  search_option TEXT,
  application TEXT
)" database))

      (sqlite-execute
       conn
       (format "
CREATE %sTABLE IF NOT EXISTS timestamp (
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  begin INTEGER NOT NULL,
  end INTEGER NOT NULL,
  start_time TEXT NOT NULL,
  start_epoch REAL,
  end_time TEXT,
  end_epoch REAL,
  at_range BOOL
)" database))

      (sqlite-execute
       conn
       (format "
CREATE %sTABLE IF NOT EXISTS sexp (
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  point INTEGER NOT NULL,
  sexp TEXT NOT NULL,
  entry TEXT NOT NULL
)" database))

      (sqlite-execute
       conn
       (format "
CREATE %sTABLE IF NOT EXISTS progress (
  node_id INTEGER REFERENCES node(node_id) ON DELETE CASCADE,
  point INTEGER NOT NULL,
  str TEXT,
  ts INTEGER NOT NULL,
  extra TEXT
)" database))))

  (sqlite-pragma conn (format "user_version = %s" orb-db--user-version)))


;;;###autoload
(defun orb-db-clear-all ()
  (interactive)
  (when orb-db--connection
    (sqlite-close orb-db--connection)
    (when orb-db-location
      (delete-file orb-db-location))
    (setq orb-db--connection nil)))


(defun orb-db--insert-file ()
  (when-let
      ((result
        (sqlite-select
         (orb-db)
         "
INSERT INTO main.file(hash, mtime, path, levels, keywords, tags, todo_keywords, done_keywords)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT
DO UPDATE
SET hash = EXCLUDED.hash
  , mtime = EXCLUDED.mtime
  , keywords = EXCLUDED.keywords
WHERE hash != EXCLUDED.hash
RETURNING file_id"
         (append
          (list
           (orb--file-hash buffer-file-name)
           (car
            (time-convert
             (file-attribute-modification-time (file-attributes buffer-file-name))
             1000000000)))
          (orb--collect-buffer)))))
    (caar result)))

(defun orb-db--insert-buffer ()
  (let ((changes
         (sqlite-execute
          (orb-db)
          "
INSERT INTO temp.file(modified_tick, path, levels, keywords, tags, todo_keywords, done_keywords)
VALUES (?, ?, ?, ?, ?, ?, ?)
ON CONFLICT
DO UPDATE
SET modified_tick = EXCLUDED.modified_tick
  , keywords = EXCLUDED.keywords
  , tags = EXCLUDED.tags
  , todo_keywords = EXCLUDED.todo_keywords
  , done_keywords = EXCLUDED.done_keywords
WHERE modified_tick < EXCLUDED.modified_tick
"
          (cons
           (buffer-chars-modified-tick)
           (orb--collect-buffer)))))
    ;; workaround temp table returning nil
    (when (> changes 0)
      (let ((result
             (sqlite-select
              (orb-db)
              "SELECT file_id FROM temp.file WHERE path = ?"
              (list (file-relative-name buffer-file-name orb-directory)))))
        (caar result)))))

(defun orb-db--insert-node (conn database file-id pom level heading text todo tags props scheduled deadline closed blocked habit)
  (sqlite-execute
   conn
   (format "
INSERT INTO %s.node(file_id, point, parent_id, level, heading, text, todo, tags, properties, scheduled, deadline, closed, blocked, habit)
VALUES (?, ?,
(SELECT node_id
FROM %s.node
WHERE file_id = ?
AND point < ?
AND level < ?
ORDER BY point DESC
LIMIT 1),
?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" database database)
   (list
    file-id
    pom
    file-id
    pom
    level
    level
    heading
    text
    todo
    (when tags (json-encode tags))
    (when props (json-encode props))
    scheduled
    deadline
    closed
    blocked
    (when habit (json-encode habit)))))

(defun orb-db--insert-link (conn database file-id begin end type path search-option application relative prop)
  (sqlite-execute
   conn
   (format "
INSERT INTO %s.link(file_id, node_id, property, begin, end, type, path, search_option, application, relative)
SELECT file_id, node_id, ?, ?, ?, ?, ?, ?, ?, ?
FROM %s.node
WHERE file_id = ?
AND point <= ?
ORDER BY point DESC
LIMIT 1
" database database)
     (list
      prop
      begin
      end
      type
      path
      search-option
      application
      relative
      file-id
      begin)))

(defun orb-db--insert-timestamp (conn database file-id begin end start_time start_epoch &optional at_range end_time end_epoch)
  (sqlite-execute
   conn
   (format "
INSERT INTO %s.timestamp(node_id, begin, end, start_time, start_epoch, at_range, end_time, end_epoch)
SELECT node_id, ?, ?, ?, ?, ?, ?, ?
FROM %s.node
WHERE file_id = ?
AND point <= ?
ORDER BY point DESC
LIMIT 1
" database database)
     (list begin end start_time start_epoch at_range end_time end_epoch file-id begin)))

(defun orb-db--insert-sexp (conn database file-id pos sexp entry)
  (sqlite-execute
   conn
   (format "
INSERT INTO %s.sexp(node_id, point, sexp, entry)
SELECT node_id, ?, ?, ?
FROM %s.node
WHERE file_id = ?
AND point <= ?
ORDER BY point DESC
LIMIT 1
" database database)
   (list pos sexp entry file-id pos)))

(defun orb-db--insert-progress (conn database file-id pos str ts extra)
  (sqlite-execute
   conn
   (format "
INSERT INTO %s.progress(node_id, point, str, ts, extra)
SELECT node_id, ?, ?, ?, ?
FROM %s.node
WHERE file_id = ?
AND point <= ?
ORDER BY point DESC
LIMIT 1
" database database)
   (list pos str ts extra file-id pos)))

(defun orb-db--fix-timestamps (conn database file-id)
  (sqlite-execute
   conn
   (format "
UPDATE %s.node AS node
SET
timestamp = (
  SELECT min(timestamp.begin)
  FROM %s.timestamp AS timestamp
  WHERE timestamp.node_id = node.node_id
  AND coalesce(timestamp.begin != node.scheduled, TRUE)
  AND coalesce(timestamp.begin != node.deadline, TRUE)
  AND coalesce(timestamp.begin != node.closed, TRUE)
  AND substr(timestamp.start_time, 1, 1) = '<'),
timestamp_ia = (
  SELECT min(timestamp_ia.begin)
  FROM %s.timestamp AS timestamp_ia
  WHERE timestamp_ia.node_id = node.node_id
  AND coalesce(timestamp_ia.begin != node.scheduled, TRUE)
  AND coalesce(timestamp_ia.begin != node.deadline, TRUE)
  AND coalesce(timestamp_ia.begin != node.closed, TRUE)
  AND substr(timestamp_ia.start_time, 1, 1) = '[')
FROM %s.file AS file
WHERE file.file_id = node.file_id
AND file.file_id = ?
" database database database database)
   (list file-id)))

(defun orb-db--update-file-content (database file-id)
  (let ((conn (orb-db)))
    (sqlite-execute
     conn
     (format "DELETE FROM %s.node WHERE file_id = ?" database)
     (list file-id))
    (orb--map-nodes
     (apply-partially 'orb-db--insert-node conn database file-id))
    (orb--map-links
     (apply-partially 'orb-db--insert-link conn database file-id))
    (orb--map-blocks
     (apply-partially 'orb-db--insert-timestamp conn database file-id))
    (orb--map-timestamps
     (apply-partially 'orb-db--insert-timestamp conn database file-id))
    (orb--map-sexps
     (apply-partially 'orb-db--insert-sexp conn database file-id))
    (orb--map-progress
     (apply-partially 'orb-db--insert-progress conn database file-id))
    (orb-db--fix-timestamps conn database file-id)))

(defun orb-db--remove-files (database remaining-abs-path-list)
  (sqlite-execute
   (orb-db)
   (format "DELETE FROM %s.file WHERE path IN (SELECT value FROM json_each(?))" database)
   (list
    (when remaining-abs-path-list
      (json-encode
       (let ((default-directory orb-directory))
         (mapcar 'file-relative-name remaining-abs-path-list)))))))

(defun orb-db--remove-deleted-files (database remaining-abs-path-list)
  (sqlite-execute
   (orb-db)
   (format "DELETE FROM %s.file WHERE path NOT IN (SELECT value FROM json_each(?))" database)
   (list
    (when remaining-abs-path-list
      (json-encode
       (let ((default-directory orb-directory))
         (mapcar 'file-relative-name remaining-abs-path-list)))))))


(defun orb-db--update-file (abs-path)
  "Update orb cache for ABS-PATH."
  (with-temp-buffer
    (let ((buffer-file-name abs-path)
          (default-directory (file-name-directory abs-path))
          (org-element-use-cache nil))
      (insert-file-contents abs-path)
      (delay-mode-hooks (org-mode))
      (org-with-wide-buffer
       (when-let ((file-id (orb-db--insert-file)))
         (orb-db--update-file-content "main" file-id))))))

(defun orb-db--delete-file (abs-path)
  (let* ((delete-path (file-relative-name abs-path orb-directory))
         (delete-length (length delete-path)))
    (sqlite-execute
     (orb-db)
     "DELETE FROM main.file WHERE substr(path, 1, ?) = ?"
     (list delete-length delete-path))))

(defun orb-db--rename-file (database old-abs-path new-abs-path)
  (let* ((old-path (file-relative-name old-abs-path orb-directory))
         (new-path (file-relative-name new-abs-path orb-directory))
         (old-length (length old-path))
         (new-length (length new-path))
         (old-levels (orb--file-name-levels old-path))
         (new-levels (orb--file-name-levels new-path))
         (old-levels-length (length old-levels)))
    (sqlite-execute
     (orb-db)
     (format "
UPDATE %s.file
SET
  path = ? || substr(path, ? + 1),
  levels = (
    WITH new_levels AS (
      SELECT old_levels.key AS key, old_levels.value + ? AS value
      FROM json_each(levels) AS old_levels
      WHERE old_levels.key + ? < json_array_length(levels)
      UNION
      SELECT key + json_array_length(levels) - ? AS key, value
      FROM json_each(?))
    SELECT json_group_array(value)
    FROM new_levels
    ORDER BY key ASC)
WHERE substr(path, 1, ?) = ?"
             database)
     (list
      new-path old-length
      (- new-length old-length)
      old-levels-length
      old-levels-length
      (when new-levels (json-encode new-levels))
      old-length old-path))))

;;;###autoload
(defun orb-db-add-file (abs-path)
  (orb-db-with-transaction
    (mapc
     (apply-partially 'orb-db--update-file)
     (if (directory-name-p abs-path)
         (orb-list-files abs-path)
       `(,abs-path)))))

;;;###autoload
(defun orb-db-update-file (&optional abs-path)
  (let ((abs-path (or abs-path buffer-file-name)))
    (orb-db-with-transaction
      (orb-db--update-file abs-path))))

;;;###autoload
(defun orb-db-delete-file (abs-path)
  (orb-db-with-transaction
    (orb-db--delete-file abs-path)))

;;;###autoload
(defun orb-db-rename-file (database old-abs-path new-abs-path)
  (orb-db-with-transaction
    (orb-db--rename-file database old-abs-path new-abs-path)))

;;;###autoload
(defun orb-db-refresh (&optional file-list)
  (interactive)
  (let* ((abs-path-list
          (seq-filter
           'orb-file-p
           (seq-uniq (mapcar 'expand-file-name file-list))))
         (buffer-list
          (if file-list
              (seq-uniq (remq nil (mapcar 'org-find-base-buffer-visiting abs-path-list)))
            (orb-buffer-list)))
         (buffer-file-list
          (mapcar
           'buffer-file-name
           (seq-filter
            (lambda (buffer)
              (and (buffer-modified-p buffer)
                   (local-variable-p 'orb-db--buffer-visited buffer)))
            buffer-list))))
    (orb-db-with-transaction
      (if file-list
          (orb-db--remove-files "temp" (seq-difference abs-path-list buffer-file-list))
        (orb-db--remove-deleted-files "temp" buffer-file-list))
      (dolist (buffer buffer-list)
        (when (buffer-modified-p buffer)
          (with-current-buffer buffer
            (make-local-variable 'orb-db--buffer-visited)
            (org-with-wide-buffer
             (when-let ((file-id (orb-db--insert-buffer)))
               (orb-db--update-file-content "temp" file-id)))))))))

;;;###autoload
(defun orb-db-sync-files (files)
  (orb-db-with-transaction
    (orb-db--remove-deleted-files "main" files)
    (mapc (apply-partially 'orb-db--update-file) files)))

;;;###autoload
(defun orb-db-sync ()
  "Synchronize the cache state with the current Org files on-disk."
  (interactive)
  (orb-db-sync-files (orb-list-files)))

(provide 'orb-db)
;;; orb-db.el ends here
