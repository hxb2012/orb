;;; orb-agenda.el --- 用Orb加速Org-agenda -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; 最初是从org-agenda.el里抄来的

;;; Commentary:

;;; Code:

(require 'org-agenda)
(require 'orb)

;;;###autoload(autoload 'org-clock-load "org-clock")
;;; ;;;###autoload(autoload 'org-clock-special-range "org-clock")
(declare-function org-clock-special-range "org-clock"
                  (key &optional time as-strings wstart mstart))

;;;###autoload(autoload 'org-habit-insert-consistency-graphs "org-habit")
(declare-function org-habit-insert-consistency-graphs "org-habit" (&optional line))
;;;###autoload(autoload 'org-habit-parse-todo "org-habit")
(declare-function org-habit-parse-todo "org-habit" (&optional pom))
(declare-function org-habit-get-priority "org-habit" (habit &optional moment))

(declare-function org-agenda-colview-summarize "org-colview" (cache))
(declare-function org-columns--display-here "org-colview" (columns &optional dateline))
(declare-function org-columns--display-here-title "org-colview" ())
(declare-function org-columns--set-widths "org-colview" (cache))
(declare-function org-columns--collect-values "org-colview" (&optional compiled-fmt))
(declare-function org-agenda-colview-compute "org-colview" (fmt))
(declare-function org-columns-compile-format "org-colview" (fmt))
(declare-function org-columns--displayed-value "org-colview" (spec value &optional no-star))

(defvar org-depend-tag-blocked) ;; defined in org-agenda.el
(defvar org-agenda-show-log-scoped) ;; defined in org-agenda.elxo
(defvar org-columns--time) ;; defined in org-colview.el
(defvar org-columns-begin-marker) ;; defined in org-colview.el
(defvar org-columns-current-fmt) ;; defined in org-colview.el
(defvar org-columns-current-fmt-compiled) ; defined in org-colview.el
(defvar org-habit-scheduled-past-days) ; defined in org-habit.el

;;;###autoload
(cl-defstruct orb-agenda--file
  file-id path keywords tags todo-keywords done-keywords columns)

(defvar orb-agenda--current-file nil)

(defun orb-agenda--get-file (path)
  (when-let
      ((result
        (orb-db-query
         (list
          "
SELECT file.file_id, file.keywords, file.tags, file.todo_keywords, file.done_keywords,
  (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = 'COLUMNS'
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ') FROM property)
FROM %s.file AS file
LEFT JOIN %s.node AS node
ON file.file_id = node.file_id
AND node.point = 1
WHERE path = ?"
          (file-relative-name path orb-directory)))))
    (pcase-let ((`(,file-id ,keywords ,tags ,todo-keywords ,done-keywords ,columns) (car result)))
      (make-orb-agenda--file
       :file-id file-id
       :path path
       :keywords (json-parse-string (or keywords "{}"))
       :tags (json-parse-string (or tags "[]") :array-type 'list)
       :todo-keywords (json-parse-string todo-keywords :array-type 'list)
       :done-keywords (json-parse-string done-keywords :array-type 'list)
       :columns columns))))


;;;###autoload
(cl-defstruct orb-agenda--node
  node-id file point level heading text olp todo tags alltags props props-literal-nil allprops allprops-literal-nil scheduled deadline closed timestamp timestamp-ia blocked habit summaries)

(defvar orb-agenda--skip-function nil)
(defvar orb-agenda--skip-function-global nil)

(defun orb-agenda--make-node (file node-id point level heading text olp todo tags alltags props props-literal-nil allprops allprops-literal-nil blocked habit &optional scheduled deadline closed timestamp timestamp-ia)
  (make-orb-agenda--node
   :node-id node-id
   :file file
   :point point
   :level level
   :heading heading
   :text text
   :olp (json-parse-string (or olp "[]") :array-type 'list :null-object nil)
   :todo todo
   :tags (json-parse-string (or tags "[]") :array-type 'list :null-object nil)
   :alltags (json-parse-string (or alltags "[]") :array-type 'list :null-object nil)
   :props (json-parse-string (or props "{}") :array-type 'list :null-object nil)
   :props-literal-nil (json-parse-string (or props-literal-nil "{}") :array-type 'list :null-object nil)
   :allprops (json-parse-string (or allprops "{}") :array-type 'list :null-object nil)
   :allprops-literal-nil (json-parse-string (or allprops-literal-nil "{}") :array-type 'list :null-object nil)
   :blocked blocked
   :habit (json-parse-string (or habit "[]") :array-type 'list :null-object nil)
   :scheduled (when scheduled (json-parse-string scheduled :object-type 'plist :null-object nil))
   :deadline (when deadline (json-parse-string deadline :object-type 'plist :null-object nil))
   :closed (when closed (json-parse-string closed :object-type 'plist :null-object nil))
   :timestamp (when timestamp (json-parse-string timestamp :object-type 'plist :null-object nil))
   :timestamp-ia (when timestamp-ia (json-parse-string timestamp-ia :object-type 'plist :null-object nil))))

(defun orb-agenda--compare-time (table op &rest params)
  (if org-agenda-todo-ignore-time-comparison-use-seconds
      (cons
       (format " AND %s.start_epoch - ? %s" table op)
       (cons (float-time nil) params))
    (cons
     (format " AND (unixepoch(datetime(%s.start_epoch, 'unixepoch', 'localtime', 'start of day'), 'localtime') - unixepoch(?, 'localtime')) / 86400 %s" table op)
     (cons
      (orb-agenda--format-date (calendar-gregorian-from-absolute (time-to-days nil)))
      params))))

(defun orb-agenda--query (file condition &optional tables columns)
  (orb-db-query
   (orb-db-concat-query
    "SELECT "
    columns
    "
    node.node_id AS node_id,
    node.point AS point,
    node.level AS level,
    node.heading AS heading,
    coalesce(node.text, node.heading) AS text,
    (
WITH RECURSIVE
  property(parent_id, level, value) AS NOT MATERIALIZED (
    SELECT parent.parent_id, parent.level, parent.heading
    FROM %s.node AS parent
    WHERE parent.node_id = node.parent_id
    UNION ALL
    SELECT parent.parent_id, parent.level, parent.heading
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id),
  path(segment) AS MATERIALIZED (
    SELECT value
    FROM property
    ORDER BY property.level ASC)
SELECT json_group_array(segment) FROM path) AS outline_path,
    node.todo AS todo,
    node.tags AS tags,
    (
WITH RECURSIVE
  property(parent_id, level, value) AS NOT MATERIALIZED (
    SELECT node.parent_id, node.level, node.tags
    UNION ALL
    SELECT parent.parent_id, parent.level, parent.tags
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id),
  tag(value) AS MATERIALIZED (
    SELECT t.value
    FROM property,
         json_each(property.value) AS t
    WHERE property.value IS NOT NULL
    ORDER BY level ASC, t.key ASC)
SELECT json_group_array(value) FROM tag) AS alltags,
    (
WITH
  raw_property(key, value) AS MATERIALIZED (
    SELECT prop.key, prop.value
    FROM %s.property AS prop
    WHERE prop.node_id = node.node_id
    ORDER BY prop.key ASC, prop.extendp ASC, prop.begin ASC),
  property(key, value) AS NOT MATERIALIZED (
    SELECT key, group_concat(value, ' ')
    FROM raw_property
    GROUP BY key)
SELECT json_group_object(key, value)
FROM property
WHERE value != 'nil') AS props,
    (
WITH
  raw_property(key, value) AS MATERIALIZED (
    SELECT prop.key, prop.value
    FROM %s.property AS prop
    WHERE prop.node_id = node.node_id
    ORDER BY prop.key ASC, prop.extendp ASC, prop.begin ASC),
  property(key, value) AS NOT MATERIALIZED (
    SELECT key, group_concat(value, ' ')
    FROM raw_property
    GROUP BY key)
SELECT json_group_object(key, value)
FROM property) AS propslitnil,
    (
WITH RECURSIVE
  ancestor(level, node_id) AS NOT MATERIALIZED (
    SELECT 0, node.node_id
    UNION
    SELECT ancestor.level + 1, parent.parent_id
    FROM ancestor
    JOIN %s.node AS parent
    ON ancestor.node_id = parent.node_id
    AND parent.parent_id IS NOT NULL),
  raw_property(level, key, value) AS MATERIALIZED (
    SELECT ancestor.level, prop.key, prop.value
    FROM %s.property AS prop
    JOIN ancestor
    ON prop.node_id = ancestor.node_id
    ORDER BY ancestor.level DESC, prop.key ASC, prop.extendp ASC, prop.begin ASC),
  property(level, key, value) AS NOT MATERIALIZED (
    SELECT level, key, group_concat(value, ' ')
    FROM raw_property
    GROUP BY level, key),
  minlevel(key, level) AS NOT MATERIALIZED (
    SELECT key, min(level)
    FROM property
    WHERE property.value != 'nil'
    GROUP BY key)
SELECT json_group_object(property.key, property.value)
FROM property
JOIN minlevel
ON property.level = minlevel.level) AS allprops,
    (
WITH RECURSIVE
  ancestor(level, node_id) AS NOT MATERIALIZED (
    SELECT 0, node.node_id
    UNION
    SELECT ancestor.level + 1, parent.parent_id
    FROM ancestor
    JOIN %s.node AS parent
    ON ancestor.node_id = parent.node_id
    AND parent.parent_id IS NOT NULL),
  raw_property(level, key, value) AS MATERIALIZED (
    SELECT ancestor.level, prop.key, prop.value
    FROM %s.property AS prop
    JOIN ancestor
    ON prop.node_id = ancestor.node_id
    ORDER BY ancestor.level DESC, prop.key ASC, prop.extendp ASC, prop.begin ASC),
  property(level, key, value) AS NOT MATERIALIZED (
    SELECT level, key, group_concat(value, ' ')
    FROM raw_property
    GROUP BY level, key),
  minlevel(key, level) AS NOT MATERIALIZED (
    SELECT key, min(level)
    FROM property
    GROUP BY key)
SELECT json_group_object(property.key, property.value)
FROM property
JOIN minlevel
ON property.level = minlevel.level) AS allpropslitnil,
    node.blocked AS blocked,
    node.habit AS habit,
    CASE WHEN scheduled.node_id IS NULL THEN NULL ELSE json_object('begin', scheduled.begin, 'raw-value', scheduled.start_time) END,
    CASE WHEN deadline.node_id IS NULL THEN NULL ELSE json_object('begin', deadline.begin, 'raw-value', deadline.start_time) END,
    CASE WHEN closed.node_id IS NULL THEN NULL ELSE json_object('begin', closed.begin, 'raw-value', closed.start_time) END,
    CASE WHEN timestamp.node_id IS NULL THEN NULL ELSE json_object('begin', timestamp.begin, 'end', timestamp.end, 'start-time', timestamp.start_time, 'end-time', timestamp.end_time) END,
    CASE WHEN timestamp_ia.node_id IS NULL THEN NULL ELSE json_object('begin', timestamp_ia.begin, 'end', timestamp_ia.end, 'start-time', timestamp_ia.start_time, 'end-time', timestamp_ia.end_time) END
FROM %s.node AS node
LEFT JOIN %s.timestamp AS scheduled
ON node.node_id = scheduled.node_id
AND node.scheduled = scheduled.begin
LEFT JOIN %s.timestamp AS deadline
ON node.node_id = deadline.node_id
AND node.deadline = deadline.begin
LEFT JOIN %s.timestamp AS closed
ON node.node_id = closed.node_id
AND node.closed = closed.begin
LEFT JOIN %s.timestamp AS timestamp
ON node.node_id = timestamp.node_id
AND node.timestamp = timestamp.begin
AND NOT coalesce(timestamp.at_range, FALSE)
LEFT JOIN %s.timestamp AS timestamp_ia
ON node.node_id = timestamp_ia.node_id
AND node.timestamp_ia = timestamp_ia.begin
AND NOT coalesce(timestamp_ia.at_range, FALSE)
"
    tables
    (list
     " WHERE node.level > 0 AND node.file_id = ? "
     (orb-agenda--file-file-id file))
    (when (and org-agenda-skip-archived-trees (not org-agenda-archives-mode))
      "AND NOT EXISTS (
WITH RECURSIVE
  property(parent_id, value) AS NOT MATERIALIZED (
    SELECT node.parent_id, 'ARCHIVE' IN (SELECT value FROM json_each(node.tags))
    UNION ALL
    SELECT parent.parent_id, 'ARCHIVE' IN (SELECT value FROM json_each(parent.tags))
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id
    AND property.value IS NOT TRUE)
SELECT value FROM property WHERE value IS TRUE) ")
    (when org-agenda-skip-comment-trees
      "AND NOT EXISTS (
WITH RECURSIVE
  property(parent_id, value) AS NOT MATERIALIZED (
    SELECT node.parent_id, ((node.text IS NOT NULL) AND (substr(node.text, 1, 7) = 'COMMENT') AND (substr(node.text, 8, 1) in (' ', '\t', '')))
    UNION ALL
    SELECT parent.parent_id, ((parent.text IS NOT NULL) AND (substr(parent.text, 1, 7) = 'COMMENT') AND (substr(parent.text, 8, 1) in (' ', '\t', '')))
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id
    AND NOT property.value)
SELECT 1 FROM property WHERE value) ")
    (when (and org-agenda-skip-function-global orb-agenda--skip-function-global)
      (cons
       (format "AND NOT (%s) " (car orb-agenda--skip-function-global))
       (eval (cons 'list (cdr orb-agenda--skip-function-global)))))
    (when (and org-agenda-skip-function orb-agenda--skip-function)
      (cons
       (format "AND NOT (%s) " (car orb-agenda--skip-function))
       (eval (cons 'list (cdr orb-agenda--skip-function)))))
    condition
    " ORDER BY node.point ASC")))


(defun orb-agenda--get-node-scheduled (file condition &optional todo-only tables)
  (when-let
      ((result
        (orb-agenda--query
         file
         (orb-db-concat-query
          condition
          (when todo-only
            (orb-db-concat-query
             (when org-agenda-todo-ignore-with-date
               "
AND node.timestamp IS NULL
AND node.scheduled IS NULL
AND node.deadline IS NULL")
             (when org-agenda-todo-ignore-scheduled
               (orb-db-concat-query
                " AND NOT (scheduled.start_epoch IS NOT NULL"
                (pcase org-agenda-todo-ignore-scheduled
                 ('future (orb-agenda--compare-time "scheduled" "> 0"))
                 ('past (orb-agenda--compare-time "scheduled" "<= 0"))
                 ((pred numberp)
                  (orb-agenda--compare-time
                   "scheduled"
                   (if (>= org-agenda-todo-ignore-scheduled 0) ">= ?" "<= ?")
                   org-agenda-todo-ignore-scheduled)))
                ")"))
             (when org-agenda-todo-ignore-deadlines
               (orb-db-concat-query
                " AND NOT (deadline.start_epoch IS NOT NULL"
                (pcase org-agenda-todo-ignore-deadlines
                  ('all nil)
                  ('future (orb-agenda--compare-time "deadline" "> 0"))
                  ('past (orb-agenda--compare-time "deadline" "<= 0"))
                  ('far
                   (let ((org-agenda-todo-ignore-time-comparison-use-seconds nil))
                     (orb-agenda--compare-time "deadline" "> ?" org-deadline-warning-days)))
                  ((pred numberp)
                   (orb-agenda--compare-time
                    "deadline"
                    (if (>= org-agenda-todo-ignore-deadlines 0) ">= ?" "<= ?")
                    org-agenda-todo-ignore-deadlines))
                  (_
                   (let ((org-agenda-todo-ignore-time-comparison-use-seconds nil))
                     (orb-agenda--compare-time "deadline" "<= ?" org-deadline-warning-days))))
                ")"))
             (when org-agenda-todo-ignore-timestamp
               (orb-db-concat-query
                " AND NOT (timestamp.start_epoch IS NOT NULL"
                (pcase org-agenda-todo-ignore-timestamp
                  ('future (orb-agenda--compare-time "timestamp" "> 0"))
                  ('past (orb-agenda--compare-time "timestamp" "<= 0"))
                  ((pred numberp)
                   (orb-agenda--compare-time
                    "timestamp"
                    (if (>= org-agenda-todo-ignore-timestamp 0) ">= ?" "<= ?")
                    org-agenda-todo-ignore-timestamp)))
                ")")))))
         tables)))
    (let (nodes)
      (dolist (entry result)
        (push
         (apply 'orb-agenda--make-node file entry)
         nodes))
      (nreverse nodes))))

(defun orb-agenda--get-node-blocks (file date)
  (when-let
      ((result
        (orb-agenda--query
         file
         (orb-db-concat-query
          (list
           "AND block.end_time IS NOT NULL
AND substr(block.start_time, 1, 1) != '['
AND unixepoch(datetime(block.start_epoch, 'unixepoch', 'localtime', 'start of day'), 'localtime') <= unixepoch(?, 'localtime')
AND unixepoch(datetime(block.end_epoch, 'unixepoch', 'localtime', 'start of day'), 'localtime') >= unixepoch(?, 'localtime')"
           date date)
          (when org-agenda-skip-timestamp-if-done
            (list
             " AND coalesce(node.todo NOT IN (SELECT value FROM json_each(?)), TRUE)"
             (json-encode org-done-keywords)))
          " GROUP BY node.node_id")
        "JOIN %s.timestamp AS block ON block.node_id = node.node_id"
        "json_group_array(json_object('end', block.end, 'start-time', block.start_time, 'end-time', block.end_time)),")))
    (let (nodes)
      (pcase-dolist (`(,blocks . ,entry) result)
        (push
         (cons
          (apply 'orb-agenda--make-node file entry)
          (json-parse-string (or blocks "[]") :array-type 'list :object-type 'plist))
         nodes))
      (nreverse nodes))))

(defun orb-agenda--get-node-timestamps (file date)
  (when-let
      ((result
        (orb-agenda--query
         file
         (orb-db-concat-query
          (list
           "AND ts.end_time IS NULL
AND coalesce(date(ts.start_epoch, 'unixepoch', 'localtime') = ?, TRUE)
AND coalesce(ts.begin != node.scheduled, TRUE)
AND coalesce(ts.begin != node.deadline, TRUE)
AND coalesce(ts.begin != node.closed, TRUE)"
           date)
          (if (not org-agenda-include-inactive-timestamps)
              " AND substr(ts.start_time, 1, 1) != '['"
            " AND NOT EXISTS (
SELECT 1
FROM %s.progress AS progress
WHERE progress.node_id = ts.node_id
AND progress.ts >= ts.begin
AND progress.ts < ts.end
AND NOT coalesce(ts.at_range, FALSE))")
          (when org-agenda-skip-timestamp-if-done
            (list
             " AND coalesce(node.todo NOT IN (SELECT value FROM json_each(?)), TRUE)"
             (json-encode org-done-keywords)))
          " GROUP BY node.node_id")
         "JOIN %s.timestamp AS ts ON ts.node_id = node.node_id"
         "json_group_array(json_object('begin', ts.begin, 'raw-value', ts.start_time)),")))
    (let (nodes)
      (pcase-dolist (`(,timestamps . ,entry) result)
        (push
         (cons
          (apply 'orb-agenda--make-node file entry)
          (json-parse-string (or timestamps "[]") :array-type 'list :object-type 'plist))
         nodes))
      (nreverse nodes))))

(defun orb-agenda--get-node-sexps (file)
  (when-let
      ((result
        (orb-agenda--query
         file
         "GROUP BY node.node_id"
         "JOIN %s.sexp AS sexp ON sexp.node_id = node.node_id"
          "json_group_array(json_object('begin', sexp.point, 'sexp', sexp.sexp, 'sexp-entry', sexp.entry)),")))
    (let (nodes)
      (pcase-dolist (`(,sexps . ,entry) result)
        (push
         (cons
          (apply 'orb-agenda--make-node file entry)
          (json-parse-string (or sexps "[]") :array-type 'list :object-type 'plist))
         nodes))
      (nreverse nodes))))

(defun orb-agenda--get-node-progress (file items date)
  (when-let
      ((result
        (orb-agenda--query
         file
         (orb-db-concat-query
          (list "AND date(progressts.start_epoch, 'unixepoch', 'localtime') = ?" date)
          (unless (memq 'closed items)
            " AND coalesce(progressts.begin != node.closed, TRUE)")
          (unless (memq 'clock items)
            " AND substr(progress.str, 1, 6) != 'CLOCK:'")
          (unless (memq 'state items)
            " AND substr(progress.str, 1, 1) != '-'")
          "GROUP BY node.node_id")
         "JOIN %s.progress AS progress
ON progress.node_id = node.node_id
JOIN %s.timestamp AS progressts
ON progress.node_id = progressts.node_id
AND progress.ts = progressts.begin
AND NOT coalesce(progressts.at_range, FALSE)"
         "json_group_array(json_object('begin', progress.point, 'timestr', progress.str, 'extra', progress.extra, 'ts', json_object('start-epoch', progressts.start_epoch, 'end-epoch', progressts.end_epoch))),")))
    (let (nodes)
      (pcase-dolist (`(,progress . ,entry) result)
        (push
         (cons
          (apply 'orb-agenda--make-node file entry)
          (json-parse-string (or progress "[]") :array-type 'list :object-type 'plist :null-object nil))
         nodes))
      (nreverse nodes))))


;;;###autoload
(cl-defstruct orb-agenda--marker
  file position node marker auxiliary)

(defun orb-agenda--get-real-marker (marker)
  (if (orb-agenda--marker-p marker)
      (if-let ((m (orb-agenda--marker-marker marker)))
          m
        (let ((buffer
               (find-file-noselect
                (orb-agenda--file-path
                 (orb-agenda--marker-file marker))))
              (m (make-marker)))
          (set-marker m (orb-agenda--marker-position marker) buffer)
          (set-marker-insertion-type m t)
          (setf (orb-agenda--marker-marker marker) m)
          m))
    marker))

(defun orb-agenda--marker-a (rest)
  (if (derived-mode-p 'org-agenda-mode)
      (cons (orb-agenda--get-real-marker (car rest)) (cdr rest))
    rest))

(defun orb-agenda--compare-marker-path (path marker)
  (when (orb-agenda--marker-p marker)
    (equal
     path
     (orb-agenda--file-path
      (orb-agenda--marker-file marker)))))

(defun orb-agenda--find-file-h ()
  (when (orb-file-p buffer-file-name)
    (let ((current (current-buffer))
          (file-name buffer-file-name))
      (dolist (buffer
               (seq-filter
                (lambda (buffer)
                  (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'org-agenda-mode))
                (buffer-list)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (dolist (prop '(org-marker org-hd-marker))
                (goto-char (point-min))
                (while-let
                    ((match
                      (text-property-search-forward prop file-name 'orb-agenda--compare-marker-path))
                     (marker (make-marker)))
                  (set-marker
                   marker
                   (orb-agenda--marker-position (prop-match-value match))
                   current)
                  (set-marker-insertion-type marker t)
                  (setf (orb-agenda--marker-marker (prop-match-value match)) marker)
                  (put-text-property
                   (prop-match-beginning match)
                   (prop-match-end match)
                   prop
                   marker))))))))))

(defun orb-agenda--entry-get-a (fn pom property &optional inherit literal-nil)
  (if (or (not (derived-mode-p 'org-agenda-mode))
          (not (orb-agenda--marker-p pom)))
      (funcall fn (orb-agenda--get-real-marker pom) property inherit literal-nil)
    (orb-agenda-get-property (orb-agenda--marker-node pom) property inherit literal-nil)))


(defun orb-agenda--prepare-buffers-a (fn files)
  (let* ((orb-files (seq-filter 'orb-file-p files))
         (closed-orb-files
          (seq-remove 'org-find-base-buffer-visiting orb-files))
         (other-files (seq-difference files closed-orb-files)))
    (funcall fn other-files)
    (when closed-orb-files
      (orb-db-with-transaction
        (let ((result
               (orb-db-query
                (list
                 "
SELECT todo_keywords, done_keywords
FROM %s.file
WHERE path IN (SELECT value FROM json_each(?))"
                 (json-encode
                  (let ((default-directory orb-directory))
                    (mapcar 'file-relative-name closed-orb-files)))))))
          (setq org-todo-keywords-for-agenda
                (org-uniquify
                 (append
                  org-todo-keywords-for-agenda
                  (apply
                   'append
                   (mapcar
                    (lambda (s) (json-parse-string s :array-type 'list))
                    (mapcar 'car result))))))
          (setq org-done-keywords-for-agenda
                (org-uniquify
                 (append
                  org-done-keywords-for-agenda
                  (apply
                   'append
                   (mapcar
                    (lambda (s) (json-parse-string s :array-type 'list))
                    (mapcar 'cadr result)))))))))))

(defun orb-agenda--get-tags (node-struct inherited-tags)
  (let ((file-tags (orb-agenda--file-tags (orb-agenda--node-file node-struct)))
        (tags (seq-uniq (orb-agenda--node-tags node-struct))))
    (if (not inherited-tags)
        tags
      (mapcar
       (lambda (tag)
         (if (member tag tags)
             tag
           (propertize tag 'inherited t)))
       (seq-uniq
        (append
         (seq-filter
          'org-tag-inherit-p
          (append
           file-tags
           (orb-agenda--node-alltags node-struct)))
         tags))))))

(defun orb-agenda--finalize-a ()
  (unless org-agenda-multi
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (save-excursion
          (while (org-activate-links (point-max))
            (goto-char (match-end 0))))
        (unless (eq org-agenda-remove-tags t)
          (org-agenda-align-tags))
        (unless org-agenda-with-colors
          (remove-text-properties (point-min) (point-max) '(face nil)))
        (when (bound-and-true-p org-overriding-columns-format)
          (setq-local org-local-columns-format
                      org-overriding-columns-format))
        (when org-agenda-view-columns-initially
          (org-agenda-columns))
        (when org-agenda-fontify-priorities
          (org-agenda-fontify-priorities))
        (when (and org-agenda-dim-blocked-tasks org-blocker-hook)
          (org-agenda-dim-blocked-tasks))
        (org-agenda-mark-clocking-task)
        (when org-agenda-entry-text-mode
          (org-agenda-entry-text-hide)
          (org-agenda-entry-text-show))
        (when (and (featurep 'org-habit)
                   (save-excursion (next-single-property-change (point-min) 'org-habit-p)))
          (org-habit-insert-consistency-graphs))
        (setq org-agenda-type (org-get-at-bol 'org-agenda-type))
        (unless (or (eq org-agenda-show-inherited-tags 'always)
                    (and (listp org-agenda-show-inherited-tags)
                         (memq org-agenda-type org-agenda-show-inherited-tags))
                    (and (eq org-agenda-show-inherited-tags t)
                         (or (eq org-agenda-use-tag-inheritance t)
                             (and (listp org-agenda-use-tag-inheritance)
                                  (not (memq org-agenda-type
                                             org-agenda-use-tag-inheritance))))))
          (let (mrk)
            (save-excursion
              (goto-char (point-min))
              (while (equal (forward-line) 0)
                (when (setq mrk (get-text-property (point) 'org-hd-marker))
                  (put-text-property
                   (line-beginning-position) (line-end-position)
                   'tags
                   (if (orb-agenda--marker-p mrk)
                       (orb-agenda--get-tags
                        (orb-agenda--marker-node mrk) t)
                     (org-with-point-at mrk
                       (org-get-tags)))))))))
        (setq org-agenda-represented-tags nil
              org-agenda-represented-categories nil)
        (when org-agenda-top-headline-filter
          (org-agenda-filter-top-headline-apply
           org-agenda-top-headline-filter))
        (when org-agenda-tag-filter
          (org-agenda-filter-apply org-agenda-tag-filter 'tag t))
        (when (assoc-default 'tag org-agenda-filters-preset)
          (org-agenda-filter-apply
           (assoc-default 'tag org-agenda-filters-preset) 'tag t))
        (when org-agenda-category-filter
          (org-agenda-filter-apply org-agenda-category-filter 'category))
        (when (assoc-default 'category org-agenda-filters-preset)
          (org-agenda-filter-apply
           (assoc-default 'category org-agenda-filters-preset) 'category))
        (when org-agenda-regexp-filter
          (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))
        (when (assoc-default 'regexp org-agenda-filters-preset)
          (org-agenda-filter-apply
           (assoc-default 'regexp org-agenda-filters-preset) 'regexp))
        (when org-agenda-effort-filter
          (org-agenda-filter-apply org-agenda-effort-filter 'effort))
        (when (assoc-default 'effort org-agenda-filters-preset)
          (org-agenda-filter-apply
           (assoc-default 'effort org-agenda-filters-preset) 'effort))
        (add-hook 'kill-buffer-hook #'org-agenda-reset-markers 'append 'local))
      (run-hooks 'org-agenda-finalize-hook))))


(defun orb-agenda--mark-blocked-entry-a (entry)
  (when (get-text-property 0 'todo-state entry)
    (let ((entry-marker (get-text-property 0 'org-hd-marker entry))
          (org-blocked-by-checkboxes nil)
          ;; Necessary so that `org-entry-blocked-p' does not change
          ;; the buffer.
          (org-depend-tag-blocked nil))
      (when entry-marker
        (let ((blocked
               (if (orb-agenda--marker-p entry-marker)
                   (orb-agenda--node-blocked (orb-agenda--marker-node entry-marker))
                 (with-current-buffer (marker-buffer entry-marker)
                   (save-excursion
                     (goto-char entry-marker)
                     (org-entry-blocked-p))))))
          (when blocked
            (let ((really-invisible
                   (and (not org-blocked-by-checkboxes)
                        (eq org-agenda-dim-blocked-tasks 'invisible))))
              (put-text-property
               0 (length entry) 'org-todo-blocked
               (if really-invisible 'invisible t)
               entry)
              (put-text-property
               0 (length entry) 'org-filter-type 'todo-blocked entry)))))))
  entry)

(defun orb-agenda--skip-todo-to-params (match)
  (pcase match
    (`'todo
     '(json-encode org-todo-keywords-1))
    (`'done
     '(json-encode org-done-keywords))
    (_
     (cons 'json-encode (cons 'quote match)))))

(defun orb-agenda--skip-conditions-to-query (conditions)
  (pcase conditions
    (`('scheduled . ,rest)
     (cons
      "node.scheduled IS NOT NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('notscheduled . ,rest)
     (cons
      "node.scheduled IS NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('deadline . ,rest)
     (cons
      "node.deadline IS NOT NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('notdeadline . ,rest)
     (cons
      "node.deadline IS NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('timestamp . ,rest)
     (cons
      "node.scheduled IS NOT NULL OR node.deadline IS NOT NULL OR node.timestamp IS NOT NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('nottimestamp . ,rest)
     (cons
      "node.scheduled IS NULL AND node.deadline IS NULL AND node.timestamp IS NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('todo 'any . ,rest)
     (cons
      "node.todo IS NOT NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('todo ,match . ,rest)
     (cons
      (list
       "coalesce(node.todo IN (SELECT value FROM json_each(?)), FALSE)"
       (orb-agenda--skip-todo-to-params match))
      (orb-agenda--skip-conditions-to-query rest)))
    (`('nottodo 'any . ,rest)
     (cons
      "node.todo IS NULL"
      (orb-agenda--skip-conditions-to-query rest)))
    (`('nottodo ,match . ,rest)
     (cons
      (list
       "coalesce(node.todo NOT IN (SELECT value FROM json_each(?)), TRUE)"
       (orb-agenda--skip-todo-to-params match))
      (orb-agenda--skip-conditions-to-query rest)))
    (`nil
     nil)
    (_
     (throw :not-supported nil))))

(defun orb-agenda--join-queries (list separator)
  (cons
   (concat
    "("
    (string-join
     (mapcar (lambda (x) (if (stringp x) x (car x))) list)
     (format ") %s (" separator))
    ")")
   (apply 'append (mapcar (lambda (x) (unless (stringp x) (cdr x))) list))))

(defun orb-agenda--skip-function-to-query (f)
  (pcase f
    (`(not ,e)
     (orb-db-concat-query
      "NOT ("
      (orb-agenda--skip-function-to-query e)
      ")"))
    (`(and . ,rest)
     (orb-agenda--join-queries
      (mapcar 'orb-agenda--skip-function-to-query rest)
      "AND"))
    (`(or . ,rest)
     (orb-agenda--join-queries
      (mapcar 'orb-agenda--skip-function-to-query rest)
      "OR"))
    (`(org-agenda-skip-entry-if . ,conditions)
     (orb-agenda--join-queries
      (orb-agenda--skip-conditions-to-query conditions)
      "OR"))
    (_
     (throw :not-supported nil))))

(defun orb-agenda--with-transaction-a (fun &rest args)
  (orb-db-with-transaction
    (apply fun args)))

(defun orb-agenda--convert-skip-function-a (fun &rest args)
  (let ((orb-agenda--skip-function-global
         (when org-agenda-skip-function-global
           (catch :not-supported
             (orb-agenda--skip-function-to-query org-agenda-skip-function-global))))
        (orb-agenda--skip-function
         (when org-agenda-skip-function
           (catch :not-supported
             (orb-agenda--skip-function-to-query org-agenda-skip-function)))))
    (apply 'orb-agenda--with-transaction-a fun args)))

(defun orb-agenda--get-property (node-struct property &optional inherit literal-nil)
  (gethash
   property
   (if (if (eq inherit 'selective)
           (org-property-inherit-p property)
         inherit)
       (if literal-nil
           (orb-agenda--node-allprops-literal-nil node-struct)
         (orb-agenda--node-allprops node-struct))
     (if literal-nil
         (orb-agenda--node-props-literal-nil node-struct)
       (orb-agenda--node-props node-struct)))))

(defun orb-agenda-get-property (node-struct property &optional inherit literal-nil)
  (pcase (upcase property)
    ("CATEGORY" (orb-agenda--get-category node-struct))
    ("ITEM"
     (let ((title (orb-agenda--node-heading node-struct)))
       (if (org-string-nw-p title)
           (org-remove-tabs title)
         "")))
    ("TODO" (orb-agenda--node-todo node-struct))
    ("PRIORITY"
     (save-match-data
       (let ((s (orb-agenda--node-text node-struct)))
         (if (string-match org-priority-regexp s)
             (match-string 2 s)
           (char-to-string org-priority-default)))))
    ("FILE"
     (orb-agenda--file-path (orb-agenda--node-file node-struct)))
    ("TAGS" (org-make-tag-string (orb-agenda--get-tags node-struct nil)))
    ("ALLTAGS" (org-make-tag-string (orb-agenda--get-tags node-struct t)))
    ("BLOCKED" (if (orb-agenda--node-blocked node-struct) "t" ""))
    ("CLOSED"
     (when-let ((plist (orb-agenda--node-closed node-struct)))
       (plist-get plist :raw-value)))
    ("DEADLINE"
     (when-let ((plist (orb-agenda--node-deadline node-struct)))
       (plist-get plist :raw-value)))
    ("SCHEDULED"
     (when-let ((plist (orb-agenda--node-scheduled node-struct)))
       (plist-get plist :raw-value)))
    ("TIMESTAMP"
     (when-let ((plist (orb-agenda--node-timestamp node-struct)))
       (if-let ((end-time (plist-get plist :end-time)))
           (concat (plist-get plist :start-time) "--" end-time)
         (plist-get plist :start-time))))
    ("TIMESTAMP_IA"
     (when-let ((plist (orb-agenda--node-timestamp-ia node-struct)))
       (if-let ((end-time (plist-get plist :end-time)))
           (concat (plist-get plist :start-time) "--" end-time)
         (plist-get plist :start-time))))
    (prop
     (orb-agenda--get-property node-struct prop inherit literal-nil))))

(defun orb-agenda-entry-get-agenda-timestamp (node-struct)
  "Retrieve timestamp information for sorting agenda views.
Given a point or marker POM, returns a cons cell of the timestamp
and the timestamp type relevant for the sorting strategy in
`org-agenda-sorting-strategy-selected'."
  (let (ts ts-date-type)
    (save-match-data
      (cond ((org-em 'scheduled-up 'scheduled-down
                     org-agenda-sorting-strategy-selected)
             (setq ts (orb-agenda-get-property node-struct "SCHEDULED")
                   ts-date-type " scheduled"))
            ((org-em 'deadline-up 'deadline-down
                     org-agenda-sorting-strategy-selected)
             (setq ts (orb-agenda-get-property node-struct "DEADLINE")
                   ts-date-type " deadline"))
            ((org-em 'ts-up 'ts-down
                     org-agenda-sorting-strategy-selected)
             (setq ts (orb-agenda-get-property node-struct "TIMESTAMP")
                   ts-date-type " timestamp"))
            ((org-em 'tsia-up 'tsia-down
                     org-agenda-sorting-strategy-selected)
             (setq ts (orb-agenda-get-property node-struct "TIMESTAMP_IA")
                   ts-date-type " timestamp_ia"))
            ((org-em 'timestamp-up 'timestamp-down
                     org-agenda-sorting-strategy-selected)
             (setq ts (or (orb-agenda-get-property node-struct "SCHEDULED")
                          (orb-agenda-get-property node-struct "DEADLINE")
                          (orb-agenda-get-property node-struct "TIMESTAMP")
                          (orb-agenda-get-property node-struct "TIMESTAMP_IA"))
                   ts-date-type ""))
            (t (setq ts-date-type "")))
      (cons (when ts (ignore-errors (org-time-string-to-absolute ts)))
            ts-date-type))))

(defun orb-agenda-format-item (extra txt &optional with-level with-category tags dotime
                                     remove-re habitp outline-path)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
WITH-LEVEL may be a string to replace the `%l' specifier.
WITH-CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (let* ((bindings (car org-prefix-format-compiled))
         (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
             do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
                 txt tags
                 org-agenda-show-inherited-tags
                 org-agenda-hide-tags-regexp))

      (with-no-warnings
        ;; `time', `tag', `effort' are needed for the eval of the prefix format.
        ;; Based on what I see in `org-compile-prefix-format', I added
        ;; a few more.
        (defvar breadcrumbs) (defvar category) (defvar category-icon)
        (defvar effort) (defvar extra)
        (defvar level) (defvar tag) (defvar time))
      (let* ((category (or with-category
                           (if-let ((file-name (orb-agenda--file-path orb-agenda--current-file)))
                               (file-name-sans-extension
                                (file-name-nondirectory file-name))
                             "")))
             (category-icon (org-agenda-get-category-icon category))
             (category-icon (if category-icon
                                (propertize " " 'display category-icon)
                              ""))
             (effort (and (not (string= txt ""))
                          (get-text-property 1 'effort txt)))
             (tag (if tags (nth (1- (length tags)) tags) ""))
             (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
             (extra (or (and (not habitp) extra) ""))
             time
             (ts (when dotime (concat
                               (if (stringp dotime) dotime "")
                               (and org-agenda-search-headline-for-time txt))))
             (time-of-day (and dotime (org-get-time-of-day ts)))
             stamp plain s0 s1 s2 rtn srp l
             duration breadcrumbs)
        (if-let ((file-name (orb-agenda--file-path orb-agenda--current-file)))
            (add-to-list 'org-agenda-contributing-files file-name))
        (when (and dotime time-of-day)
          ;; Extract starting and ending time and move them to prefix
          (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
                    (setq plain (string-match org-plain-time-of-day-regexp ts)))
            (setq s0 (match-string 0 ts)
                  srp (and stamp (match-end 3))
                  s1 (match-string (if plain 1 2) ts)
                  s2 (match-string (if plain 8 (if srp 4 6)) ts))

            ;; If the times are in TXT (not in DOTIMES), and the prefix will list
            ;; them, we might want to remove them there to avoid duplication.
            ;; The user can turn this off with a variable.
            (when (and org-prefix-has-time
                       org-agenda-remove-times-when-in-prefix (or stamp plain)
                       (string-match (concat (regexp-quote s0) " *") txt)
                       (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
                       (if (eq org-agenda-remove-times-when-in-prefix 'beg)
                           (= (match-beginning 0) 0)
                         t))
              (setq txt (replace-match "" nil nil txt))))
          ;; Normalize the time(s) to 24 hour.
          (when s1 (setq s1 (org-get-time-of-day s1 t)))
          (when s2 (setq s2 (org-get-time-of-day s2 t)))
          ;; Try to set s2 if s1 and
          ;; `org-agenda-default-appointment-duration' are set
          (when (and s1 (not s2) org-agenda-default-appointment-duration)
            (setq s2
                  (org-duration-from-minutes
                   (+ (org-duration-to-minutes s1 t)
                      org-agenda-default-appointment-duration)
                   nil t)))
          ;; Compute the duration
          (when s2
            (setq duration (- (org-duration-to-minutes s2)
                              (org-duration-to-minutes s1))))
          ;; Format S1 and S2 for display.
          (when s1 (setq s1 (format "%5s" (org-get-time-of-day s1 'overtime))))
          (when s2 (setq s2 (org-get-time-of-day s2 'overtime))))
        (when (string-match org-tag-group-re txt)
          ;; Tags are in the string
          (if (or (eq org-agenda-remove-tags t)
                  (and org-agenda-remove-tags
                       org-prefix-has-tag))
              (setq txt (replace-match "" t t txt))
            (setq txt (replace-match
                       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
                               (match-string 1 txt))
                       t t txt))))

        (when remove-re
          (while (string-match remove-re txt)
            (setq txt (replace-match "" t t txt))))

        ;; Set org-heading property on `txt' to mark the start of the
        ;; heading.
        (add-text-properties 0 (length txt) '(org-heading t) txt)

        ;; Prepare the variables needed in the eval of the compiled format
        (when org-prefix-has-breadcrumbs
          (setq breadcrumbs
                (let ((s (org-format-outline-path outline-path
                                                  (1- (frame-width))
                                                  nil org-agenda-breadcrumbs-separator)))
                  (if (equal "" s) "" (concat s org-agenda-breadcrumbs-separator)))))
        (setq time (cond (s2 (concat
                              (org-agenda-time-of-day-to-ampm-maybe s1)
                              "-" (org-agenda-time-of-day-to-ampm-maybe s2)
                              (when org-agenda-timegrid-use-ampm " ")))
                         (s1 (concat
                              (org-agenda-time-of-day-to-ampm-maybe s1)
                              (if org-agenda-timegrid-use-ampm
                                  (concat time-grid-trailing-characters " ")
                                time-grid-trailing-characters)))
                         (t ""))
              category (if (symbolp category) (symbol-name category) category)
              level (or with-level ""))
        (if (string-match org-link-bracket-re category)
            (progn
              (setq l (string-width (or (match-string 2) (match-string 1))))
              (when (< l (or org-prefix-category-length 0))
                (setq category (copy-sequence category))
                (org-add-props category nil
                  'extra-space (make-string
                                (- org-prefix-category-length l 1) ?\ ))))
          (when (and org-prefix-category-max-length
                     (>= (length category) org-prefix-category-max-length))
            (setq category (substring category 0 (1- org-prefix-category-max-length)))))
        ;; Evaluate the compiled format
        (setq rtn (concat (eval formatter t) txt))

        ;; And finally add the text properties
        (remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
        (org-add-props rtn nil
          'org-category category
          'tags tags
          'org-priority-highest org-priority-highest
          'org-priority-lowest org-priority-lowest
          'time-of-day time-of-day
          'duration duration
          'breadcrumbs breadcrumbs
          'txt txt
          'level level
          'time time
          'extra extra
          'format org-prefix-format-compiled
          'dotime dotime)))))

(defun orb-agenda--format-date (date)
  (format "%04d-%02d-%02d" (caddr date) (car date) (cadr date)))

(defun orb-agenda--get-category (node-struct)
  (or (orb-agenda--get-property node-struct "CATEGORY" t)
      (let ((file (orb-agenda--node-file node-struct)))
        (or (gethash "CATEGORY" (orb-agenda--file-keywords file))
            (file-name-base (orb-agenda--file-path file))))))

(defun orb-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
                      'done-face 'org-agenda-done
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'mouse-face 'highlight
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         ee)
    (dolist (node-struct
             (orb-agenda--get-node-scheduled
              orb-agenda--current-file
              (orb-db-concat-query
               (list
                " AND node.todo IN (SELECT value FROM json_each(?))"
                (json-encode
                 (cond
                  ((and org-select-this-todo-keyword
                        (equal org-select-this-todo-keyword "*"))
                   org-todo-keywords-1)
                  (org-select-this-todo-keyword
                   (org-split-string org-select-this-todo-keyword "|"))
                  (t
                   (seq-difference org-todo-keywords-1 org-done-keywords)))))
               (unless org-agenda-todo-list-sublevels
                 " AND NOT EXISTS (
WITH RECURSIVE
  property(parent_id, value) AS NOT MATERIALIZED (
    SELECT parent.parent_id, parent.todo IS NOT NULL
    FROM %s.node AS parent
    WHERE parent.node_id = node.parent_id
    UNION ALL
    SELECT parent.parent_id, parent.todo IS NOT NULL
    FROM %s.node AS parent
    JOIN property
    ON parent.node_id = property.parent_id
    AND NOT property.value)
SELECT value FROM property WHERE value IS TRUE
)"))
              t))
      (let* ((todo-state (orb-agenda--node-todo node-struct))
             (marker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
             (category (orb-agenda--get-category node-struct))
             (effort (orb-agenda-get-property node-struct org-effort-property))
             (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
             (ts-date-pair (orb-agenda-entry-get-agenda-timestamp node-struct))
             (ts-date (car ts-date-pair))
             (ts-date-type (cdr ts-date-pair))
             (level (make-string (org-reduced-level
                                  (orb-agenda--node-level node-struct))
                                 ?\s))
             (olp (orb-agenda--node-olp node-struct))
             (txt (concat todo-state " " (orb-agenda--node-text node-struct)))
             (inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'todo org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'todo org-agenda-use-tag-inheritance)))))
             (tags (orb-agenda--get-tags node-struct inherited-tags))
             (txt (orb-agenda-format-item
                   ""
                   (org-add-props txt nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   level category tags t
                   nil nil olp))
             (priority (1+ (org-get-priority txt))))
        (org-add-props txt props
          'org-marker marker 'org-hd-marker marker
          'priority priority
          'effort effort 'effort-minutes effort-minutes
          'level level
          'ts-date ts-date
          'type (concat "todo" ts-date-type) 'todo-state todo-state)
        (push txt ee)))
    (nreverse ee)))

(defun orb-agenda-get-timestamps (&optional deadlines)
  "Return the date stamp information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view."
  (with-no-warnings (defvar date))
  (let* ((props (list 'face 'org-agenda-calendar-event
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'mouse-face 'highlight
                      'help-echo
                      (format "mouse-2 or RET jump to Org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         (current (calendar-absolute-from-gregorian date))
         (today (org-today))
         (deadline-pos
          (mapcar (lambda (d)
                    (let ((m (get-text-property 0 'orb-marker d)))
                      (cdr m)))
                  deadlines))
         timestamp-items)
    (pcase-dolist
        (`(,node-struct . ,timestamps)
         (orb-agenda--get-node-timestamps
          orb-agenda--current-file
          (orb-agenda--format-date date)))
      (catch :skip
        (let* ((todo-state (orb-agenda--node-todo node-struct))
               (warntime (orb-agenda-get-property node-struct "APPT_WARNTIME" 'selective))
               (category (orb-agenda--get-category node-struct))
               (effort (orb-agenda-get-property node-struct org-effort-property))
               (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
               (hdmarker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
               (inherited-tags
                (or (eq org-agenda-show-inherited-tags 'always)
                    (and (listp org-agenda-show-inherited-tags)
                         (memq 'todo org-agenda-show-inherited-tags))
                    (and (eq org-agenda-show-inherited-tags t)
                         (or (eq org-agenda-use-tag-inheritance t)
                             (memq 'agenda org-agenda-use-tag-inheritance)))))
               (tags (orb-agenda--get-tags node-struct inherited-tags))
               (level (make-string (org-reduced-level
                                    (orb-agenda--node-level node-struct))
                                   ?\s))
               (olp (orb-agenda--node-olp node-struct))
               (habit? (orb-agenda--node-habit node-struct))
               (head (concat todo-state " " (orb-agenda--node-text node-struct))))
          (when (and org-agenda-skip-timestamp-if-deadline-is-shown
                     (memq (orb-agenda--node-point node-struct) deadline-pos))
            (throw :skip nil))
          (pcase-dolist
              ((map (:begin pos) (:raw-value time-stamp))
               (seq-sort-by (lambda (ts) (plist-get ts :begin)) '< timestamps))
            (when
                (and
                 (catch :skip
                   (save-match-data
                     (string-match orb--timestamp-regexp time-stamp)
                     (let*
                         ((marker (make-orb-agenda--marker :file orb-agenda--current-file :position pos :node node-struct))
                          (sexp-entry (match-string 3 time-stamp))
                          (repeat (match-string 1 time-stamp))
                          (inactive? (string-prefix-p "[" time-stamp))
                          (item
                           (orb-agenda-format-item
                            (and inactive? org-agenda-inactive-leader)
                            (org-add-props head nil
                              'effort effort
                              'effort-minutes effort-minutes)
                            level category tags time-stamp org-ts-regexp habit?
                            olp)))
                       ;; S-exp entry doesn't match current day: skip it.
                       (when (and sexp-entry (not (org-diary-sexp-entry sexp-entry "" date)))
                         (throw :skip nil))
                       (when repeat
                         (let*
                             ((past
                               ;; A repeating time stamp is shown at its base
                               ;; date and every repeated date up to TODAY.  If
                               ;; `org-agenda-prefer-last-repeat' is non-nil,
                               ;; however, only the last repeat before today
                               ;; (inclusive) is shown.
                               (org-agenda--timestamp-to-absolute
                                repeat
                                (if (or (> current today)
                                        (eq org-agenda-prefer-last-repeat t)
                                        (member todo-state org-agenda-prefer-last-repeat))
                                    today
                                  current)
                                'past
                                (orb-agenda--file-path orb-agenda--current-file)
                                (orb-agenda--node-point node-struct)))
                              (future
                               ;;  Display every repeated date past TODAY
                               ;;  (exclusive) unless
                               ;;  `org-agenda-show-future-repeats' is nil.  If
                               ;;  this variable is set to `next', only display
                               ;;  the first repeated date after TODAY
                               ;;  (exclusive).
                               (cond
                                ((<= current today) past)
                                ((not org-agenda-show-future-repeats) past)
                                (t
                                 (let ((base (if (eq org-agenda-show-future-repeats 'next)
                                                 (1+ today)
                                               current)))
                                   (org-agenda--timestamp-to-absolute
                                    repeat base 'future
                                    (orb-agenda--file-path orb-agenda--current-file)
                                    (orb-agenda--node-point node-struct)))))))
                           (when (and (/= current past) (/= current future))
                             (throw :skip nil))))
                       (org-add-props item props
                         'priority
                         (if habit?
                             (org-habit-get-priority (org-habit-parse-todo))
                           (org-get-priority item))
                         'org-marker marker
                         'org-hd-marker hdmarker
                         'date date
                         'level level
                         'effort effort 'effort-minutes effort-minutes
                         'ts-date
                         (if repeat (org-agenda--timestamp-to-absolute repeat)
                           current)
                         'todo-state todo-state
                         'warntime warntime
                         'type "timestamp")
                       (push item timestamp-items)
                       t)))
                 org-agenda-skip-additional-timestamps-same-entry)
              (throw :skip nil))))))
    (nreverse timestamp-items)))

(defun orb-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (with-no-warnings (defvar date) (defvar entry))
  (let ((props (list 'face 'org-agenda-calendar-sexp
                     'mouse-face 'highlight
                     'help-echo
                     (format "mouse-2 or RET jump to org file %s"
                             (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
        ee txt)
    (pcase-dolist
        (`(,node-struct . ,sexps)
         (orb-agenda--get-node-sexps orb-agenda--current-file))
      (let* ((level (make-string (org-reduced-level
                                  (orb-agenda--node-level node-struct))
                                 ?\s))
             (olp (orb-agenda--node-olp node-struct))
             (category (orb-agenda--get-category node-struct))
             (effort (orb-agenda-get-property node-struct org-effort-property))
             (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
             (inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'todo org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'agenda org-agenda-use-tag-inheritance)))))
             (tags (orb-agenda--get-tags node-struct inherited-tags))
             (todo-state (orb-agenda--node-todo node-struct))
             (warntime (orb-agenda-get-property node-struct "APPT_WARNTIME" 'selective))
             (extra nil))
        (pcase-dolist
            ((map (:begin beg) (:sexp sexp) (:sexp-entry sexp-entry))
             (seq-sort-by (lambda (s) (plist-get s :begin)) '< sexps))
          (when-let ((result (org-diary-sexp-entry sexp sexp-entry date)))
            (dolist (r (if (stringp result)
                           (list result)
                         result)) ;; we expect a list here
              (when (and org-agenda-diary-sexp-prefix
                         (string-match org-agenda-diary-sexp-prefix r))
                (setq extra (match-string 0 r)
                      r (replace-match "" nil nil r)))
              (if (string-match "\\S-" r)
                  (setq txt r)
                (setq txt "SEXP entry returned empty string"))
              (setq txt (orb-agenda-format-item
                         extra
                         (org-add-props txt nil
                           'effort effort
                           'effort-minutes effort-minutes)
                         level category tags 'time
                         nil nil olp))
              (org-add-props txt props
                'org-marker (make-orb-agenda--marker :file orb-agenda--current-file :position beg :node node-struct)
                'date date 'todo-state todo-state
                'effort effort 'effort-minutes effort-minutes
                'level level 'type "sexp" 'warntime warntime)
              (push txt ee))))))
    (nreverse ee)))


(defun orb-agenda-get-progress ()
  "Return the logged TODO entries for agenda display."
  (with-no-warnings (defvar date))
  (let* ((props (list 'mouse-face 'highlight
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         (items (if (consp org-agenda-show-log-scoped)
                    org-agenda-show-log-scoped
                  (if (eq org-agenda-show-log-scoped 'clockcheck)
                      '(clock)
                    org-agenda-log-mode-items)))
         ee)

    (pcase-dolist
        (`(,node-struct . ,progress)
         (orb-agenda--get-node-progress
          orb-agenda--current-file items (orb-agenda--format-date date)))
      (let* ((todo-state (orb-agenda--node-todo node-struct))
             (category (orb-agenda--get-category node-struct))
             (effort (orb-agenda-get-property node-struct org-effort-property))
             (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
             (hdmarker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
             (inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'todo org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'todo org-agenda-use-tag-inheritance)))))
             (tags (orb-agenda--get-tags node-struct inherited-tags))
             (level (make-string (org-reduced-level
                                  (orb-agenda--node-level node-struct))
                                 ?\s))
             (olp (orb-agenda--node-olp node-struct))
             (head (concat todo-state " " (orb-agenda--node-text node-struct))))
        (pcase-dolist
            ((map (:begin pos) (:timestr timestr) (:extra extra) (:ts ts))
             (seq-sort-by (lambda (p) (plist-get p :begin)) '< progress))
          (let* ((marker (make-orb-agenda--marker :file orb-agenda--current-file :position pos :node node-struct :auxiliary ts))
                 (closedp (string-prefix-p org-closed-string timestr))
                 (statep (string-prefix-p "-" timestr))
                 (clockp (not (or closedp statep)))
                 (state (and statep
                             (save-match-data
                               (string-match
                                (format "- +State \"%s\".*?" org-todo-regexp)
                                timestr)
                               (match-string 1 timestr))))
                 (clocked
                  (and clockp
                       (substring timestr (1+ (string-search "]" timestr)))))
                 (txt
                  (orb-agenda-format-item
                   (cond
                    (closedp "Closed:    ")
                    (statep (concat "State:     (" state ")"))
                    (t (concat "Clocked:   (" clocked  ")")))
                   (org-add-props
                       (if (or (not extra) (not org-agenda-log-mode-add-notes))
                           head
                         (if (string-match "\\([ \t]+\\)\\(:[^ \n\t]*?:\\)[ \t]*$" head)
                             (concat (substring head 0 (match-beginning 1))
                                     " - " extra " " (match-string 2 head))
                           (concat head " - " extra)))
                       nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   level category tags
                   (if (not clockp)
                       timestr
                     (substring timestr 0 (1+ (string-search "]" timestr))))
                   nil nil olp))
                 (type (cond (closedp "closed")
                             (statep "state")
                             (t "clock"))))
            (org-add-props txt props
              'org-marker marker 'org-hd-marker hdmarker 'face 'org-agenda-done
              'priority 100000 'level level
              'effort effort 'effort-minutes effort-minutes
              'type type 'date date
              'undone-face 'org-warning 'done-face 'org-agenda-done)
            (push txt ee)))))
    (nreverse ee)))

(defun orb-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let* ((today (org-today))
         (today? (org-agenda-today-p date)) ; DATE bound by calendar.
         (current (calendar-absolute-from-gregorian date))
         (props (list 'mouse-face 'highlight
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         deadline-items)
    (dolist (node-struct
             (orb-agenda--get-node-scheduled
              orb-agenda--current-file
              (orb-db-concat-query
               " AND node.deadline IS NOT NULL"
               (when with-hour
                 " AND deadline.start_time LIKE '%%:%%'")
               (when org-agenda-skip-deadline-if-done
                 (list
                  " AND coalesce(node.todo NOT IN (SELECT value FROM json_each(?)), TRUE)"
                  (json-encode org-done-keywords))))))
      (catch :skip
        (let* ((s (substring (plist-get (orb-agenda--node-deadline node-struct) :raw-value) 1 -1))
               (pos (plist-get (orb-agenda--node-deadline node-struct) :begin))
               (todo-state (orb-agenda--node-todo node-struct))
               (done? (member todo-state org-done-keywords))
               (sexp? (string-prefix-p "%%" s))
               (marker (make-orb-agenda--marker :file orb-agenda--current-file :position pos :node node-struct))
               (deadline
                (cond
                 (sexp? (org-agenda--timestamp-to-absolute s current))
                 ((or (eq org-agenda-prefer-last-repeat t)
                      (member todo-state org-agenda-prefer-last-repeat))
                  (org-agenda--timestamp-to-absolute
                   s today 'past (current-buffer) pos))
                 (t (org-agenda--timestamp-to-absolute s))))
               ;; REPEAT is the future repeat closest from CURRENT,
               ;; according to `org-agenda-show-future-repeats'. If
               ;; the latter is nil, or if the time stamp has no
               ;; repeat part, default to DEADLINE.
               (repeat
                (cond
                 (sexp? deadline)
                 ((<= current today) deadline)
                 ((not org-agenda-show-future-repeats) deadline)
                 (t
                  (let ((base (if (eq org-agenda-show-future-repeats 'next)
                                  (1+ today)
                                current)))
                    (org-agenda--timestamp-to-absolute
                     s base 'future (current-buffer) pos)))))
               (diff (- deadline current))
               (suppress-prewarning
                (let ((scheduled
                       (and org-agenda-skip-deadline-prewarning-if-scheduled
                            (plist-get (orb-agenda--node-scheduled node-struct) :begin))))
                  (cond
                   ((not scheduled) nil)
                   ;; The current item has a scheduled date, so
                   ;; evaluate its prewarning lead time.
                   ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
                    ;; Use global prewarning-restart lead time.
                    org-agenda-skip-deadline-prewarning-if-scheduled)
                   ((eq org-agenda-skip-deadline-prewarning-if-scheduled
                        'pre-scheduled)
                    ;; Set pre-warning to no earlier than SCHEDULED.
                    (min (- deadline
                            (org-agenda--timestamp-to-absolute scheduled))
                         org-deadline-warning-days))
                   ;; Set pre-warning to deadline.
                   (t 0))))
               (wdays (or suppress-prewarning (org-get-wdays s))))
          (cond
           ;; Only display deadlines at their base date, at future
           ;; repeat occurrences or in today agenda.
           ((= current deadline) nil)
           ((= current repeat) nil)
           ((not today?) (throw :skip nil))
           ;; Upcoming deadline: display within warning period WDAYS.
           ((> deadline current) (when (> diff wdays) (throw :skip nil)))
           ;; Overdue deadline: warn about it for
           ;; `org-deadline-past-days' duration.
           (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
          ;; Possibly skip done tasks.
          (when (and done? (/= deadline current))
            (throw :skip nil))
          (let* ((category (orb-agenda--get-category node-struct))
                 (effort (orb-agenda-get-property node-struct org-effort-property))
                 (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
                 (level (make-string (org-reduced-level
                                      (orb-agenda--node-level node-struct))
                                     ?\s))
                 (olp (orb-agenda--node-olp node-struct))
                 (head (concat todo-state " " (orb-agenda--node-text node-struct)))
                 (hdmarker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
                 (inherited-tags
                  (or (eq org-agenda-show-inherited-tags 'always)
                      (and (listp org-agenda-show-inherited-tags)
                           (memq 'todo org-agenda-show-inherited-tags))
                      (and (eq org-agenda-show-inherited-tags t)
                           (or (eq org-agenda-use-tag-inheritance t)
                               (memq 'agenda org-agenda-use-tag-inheritance)))))
                 (tags (orb-agenda--get-tags node-struct inherited-tags))
                 (time
                  (save-match-data
                    (cond
                     ;; No time of day designation if it is only a
                     ;; reminder.
                     ((and (/= current deadline) (/= current repeat)) nil)
                     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
                      (concat (substring s (match-beginning 1)) " "))
                     (t 'time))))
                 (item
                  (orb-agenda-format-item
                   ;; Insert appropriate suffixes before deadlines.
                   ;; Those only apply to today agenda.
                   (pcase-let ((`(,now ,future ,past)
                                org-agenda-deadline-leaders))
                     (cond
                      ((and today? (< deadline today)) (format past (- diff)))
                      ((and today? (> deadline today)) (format future diff))
                      (t now)))
                   (org-add-props head nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   level category tags time
                   nil nil olp))
                 (face (org-agenda-deadline-face
                        (- 1 (/ (float diff) (max wdays 1)))))
                 (upcoming? (and today? (> deadline today)))
                 (warntime (orb-agenda-get-property node-struct "APPT_WARNTIME" 'selective)))
            (org-add-props item props
              'org-marker marker
              'org-hd-marker hdmarker
              'warntime warntime
              'level level
              'effort effort 'effort-minutes effort-minutes
              'ts-date deadline
              'priority
              ;; Adjust priority to today reminders about deadlines.
              ;; Overdue deadlines get the highest priority
              ;; increase, then imminent deadlines and eventually
              ;; more distant deadlines.
              (let ((adjust (if today? (- diff) 0)))
                (+ adjust (org-get-priority item)))
              'todo-state todo-state
              'type (if upcoming? "upcoming-deadline" "deadline")
              'date (if upcoming? date deadline)
              'face (if done? 'org-agenda-done face)
              'undone-face face
              'done-face 'org-agenda-done)
            (push item deadline-items)))))
    (nreverse deadline-items)))

(defun orb-agenda-get-scheduled (&optional deadlines with-hour)
  "Return the scheduled information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.  When WITH-HOUR is non-nil, only return
scheduled items with an hour specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'done-face 'org-agenda-done
                      'mouse-face 'highlight
                      'help-echo
                      (format "mouse-2 or RET jump to Org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         (today (org-today))
         (todayp (org-agenda-today-p date)) ; DATE bound by calendar.
         (current (calendar-absolute-from-gregorian date))
         (deadline-pos
          (mapcar (lambda (d)
                    (let ((m (get-text-property 0 'org-hd-marker d)))
                      (orb-agenda--marker-position m)))
                  deadlines))
         scheduled-items)
    (dolist (node-struct
             (orb-agenda--get-node-scheduled
              orb-agenda--current-file
              (orb-db-concat-query
               " AND node.scheduled IS NOT NULL"
               (when with-hour
                 " AND scheduled.start_time LIKE '%%:%%'")
               (when org-agenda-skip-scheduled-if-done
                 (list
                  " AND coalesce(node.todo NOT IN (SELECT value FROM json_each(?)), TRUE)"
                  (json-encode org-done-keywords))))))
      (catch :skip
        (let* ((deadline (orb-agenda-get-property node-struct "DEADLINE"))
               (pos (plist-get (orb-agenda--node-scheduled node-struct) :begin))
               (s (substring (plist-get (orb-agenda--node-scheduled node-struct) :raw-value) 1 -1))
               (todo-state (orb-agenda--node-todo node-struct))
               (donep (member todo-state org-done-keywords))
               (sexp? (string-prefix-p "%%" s))
               (marker (make-orb-agenda--marker :file orb-agenda--current-file :position pos :node node-struct))
               ;; SCHEDULE is the scheduled date for the entry.  It is
               ;; either the bare date or the last repeat, according
               ;; to `org-agenda-prefer-last-repeat'.
               (schedule
                (cond
                 (sexp? (org-agenda--timestamp-to-absolute s current))
                 ((or (eq org-agenda-prefer-last-repeat t)
                      (member todo-state org-agenda-prefer-last-repeat))
                  (org-agenda--timestamp-to-absolute
                   s today 'past (orb-agenda--file-path orb-agenda--current-file) pos))
                 (t (org-agenda--timestamp-to-absolute s))))
               ;; REPEAT is the future repeat closest from CURRENT,
               ;; according to `org-agenda-show-future-repeats'. If
               ;; the latter is nil, or if the time stamp has no
               ;; repeat part, default to SCHEDULE.
               (repeat
                (cond
                 (sexp? schedule)
                 ((<= current today) schedule)
                 ((not org-agenda-show-future-repeats) schedule)
                 (t
                  (let ((base (if (eq org-agenda-show-future-repeats 'next)
                                  (1+ today)
                                current)))
                    (org-agenda--timestamp-to-absolute
                     s base 'future (orb-agenda--file-path orb-agenda--current-file) pos)))))
               (diff (- current schedule))
               (warntime (orb-agenda-get-property node-struct "APPT_WARNTIME" 'selective))
               (pastschedp (< schedule today))
               (futureschedp (> schedule today))
               (habitp (orb-agenda--node-habit node-struct))
               (suppress-delay
                (let ((deadline (and org-agenda-skip-scheduled-delay-if-deadline deadline)))
                  (cond
                   ((not deadline) nil)
                   ;; The current item has a deadline date, so
                   ;; evaluate its delay time.
                   ((integerp org-agenda-skip-scheduled-delay-if-deadline)
                    ;; Use global delay time.
                    (- org-agenda-skip-scheduled-delay-if-deadline))
                   ((eq org-agenda-skip-scheduled-delay-if-deadline
                        'post-deadline)
                    ;; Set delay to no later than DEADLINE.
                    (min (- schedule
                            (org-agenda--timestamp-to-absolute deadline))
                         org-scheduled-delay-days))
                   (t 0))))
               (ddays
                (cond
                 ;; Nullify delay when a repeater triggered already
                 ;; and the delay is of the form --Xd.
                 ((and (string-match-p "--[0-9]+[hdwmy]" s)
                       (> schedule (org-agenda--timestamp-to-absolute s)))
                  0)
                 (suppress-delay
                  (let ((org-scheduled-delay-days suppress-delay))
                    (org-get-wdays s t t)))
                 (t (org-get-wdays s t)))))
          ;; Display scheduled items at base date (SCHEDULE), today if
          ;; scheduled before the current date, and at any repeat past
          ;; today.  However, skip delayed items and items that have
          ;; been displayed for more than `org-scheduled-past-days'.
          (unless (and todayp
                       habitp
                       (bound-and-true-p org-habit-show-all-today))
            (when (or (and (> ddays 0) (< diff ddays))
                      (> diff (or (and habitp org-habit-scheduled-past-days)
                                  org-scheduled-past-days))
                      (> schedule current)
                      (and (/= current schedule)
                           (/= current today)
                           (/= current repeat)))
              (throw :skip nil)))
          ;; Possibly skip done tasks.
          (when (and donep
                     (or org-agenda-skip-scheduled-if-done
                         (/= schedule current)))
            (throw :skip nil))
          ;; Skip entry if it already appears as a deadline, per
          ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
          ;; doesn't apply to habits.
          (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
                  ((guard
                    (or (not (memq (orb-agenda--node-point node-struct) deadline-pos))
                        habitp))
                   nil)
                  (`repeated-after-deadline
                   (let ((deadline (org-agenda--timestamp-to-absolute deadline)))
                     (and (<= schedule deadline) (> current deadline))))
                  (`not-today pastschedp)
                  (`t t)
                  (_ nil))
            (throw :skip nil))
          ;; Skip habits if `org-habit-show-habits' is nil, or if we
          ;; only show them for today.  Also skip done habits.
          (when (and habitp
                     (or donep
                         (not (bound-and-true-p org-habit-show-habits))
                         (and (not todayp)
                              (bound-and-true-p
                               org-habit-show-habits-only-for-today))))
            (throw :skip nil))

          (let* ((category (orb-agenda--get-category node-struct))
                 (effort (orb-agenda-get-property node-struct org-effort-property))
                 (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
                 (hdmarker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
                 (inherited-tags
                  (or (eq org-agenda-show-inherited-tags 'always)
                      (and (listp org-agenda-show-inherited-tags)
                           (memq 'todo org-agenda-show-inherited-tags))
                      (and (eq org-agenda-show-inherited-tags t)
                           (or (eq org-agenda-use-tag-inheritance t)
                               (memq 'agenda org-agenda-use-tag-inheritance)))))
                 (tags (orb-agenda--get-tags node-struct inherited-tags))
                 (level (make-string (org-reduced-level
                                      (orb-agenda--node-level node-struct))
                                     ?\s))
                 (olp (orb-agenda--node-olp node-struct))
                 (head (concat todo-state " " (orb-agenda--node-text node-struct)))
                 (time
                  (save-match-data
                    (cond
                     ;; No time of day designation if it is only a
                     ;; reminder, except for habits, which always show
                     ;; the time of day.  Habits are an exception
                     ;; because if there is a time of day, that is
                     ;; interpreted to mean they should usually happen
                     ;; then, even if doing the habit was missed.
                     ((and
                       (not habitp)
                       (/= current schedule)
                       (/= current repeat))
                      nil)
                     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
                      (concat (substring s (match-beginning 1)) " "))
                     (t 'time))))
                 (item
                  (orb-agenda-format-item
                   (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
                     ;; Show a reminder of a past scheduled today.
                     (if (and todayp pastschedp)
                         (format past diff)
                       first))
                   (org-add-props head nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   level category tags time nil habitp
                   olp))
                 (face (cond
                        ((and (not habitp) pastschedp)
                         'org-scheduled-previously)
                        ((and habitp futureschedp)
                         'org-agenda-done)
                        (todayp 'org-scheduled-today)
                        (t 'org-scheduled))))

            (org-add-props item props
              'undone-face face
              'face (if donep 'org-agenda-done face)
              'org-marker marker
              'org-hd-marker hdmarker
              'type (if pastschedp "past-scheduled" "scheduled")
              'date (if pastschedp schedule date)
              'warntime warntime
              'ts-date schedule
              'level level
              'effort effort 'effort-minutes effort-minutes
              'priority (if habitp (org-habit-get-priority habitp)
                          (+ 99 diff (org-get-priority item)))
              'org-habit-p habitp
              'todo-state todo-state)
            (push item scheduled-items)))))
    (nreverse scheduled-items)))

(defun orb-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (with-no-warnings (defvar date))
  (let* ((props (list 'face nil
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'mouse-face 'highlight
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name (orb-agenda--file-path orb-agenda--current-file)))))
         (d0 (calendar-absolute-from-gregorian date))
         ee)
    (pcase-dolist
        (`(,node-struct . ,blocks)
         (orb-agenda--get-node-blocks
          orb-agenda--current-file
          (orb-agenda--format-date date)))
      (let* ((todo-state (orb-agenda--node-todo node-struct))
             (category (orb-agenda--get-category node-struct))
             (effort (orb-agenda-get-property node-struct org-effort-property))
             (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
             (hdmarker (make-orb-agenda--marker :file orb-agenda--current-file :position (orb-agenda--node-point node-struct) :node node-struct))
             (inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'todo org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'agenda org-agenda-use-tag-inheritance)))))
             (tags (orb-agenda--get-tags node-struct inherited-tags))
             (level (make-string (org-reduced-level
                                  (orb-agenda--node-level node-struct))
                                 ?\s))
             (olp (orb-agenda--node-olp node-struct))
             (head (concat todo-state " " (orb-agenda--node-text node-struct))))
        (pcase-dolist
            ((map (:end pos) (:start-time s1) (:end-time s2))
             (seq-sort-by (lambda (block) (plist-get block :end)) '< blocks))
          (let* ((d1 (time-to-days (org-time-string-to-time s1)))
                 (d2 (time-to-days (org-time-string-to-time s2))))
            (let* ((marker (make-orb-agenda--marker :file orb-agenda--current-file :position pos :node node-struct))
                   (remove-re
                    (if org-agenda-remove-timeranges-from-blocks
                        (concat
                         (regexp-quote s1)
                         "--"
                         (regexp-quote s2))
                      nil))
                   (txt (orb-agenda-format-item
                         (format (nth (if (= d1 d2) 0 1)
                                      org-agenda-timerange-leaders)
                                 (1+ (- d0 d1)) (1+ (- d2 d1)))
                         (org-add-props head nil
                           'effort effort
                           'effort-minutes effort-minutes)
                         level category tags
                         (save-match-data
                           (let ((hhmm1 (and (string-match org-ts-regexp1 s1)
                                             (match-string 6 s1)))
                                 (hhmm2 (and (string-match org-ts-regexp1 s2)
                                             (match-string 6 s2))))
                             (cond ((string= hhmm1 hhmm2)
                                    (concat s1 "--" s2))
                                   ((and (= d1 d0) (= d2 d0))
                                    (concat s1 "--" s2))
                                   ((= d1 d0)
                                    s1)
                                   ((= d2 d0)
                                    s2))))
                         remove-re
                         nil olp)))

              (org-add-props txt props
                'org-marker marker 'org-hd-marker hdmarker
                'type "block" 'date date
                'level level
                'effort effort 'effort-minutes effort-minutes
                'todo-state todo-state
                'priority (org-get-priority txt))
              (push txt ee))))))
    (nreverse ee)))

(defun orb-agenda--get-day-entries-a (fn file date &rest args)
  (if (or (and org-agenda-skip-function-global
               (not orb-agenda--skip-function-global))
          (and org-agenda-skip-function
               (not orb-agenda--skip-function))
          (org-find-base-buffer-visiting file)
          (not (orb-file-p file)))
      (apply fn file date args)
    (let ((orb-agenda--current-file (orb-agenda--get-file file)))
      (if (not orb-agenda--current-file)
          (apply fn file date args)
        (unless
            (and org-agenda-skip-archived-trees
                 (not org-agenda-archives-mode)
                 (member org-archive-tag (orb-agenda--file-tags orb-agenda--current-file)))
          (setf org-agenda-current-date date)
          ;; Rationalize ARGS.  Also make sure `:deadline' comes
          ;; first in order to populate DEADLINES before passing it.
          ;;
          ;; We use `delq' since `org-uniquify' duplicates ARGS,
          ;; guarding us from modifying `org-agenda-entry-types'.
          (setf args (org-uniquify (or args org-agenda-entry-types)))
          (when (and (memq :scheduled args) (memq :scheduled* args))
            (setf args (delq :scheduled* args)))
          (cond
           ((memq :deadline args)
            (setf args (cons :deadline
                             (delq :deadline (delq :deadline* args)))))
           ((memq :deadline* args)
            (setf args (cons :deadline* (delq :deadline* args)))))

          (let* ((org-todo-keywords-1 (orb-agenda--file-todo-keywords orb-agenda--current-file))
                 (org-done-keywords (orb-agenda--file-done-keywords orb-agenda--current-file))
                 (org-todo-regexp (regexp-opt org-todo-keywords-1 t))
                 (org-not-done-regexp
                  (regexp-opt (seq-difference org-todo-keywords-1 org-done-keywords) t))
                 (org-complex-heading-regexp
                  (concat "^\\(\\*+\\)"
                          "\\(?: +" org-todo-regexp "\\)?"
                          "\\(?: +\\(\\[#.\\]\\)\\)?"
                          "\\(?: +\\(.*?\\)\\)??"
                          "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
                          "[ \t]*$")))
            ;; Collect list of headlines.  Return them flattened.
            (let ((case-fold-search nil) results deadlines)
              (org-dlet
                  ((date date))
                (dolist (arg args (apply #'append (remq nil results)))
                  (pcase arg
                    ((and :todo (guard (org-agenda-today-p date)))
                     (push (orb-agenda-get-todos) results))
                    (:timestamp
                     (push (orb-agenda-get-blocks) results)
                     (push (orb-agenda-get-timestamps deadlines) results))
                    (:sexp
                     (push (orb-agenda-get-sexps) results))
                    (:scheduled
                     (push (orb-agenda-get-scheduled deadlines) results))
                    (:scheduled*
                     (push (orb-agenda-get-scheduled deadlines t) results))
                    (:closed
                     (push (orb-agenda-get-progress) results))
                    (:deadline
                     (setf deadlines (orb-agenda-get-deadlines))
                     (push deadlines results))
                    (:deadline*
                     (setf deadlines (orb-agenda-get-deadlines t))
                     (push deadlines results))))))))))))

(defun orb-agenda--scan-tags (matcher matcher-query todo-only)
  (let ((props (list 'face 'default
                      'done-face 'org-agenda-done
                      'undone-face 'default
                      'mouse-face 'highlight
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'help-echo
                      (format "mouse-2 or RET jump to Org file %S"
                              (abbreviate-file-name
                               (orb-agenda--file-path orb-agenda--current-file)))))
        rtn)
    (dolist
        (node-struct
         (orb-agenda--get-node-scheduled
          orb-agenda--current-file
          (orb-db-concat-query
           (when todo-only
             " AND node.todo IS NOT NULL")
           (when matcher-query
             (orb-db-concat-query " AND " matcher-query)))
          todo-only
          "JOIN %s.file AS file ON file.file_id = node.file_id"))
      (let* ((todo (orb-agenda--node-todo node-struct))
             (tags-list (orb-agenda--get-tags node-struct t))
             (level (orb-agenda--node-level node-struct)))
        (when
            (or matcher-query
                (if (functionp matcher)
                    (cl-letf ((case-fold-search t)
                              (org-trust-scanner-tags t)
                              ((symbol-function 'org-get-category)
                               (lambda (_)
                                 (orb-agenda--get-category node-struct)))
                              ((symbol-function 'org-cached-entry-get)
                               (lambda (_ prop)
                                 (orb-agenda-get-property node-struct prop 'selective))))
                      (funcall matcher todo tags-list level))
                  matcher))
          (let* ((lspos (orb-agenda--node-point node-struct))
                 (category (orb-agenda--get-category node-struct))
                 (effort (orb-agenda-get-property node-struct org-effort-property))
                 (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
                 (ts-date-pair (orb-agenda-entry-get-agenda-timestamp node-struct))
                 (ts-date (car ts-date-pair))
                 (ts-date-type (cdr ts-date-pair))
                 (txt
                  (org-agenda-format-item
                   ""
                   (org-add-props
                       (concat
                        (if (eq org-tags-match-list-sublevels 'indented)
                            (make-string (1- level) ?.) "")
                        (concat todo  " " (orb-agenda--node-text node-struct)))
                       nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   (make-string level ?\s)
                   category
                   tags-list))
                 (priority (org-get-priority txt))
                 (marker (make-orb-agenda--marker
                          :file orb-agenda--current-file
                          :position lspos
                          :node node-struct)))
            (org-add-props txt props
              'effort effort 'effort-minutes effort-minutes
              'org-marker marker 'org-hd-marker marker 'org-category category
              'todo-state todo
              'ts-date ts-date
              'priority priority
              'type (concat "tagsmatch" ts-date-type))
            (push txt rtn)))))
    (nreverse rtn)))

(defun orb-agenda--get-tags-entries-from-buffer (file matcher todo-only)
  (unless (file-exists-p file)
    (error "No such file %s" file))
  (if-let ((buffer (org-get-agenda-file-buffer file)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode)
          (error "Agenda file %s is not in Org mode" file))
        (save-excursion
          (save-restriction
            (if (eq buffer org-agenda-restrict)
                (narrow-to-region org-agenda-restrict-begin
                                  org-agenda-restrict-end)
              (widen))
            (org-scan-tags 'agenda matcher todo-only))))
    (list (format "ORG-AGENDA-ERROR: No such org-file %s" file))))

(defun orb-agenda--get-tags-entries (file matcher matcher-query todo-only)
  (if (or (and org-agenda-skip-function-global
               (not orb-agenda--skip-function-global))
          (and org-agenda-skip-function
               (not orb-agenda--skip-function))
          (org-find-base-buffer-visiting file)
          (not (orb-file-p file)))
      (orb-agenda--get-tags-entries-from-buffer file matcher todo-only)
    (let ((orb-agenda--current-file (orb-agenda--get-file file)))
      (if (not orb-agenda--current-file)
          (orb-agenda--get-tags-entries-from-buffer file matcher todo-only)
        (unless
            (and org-agenda-skip-archived-trees
                 (not org-agenda-archives-mode)
                 (member org-archive-tag (orb-agenda--file-tags orb-agenda--current-file)))
          (let* ((org-todo-keywords-1 (orb-agenda--file-todo-keywords orb-agenda--current-file))
                 (org-done-keywords (orb-agenda--file-done-keywords orb-agenda--current-file))
                 (org-todo-regexp (regexp-opt org-todo-keywords-1 t))
                 (org-not-done-regexp
                  (regexp-opt (seq-difference org-todo-keywords-1 org-done-keywords) t))
                 (org-complex-heading-regexp
                  (concat "^\\(\\*+\\)"
                          "\\(?: +" org-todo-regexp "\\)?"
                          "\\(?: +\\(\\[#.\\]\\)\\)?"
                          "\\(?: +\\(.*?\\)\\)??"
                          "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
                          "[ \t]*$")))
            (orb-agenda--scan-tags matcher matcher-query todo-only)))))))


(defun orb-agenda--op-to-query (op)
  (pcase op
    ((or '< 'org-string< 'org-time<) "<")
    ((or '> 'org-string> 'org-time>) ">")
    ((or '<= 'org-string<= 'org-time<=) "<=")
    ((or '>= 'org-string>= 'org-time>=) ">=")
    ((or '= 'string= 'org-time=) "=")
    ((or '/= 'org-string<> 'org-time<>) "!=")))

(defun orb-agenda--matcher-to-query (matcher)
  (pcase matcher
    (`(not ,e)
     (orb-db-concat-query "NOT (" (orb-agenda--matcher-to-query e) ")"))
    (`(and . ,rest)
     (orb-agenda--join-queries
      (mapcar 'orb-agenda--matcher-to-query rest)
      "AND"))
    (`(or . ,rest)
     (orb-agenda--join-queries
      (mapcar 'orb-agenda--matcher-to-query rest)
      "OR"))
    (`(member ,tag tags-list)
     (orb-db-concat-query
      "EXISTS (SELECT 1 FROM json_each("
      (if (org-tag-inherit-p tag) "alltags" "node.tags")
      (list ") WHERE value = ?)" tag)))
    (`(,(or '< '> '<= '>= '= '/=) level ,value)
     (list
      (format "node.level %s ?" (orb-agenda--op-to-query (car matcher)))
      value))
    (`(,(or 'org-string< 'org-string>
            'org-string<= 'org-string>=
            'string= 'org-string<>)
       (or todo "") ,value)
     (list
      (format "coalesce(node.todo, '') %s ?" (orb-agenda--op-to-query (car matcher)))
      value))
    ((and `(,(or '< '> '<= '>= '= '/=)
            (string-to-number (or (org-cached-entry-get nil ,prop) "")) ,value)
          (guard (not (member prop org-special-properties))))
     (list
      (format
       "CAST(coalesce(json_extract(%s, '$.' || ?), '') AS NUMERIC) %s ?"
       (if (org-property-inherit-p prop) "allprops" "props")
       (orb-agenda--op-to-query (car matcher)))
      prop value))
    (`(string= (or (org-get-category (point)) "") ,value)
     (list
      "coalesce(json_extract(allprops, '$.CATEGORY') = ?,
(substring(CASE WHEN file.levels IS NULL THEN file.path
ELSE substring(file.path, 1 + json_extract(file.levels, '$[0]'))
END, 1, ?) = ?) AND
(instr(substring(CASE WHEN file.levels IS NULL THEN file.path
ELSE substring(file.path, 1 + json_extract(file.levels, '$[0]'))
END, ?), '.') = 0))"
      value (1+ (length value)) (concat value ".") (+ 2 (length value))))
    ((and `(,(or 'org-string< 'org-string>
                 'org-string<= 'org-string>=
                 'string= 'org-string<>)
            (or (org-cached-entry-get nil ,prop) "") ,value)
          (guard (not (member prop org-special-properties))))
     (list
      (format
       "coalesce(json_extract(%s, '$.' || ?), '') %s ?"
       (if (org-property-inherit-p prop) "allprops" "props")
       (orb-agenda--op-to-query (car matcher)))
      prop value))
    (`(,(or 'org-time< 'org-time>
            'org-time<= 'org-time>=
            'org-time= 'org-time<>)
       (or (org-cached-entry-get nil ,(or "SCHEDULED" "DEADLINE" "CLOSED" "TIMESTAMP" "TIMESTAMP_IA")) "")
       ,value)
     (let ((table (downcase (caddr (cadr (cadr matcher))))))
       (list
        (format
         "(%s.start_epoch IS NOT NULL) AND (%s.start_epoch %s ?)"
         table table (orb-agenda--op-to-query (car matcher)))
        value)))
    ((and `(,(or 'org-time< 'org-time>
                 'org-time<= 'org-time>=
                 'org-time= 'org-time<>)
            (or (org-cached-entry-get nil ,prop) "") ,value)
          (guard (not (member prop org-special-properties))))
     (list
      (format "EXISTS (
SELECT 1
FROM %%s.property AS prop
JOIN %%s.timestamp AS tscompare
ON prop.node_id = tscompare.node_id
AND tscompare.begin >= prop.begin
AND tscompare.end <= prop.end
WHERE prop.node_id = node.node_id
AND prop.key = ?
AND tscompare.start_epoch IS NOT NULL
AND tscompare.start_epoch %s ?)"
              (orb-agenda--op-to-query (car matcher)))
      prop value))
    (_
     (throw :not-supported nil))))

(defun orb-agenda--tags-view-a (&optional todo-only match)
  (when org-agenda-overriding-arguments
    (setq todo-only (car org-agenda-overriding-arguments)
          match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
          org-tags-match-list-sublevels)
         (completion-ignore-case t)
         (org--matcher-tags-todo-only todo-only)
         rtn rtnall files file pos matcher matcher-query)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (catch 'exit
      (setq org-agenda-buffer-name
            (org-agenda--get-buffer-name
             (and org-agenda-sticky
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m"))
                              match)
                    (format "*Org Agenda(%s)*"
                            (or (and todo-only "M") "m"))))))
      (setq matcher (org-make-tags-matcher match))
      (setq matcher-query
            (pcase-let ((`(_ lambda _ (progn _ ,body)) matcher))
              (catch :not-supported
                (orb-agenda--matcher-to-query body))))
      ;; Prepare agendas (and `org-tag-alist-for-agenda') before
      ;; expanding tags within `org-make-tags-matcher'
      (org-agenda-prepare (concat "TAGS " match))
      (setq match (car matcher)
            matcher (cdr matcher))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
            (list 'org-tags-view
                  `(quote ,org--matcher-tags-todo-only)
                  `(if current-prefix-arg nil ,org-agenda-query-string)))
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (orb-agenda--get-tags-entries file matcher matcher-query org--matcher-tags-todo-only))
          (setq rtnall (append rtnall rtn))))
      (org-agenda--insert-overriding-header
        (with-temp-buffer
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure-filter))
          (setq pos (point))
          (unless org-agenda-multi
            (insert (substitute-command-keys
                     "Press \
\\<org-agenda-mode-map>`\\[universal-argument] \\[org-agenda-redo]' \
to search again\n")))
          (add-text-properties pos (1- (point))
                               (list 'face 'org-agenda-structure-secondary))
          (buffer-string)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
        (insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties
       (point-min) (point-max)
       `(org-agenda-type tags
                         org-last-args (,org--matcher-tags-todo-only ,match)
                         org-redo-cmd ,org-agenda-redo-command
                         org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))


(defun orb-agenda--show-clocking-issues-a ()
  (let* ((pl org-agenda-clock-consistency-checks)
         (re (concat "^[ \t]*"
                     org-clock-string
                     "[ \t]+"
                     "\\(\\[.*?\\]\\)"        ; group 1 is first stamp
                     "\\(-\\{1,3\\}\\(\\[.*?\\]\\)\\)?")) ; group 3 is second
         (tlstart 0.)
         (tlend 0.)
         (maxtime (org-duration-to-minutes
                   (or (plist-get pl :max-duration) "24:00")))
         (mintime (org-duration-to-minutes
                   (or (plist-get pl :min-duration) 0)))
         (maxgap  (org-duration-to-minutes
                   ;; default 30:00 means never complain
                   (or (plist-get pl :max-gap) "30:00")))
         (gapok (mapcar #'org-duration-to-minutes
                        (plist-get pl :gap-ok-around)))
         (def-face (or (plist-get pl :default-face)
                       '((:background "DarkRed") (:foreground "white"))))
         issue face m te ts dt ov)
    (goto-char (point-min))
    (while (re-search-forward " Clocked: +(\\(?:-\\|\\([0-9]+:[0-9]+\\)\\))" nil t)
      (setq issue nil face def-face)
      (catch 'next
        (setq m (org-get-at-bol 'org-marker)
              te nil ts nil)
        (if (orb-agenda--marker-p m)
            (let ((time-stamp (orb-agenda--marker-auxiliary m)))
              (setq ts (plist-get time-stamp :start-epoch)
                    te (plist-get time-stamp :end-epoch))
              (unless te
                (setq issue
                      (format
                       "No end time: (%s)"
                       (org-duration-from-minutes
                        (floor
                         (- (float-time (org-current-time))
                            ts)
                         60)))
                      face (or (plist-get pl :no-end-time-face) face))
                (throw 'next t))
              (setq dt (- te ts)))
          (unless (and m (markerp m))
            (setq issue "No valid clock line") (throw 'next t))
          (org-with-point-at m
            (save-excursion
              (goto-char (line-beginning-position))
              (unless (looking-at re)
                (error "No valid Clock line")
                (throw 'next t))
              (unless (match-end 3)
                (setq issue
                      (format
                       "No end time: (%s)"
                       (org-duration-from-minutes
                        (floor
                         (- (float-time (org-current-time))
                            (float-time (org-time-string-to-time (match-string 1))))
                         60)))
                      face (or (plist-get pl :no-end-time-face) face))
                (throw 'next t))
              (setq ts (match-string 1)
                    te (match-string 3)
                    ts (float-time (org-time-string-to-time ts))
                    te (float-time (org-time-string-to-time te))
                    dt (- te ts)))))
        (cond
         ((> dt (* 60 maxtime))
          ;; a very long clocking chunk
          (setq issue (format "Clocking interval is very long: %s"
                              (org-duration-from-minutes (floor dt 60)))
                face (or (plist-get pl :long-face) face)))
         ((< dt (* 60 mintime))
          ;; a very short clocking chunk
          (setq issue (format "Clocking interval is very short: %s"
                              (org-duration-from-minutes (floor dt 60)))
                face (or (plist-get pl :short-face) face)))
         ((and (> tlend 0) (< ts tlend))
          ;; Two clock entries are overlapping
          (setq issue (format "Clocking overlap: %d minutes"
                              (/ (- tlend ts) 60))
                face (or (plist-get pl :overlap-face) face)))
         ((and (> tlend 0) (> ts (+ tlend (* 60 maxgap))))
          ;; There is a gap, lets see if we need to report it
          (unless (org-agenda-check-clock-gap tlend ts gapok)
            (setq issue (format "Clocking gap: %d minutes"
                                (/ (- ts tlend) 60))
                  face (or (plist-get pl :gap-face) face))))
         (t nil)))
      (setq tlend (or te tlend) tlstart (or ts tlstart))
      (when issue
        ;; OK, there was some issue, add an overlay to show the issue
        (setq ov (make-overlay (line-beginning-position) (line-end-position)))
        (overlay-put ov 'before-string
                     (concat
                      (org-add-props
                          (format "%-43s" (concat " " issue))
                          nil
                        'face face)
                      "\n"))
        (overlay-put ov 'evaporate t)))))

(defun orb-agenda--todo-keywords-1 (marker)
  (if (orb-agenda--marker-p marker)
      (orb-agenda--file-todo-keywords
       (orb-agenda--marker-file marker))
    (when-let ((f (and marker (marker-buffer marker))))
      (with-current-buffer f
        org-todo-keywords-1))))

(defun orb-agenda--cmp-todo-state (a b)
  "Compare the todo states of strings A and B."
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (todo-kwds
          (or (orb-agenda--todo-keywords-1 ma)
              (orb-agenda--todo-keywords-1 mb)))
         (ta (or (get-text-property 1 'todo-state a) ""))
         (tb (or (get-text-property 1 'todo-state b) ""))
         (la (- (length (member ta todo-kwds))))
         (lb (- (length (member tb todo-kwds))))
         (donepa (member ta org-done-keywords-for-agenda))
         (donepb (member tb org-done-keywords-for-agenda)))
    (cond ((and donepa (not donepb)) -1)
          ((and (not donepa) donepb) +1)
          ((< la lb) -1)
          ((< lb la) +1))))


(defun orb-agenda--entries-lessp-a (a b)
  ;; The following variables will be used when the form is evaluated.
  ;; So even though the compiler complains, keep them.
  (let ((ss org-agenda-sorting-strategy-selected))
    (org-dlet
        ((timestamp-up    (and (org-em 'timestamp-up 'timestamp-down ss)
                               (org-cmp-ts a b "")))
         (timestamp-down  (if timestamp-up (- timestamp-up) nil))
         (scheduled-up    (and (org-em 'scheduled-up 'scheduled-down ss)
                               (org-cmp-ts a b "scheduled")))
         (scheduled-down  (if scheduled-up (- scheduled-up) nil))
         (deadline-up     (and (org-em 'deadline-up 'deadline-down ss)
                               (org-cmp-ts a b "deadline")))
         (deadline-down   (if deadline-up (- deadline-up) nil))
         (tsia-up         (and (org-em 'tsia-up 'tsia-down ss)
                               (org-cmp-ts a b "timestamp_ia")))
         (tsia-down       (if tsia-up (- tsia-up) nil))
         (ts-up           (and (org-em 'ts-up 'ts-down ss)
                               (org-cmp-ts a b "timestamp")))
         (ts-down         (if ts-up (- ts-up) nil))
         (time-up         (and (org-em 'time-up 'time-down ss)
                               (org-cmp-time a b)))
         (time-down       (if time-up (- time-up) nil))
         (stats-up        (and (org-em 'stats-up 'stats-down ss)
                               (org-cmp-values a b 'org-stats)))
         (stats-down      (if stats-up (- stats-up) nil))
         (priority-up     (and (org-em 'priority-up 'priority-down ss)
                               (org-cmp-values a b 'priority)))
         (priority-down   (if priority-up (- priority-up) nil))
         (effort-up       (and (org-em 'effort-up 'effort-down ss)
                               (org-cmp-effort a b)))
         (effort-down     (if effort-up (- effort-up) nil))
         (category-up     (and (or (org-em 'category-up 'category-down ss)
                                   (memq 'category-keep ss))
                               (org-cmp-category a b)))
         (category-down   (if category-up (- category-up) nil))
         (category-keep   (if category-up +1 nil))
         (tag-up          (and (org-em 'tag-up 'tag-down ss)
                               (org-cmp-tag a b)))
         (tag-down        (if tag-up (- tag-up) nil))
         (todo-state-up   (and (org-em 'todo-state-up 'todo-state-down ss)
                               (orb-agenda--cmp-todo-state a b)))
         (todo-state-down (if todo-state-up (- todo-state-up) nil))
         (habit-up        (and (org-em 'habit-up 'habit-down ss)
                               (org-cmp-habit-p a b)))
         (habit-down      (if habit-up (- habit-up) nil))
         (alpha-up        (and (org-em 'alpha-up 'alpha-down ss)
                               (org-cmp-alpha a b)))
         (alpha-down      (if alpha-up (- alpha-up) nil))
         (need-user-cmp   (org-em 'user-defined-up 'user-defined-down ss))
         user-defined-up user-defined-down)
      (when (and need-user-cmp org-agenda-cmp-user-defined
                 (functionp org-agenda-cmp-user-defined))
        (setq user-defined-up
              (funcall org-agenda-cmp-user-defined a b)
              user-defined-down (if user-defined-up (- user-defined-up) nil)))
      (cdr (assoc
            (eval (cons 'or org-agenda-sorting-strategy-selected) t)
            '((-1 . t) (1 . nil) (nil . nil)))))))


(defun orb-agenda--find-top-headline-a (fn &optional pos)
  (if (not (orb-agenda--marker-p pos))
      (funcall fn pos)
    (ignore-errors
      (or (car
           (orb-agenda--node-olp
            (orb-agenda--marker-node pos)))
          (orb-agenda--node-heading
           (orb-agenda--marker-node pos))))))


(defun orb-agenda--display-outline-path (marker file-or-title)
  (let* ((file (orb-agenda--marker-file marker))
         (bfn (orb-agenda--file-path file))
         (title-prop (when (eq file-or-title 'title) (gethash "TITLE" (orb-agenda--file-keywords file))))
         (path (orb-agenda--node-olp (orb-agenda--marker-node marker)))
         (res
          (org-format-outline-path
           path
           (1- (frame-width))
           (and file-or-title bfn (concat (if (and (eq file-or-title 'title) title-prop)
                                              title-prop
                                            (file-name-nondirectory bfn))
                                          nil))
           nil)))
    (add-face-text-property 0 (length res)
                            `(:height ,(face-attribute 'default :height))
                            nil res)
    (org-unlogged-message "%s" res)))

(defun orb-agenda--do-context-action-a ()
  (let ((m (org-get-at-bol 'org-marker)))
    (cond
     ((orb-agenda--marker-p m)
      (and org-agenda-follow-mode
           (if org-agenda-follow-indirect
               (org-agenda-tree-to-indirect-buffer nil)
             (org-agenda-show)))
      (and org-agenda-show-outline-path
           (orb-agenda--display-outline-path m org-agenda-show-outline-path)))
     ((and (markerp m) (marker-buffer m))
      (and org-agenda-follow-mode
           (if org-agenda-follow-indirect
               (org-agenda-tree-to-indirect-buffer nil)
             (org-agenda-show)))
      (and org-agenda-show-outline-path
           (org-with-point-at m (org-display-outline-path org-agenda-show-outline-path)))))))


(defun orb-agenda--bulk-action-a (&optional _arg)
  (if (not org-agenda-bulk-marked-entries)
      (save-excursion (org-agenda-bulk-mark)))
  (setq org-agenda-bulk-marked-entries
        (mapcar 'orb-agenda--get-real-marker org-agenda-bulk-marked-entries)))

(defun orb-agenda--get-columns-default-format (m)
  (if (orb-agenda--marker-p m)
      (or (orb-agenda-get-property (orb-agenda--marker-node m) "COLUMNS" t)
          (gethash "COLUMNS" (orb-agenda--file-keywords (orb-agenda--marker-file m)))
          org-columns-default-format)
    (or (org-entry-get m "COLUMNS" t)
        (with-current-buffer (marker-buffer m)
          org-columns-default-format))))

(defun orb-agenda--collect-values (m compiled-fmt)
  (let ((summaries (orb-agenda--node-summaries (orb-agenda--marker-node m))))
    (mapcar
     (lambda (spec)
       (pcase spec
         (`(,p . ,_)
          (let* ((v (or (cdr (assoc spec summaries))
                        (orb-agenda-get-property (orb-agenda--marker-node m) p 'selective t)
                        (and compiled-fmt ;assume `org-agenda-columns'
                             ;; Effort property is not defined.  Try
                             ;; to use appointment duration.
                             org-agenda-columns-add-appointments-to-effort-sum
                             (string= p (upcase org-effort-property))
                             (get-text-property (point) 'duration)
                             (propertize (org-duration-from-minutes
                                          (get-text-property (point) 'duration))
                                         'face 'org-warning))
                        "")))
            (list spec v (org-columns--displayed-value spec v compiled-fmt))))))
     compiled-fmt)))

(defun orb-agenda-colview-compute (fmt)
  (when (seq-some
           (lambda (spec)
             (or (nth 3 spec)
                 (member (car spec) '("CLOCKSUM" "CLOCKSUM_T"))))
           fmt)
    (let* ((files (seq-filter 'org-find-base-buffer-visiting org-agenda-contributing-files))
           (markers nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((m (org-get-at-bol 'org-hd-marker)))
            (if (orb-agenda--marker-p m)
                (push m markers)))
          (forward-line)))
      (pcase-dolist
          (`(file . ,markers)
           (seq-group-by
            (lambda (m) (orb-agenda--file-path (orb-agenda--marker-file m)))
            markers))
        (let* ((nodes (mapcar 'orb-agenda--marker-node markers))
               (file (orb-agenda--marker-file (car markers)))
               (file-fmt (org-columns-compile-format
                          (or (orb-agenda--file-columns file)
                              (gethash "COLUMNS" (orb-agenda--file-keywords file))
                              org-columns-default-format)))
               (clocksum-props
                (seq-filter (lambda (p) (assoc p fmt)) '("CLOCKSUM" "CLOCKSUM_T")))
               (props (seq-difference
                       (mapcar 'car
                               (seq-filter
                                (lambda (spec)
                                  (and (not (member (car spec) clocksum-props))
                                       (nth 3 spec)
                                       (let ((a (assoc (car spec) file-fmt)))
                                         (equal (nth 3 a) (nth 3 spec)))))
                                fmt))
                       ;; Special properties cannot be collected nor summarized, as
                       ;; they have their own way to be computed.
                       org-special-properties))
               (compute-fmt (seq-filter
                             (lambda (spec)
                               (and (nth 3 spec)
                                    (member (car spec) props)))
                             file-fmt))
               (operators
                (seq-uniq
                 (append (when clocksum-props (list ":"))
                         (mapcar (lambda (spec) (nth 3 spec)) compute-fmt))))
               (summarizers
                (cl-pairlis operators (mapcar 'org-columns--summarize operators)))
               (all-fmt
                (append compute-fmt
                        (seq-filter
                         (lambda (spec) (member (car spec) clocksum-props))
                         file-fmt)))
               (values-table (make-hash-table))
               (summary-table (make-hash-table)))
          (if (seq-some 'org-columns--collect operators)
              (progn
                (push file files)
                (find-file-noselect file))
            (pcase-dolist
                (`(,node-id ,parent-id . ,array)
                 (orb-db-query
                  (orb-db-concat-query
                   (list
                    "WITH RECURSIVE
  parent AS NOT MATERIALIZED (
    SELECT value AS node_id
    FROM json_each(?)
    UNION
    SELECT node.node_id AS node_id
    FROM %s.node AS node
    JOIN parent
    ON node.parent_id = parent.node_id)"
                    (json-encode (mapcar 'orb-agenda--node-node-id nodes)))
                   "SELECT node.node_id, node.parent_id"
                   (apply
                    'orb-db-concat-query
                    (mapcar
                     (lambda (prop)
                       (list ", (
WITH property(value) AS MATERIALIZED (
  SELECT prop.value
  FROM %s.property AS prop
  WHERE prop.node_id = node.node_id
  AND prop.key = ?
  ORDER BY prop.extendp ASC, prop.begin ASC)
SELECT group_concat(value, ' ') FROM property)" prop))
                     props))
                   (when (member "CLOCKSUM" clocksum-props)
                     ", (
SELECT SUM(progressts.end_epoch - progressts.start_epoch)
FROM %s.progress AS progress
JOIN %s.timestamp AS progressts
ON progress.node_id = progressts.node_id
AND progress.ts = progressts.begin
WHERE progress.node_id = node.node_id
AND substr(progress.str, 1, 6) = 'CLOCK:')")
                   (when (member "CLOCKSUM_T" clocksum-props)
                     (cons
                      ", (
SELECT SUM(MIN(progressts.end_epoch, ?) - MAX(progressts.start_epoch, ?))
FROM %s.progress AS progress
JOIN %s.timestamp AS progressts
ON progress.node_id = progressts.node_id
AND progress.ts = progressts.begin
WHERE progress.node_id = node.node_id
AND substr(progress.str, 1, 6) = 'CLOCK:')"
                      (let ((range (org-clock-special-range 'today org-columns--time)))
                        (list
                         (time-convert (cadr range) 'integer)
                         (time-convert (car range) 'integer)))))
                   "
FROM parent
JOIN %s.node AS node
ON node.node_id = parent.node_id
ORDER BY node.point DESC")))
              (let* ((spec-values (gethash node-id values-table))
                     (properties (cl-pairlis (append props clocksum-props) array))
                     (summaries
                      (seq-map-indexed
                       (lambda (spec index)
                         (let* ((prop (car spec))
                                (operator (nth 3 spec))
                                (printf (nth 4 spec))
                                (clocksum-p (member prop '("CLOCKSUM" "CLOCKSUM_T"))))
                           (if-let ((values
                                     (seq-filter 'identity (nth index spec-values))))
                               (funcall
                                (assoc-default (if clocksum-p ":" operator) summarizers)
                                values printf)
                             (when-let ((value (assoc-default prop properties)))
                               (if clocksum-p
                                   (org-duration-from-minutes (/ value 60))
                                 (when (org-string-nw-p value)
                                   value))))))
                       all-fmt))
                     (parent-values (gethash parent-id values-table)))
                (puthash node-id (cl-pairlis all-fmt summaries) summary-table)
                (when (seq-some 'identity summaries)
                  (puthash
                   parent-id
                   (if parent-values
                       (cl-pairlis summaries parent-values)
                     (mapcar 'list summaries))
                   values-table))))
            (dolist (node nodes)
              (setf (orb-agenda--node-summaries node)
                    (gethash (orb-agenda--node-node-id node) summary-table))))))
      (let ((org-agenda-contributing-files files))
        (org-agenda-colview-compute fmt)))))

(defun orb-agenda--columns-a ()
  (org-columns-remove-overlays)
  (if (markerp org-columns-begin-marker)
      (move-marker org-columns-begin-marker (point))
    (setq org-columns-begin-marker (point-marker)))
  (let* ((org-columns--time (float-time))
         (org-done-keywords org-done-keywords-for-agenda)
         (fmt
          (cond
           ((bound-and-true-p org-overriding-columns-format))
           ((bound-and-true-p org-local-columns-format))
           ((bound-and-true-p org-columns-default-format-for-agenda))
           ((let ((m (org-get-at-bol 'org-hd-marker)))
              (and m
                   (orb-agenda--get-columns-default-format m))))
           ((and (local-variable-p 'org-columns-current-fmt)
                 org-columns-current-fmt))
           ((let ((m (next-single-property-change (point-min) 'org-hd-marker)))
              (and m
                   (let ((m (get-text-property m 'org-hd-marker)))
                     (orb-agenda--get-columns-default-format m)))))
           (t org-columns-default-format)))
         (compiled-fmt (org-columns-compile-format fmt)))
    (setq org-columns-current-fmt fmt)
    (when org-agenda-columns-compute-summary-properties
      (orb-agenda-colview-compute org-columns-current-fmt-compiled))
    (save-excursion
      ;; Collect properties for each headline in current view.
      (goto-char (point-min))
      (let (cache)
        (while (not (eobp))
          (let ((m (org-get-at-bol 'org-hd-marker)))
            (when m
              (push (cons (line-beginning-position)
                          (if (orb-agenda--marker-p m)
                              (orb-agenda--collect-values m compiled-fmt)
                            ;; `org-columns-current-fmt-compiled' is
                            ;; initialized but only set locally to the
                            ;; agenda buffer.  Since current buffer is
                            ;; changing, we need to force the original
                            ;; compiled-fmt there.
                            (org-with-point-at m
                              (org-columns--collect-values compiled-fmt))))
                    cache)))
          (forward-line))
        (when cache
          (org-columns--set-widths cache)
          (org-columns--display-here-title)
          (when (setq-local org-columns-flyspell-was-active
                            (bound-and-true-p flyspell-mode))
            (flyspell-mode 0))
          (dolist (entry cache)
            (goto-char (car entry))
            (org-columns--display-here (cdr entry)))
          (setq-local org-agenda-columns-active t)
          (when org-agenda-columns-show-summaries
            (org-agenda-colview-summarize cache)))))))

(defun orb-agenda--open-files-a ()
  (dolist (file org-agenda-contributing-files)
    (unless (find-buffer-visiting file)
      (find-file-noselect file))))


(defun orb-agenda--get-timer-title-a ()
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let* ((marker (or (get-text-property (point) 'org-marker)))
           (hdmarker (or (get-text-property (point) 'org-hd-marker)
                         marker)))
      (cond
       ((orb-agenda--marker-p hdmarker)
        (or (orb-agenda--node-heading (orb-agenda--marker-node hdmarker))
            (orb-agenda--file-path (orb-agenda--marker-node hdmarker))))
       ((and marker (marker-buffer marker))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char hdmarker)
           (or (ignore-errors (org-get-heading))
               (buffer-name (buffer-base-buffer)))))))))
   ((derived-mode-p 'org-mode)
    (ignore-errors (org-get-heading)))
   (t (buffer-name (buffer-base-buffer)))))


;;;###autoload
(define-minor-mode orb-agenda-mode
  "Global minor mode to tweak `org-agenda' to query from cache in
 Orb."
  :group 'orb
  :global t
  :init-value nil
  (cond
   (orb-agenda-mode
    (advice-add 'markerp :before-until 'orb-agenda--marker-p)
    (advice-add 'marker-buffer :filter-args 'orb-agenda--marker-a)
    (advice-add 'marker-position :filter-args 'orb-agenda--marker-a)
    (advice-add 'goto-char :filter-args 'orb-agenda--marker-a)
    (advice-add 'org-entry-get :around 'orb-agenda--entry-get-a)
    (advice-add 'org-agenda-prepare-buffers :around 'orb-agenda--prepare-buffers-a)
    (advice-add 'org-agenda-finalize :override 'orb-agenda--finalize-a)
    (advice-add 'org-agenda-mark-clocking-task :before 'org-clock-load)
    (advice-add 'org-agenda--mark-blocked-entry :override 'orb-agenda--mark-blocked-entry-a)
    (advice-add 'org-agenda-list :around 'orb-agenda--convert-skip-function-a)
    (advice-add 'org-todo-list :around 'orb-agenda--convert-skip-function-a)
    (advice-add 'org-agenda-get-day-entries :around 'orb-agenda--get-day-entries-a)
    (advice-add 'org-tags-view :override 'orb-agenda--tags-view-a)
    (advice-add 'org-tags-view :around 'orb-agenda--convert-skip-function-a)
    (advice-add 'org-agenda-show-clocking-issues :override 'orb-agenda--show-clocking-issues-a)
    (advice-add 'org-entries-lessp :override 'orb-agenda--entries-lessp-a)
    (advice-add 'org-find-top-headline :around 'orb-agenda--find-top-headline-a)
    (advice-add 'org-agenda-do-context-action :override 'orb-agenda--do-context-action-a)
    (advice-add 'org-agenda-bulk-action :before 'orb-agenda--bulk-action-a)
    (advice-add 'org-agenda-columns :override 'orb-agenda--columns-a)
    (advice-add 'org-agenda-columns :around 'orb-agenda--with-transaction-a)
    (advice-add 'org-icalendar-export-current-agenda :before 'orb-agenda--open-files-a)
    (advice-add 'org-timer--get-timer-title :override 'orb-agenda--get-timer-title-a)
    (add-hook 'find-file-hook 'orb-agenda--find-file-h))
   (t
    (remove-hook 'find-file-hook 'orb-agenda--find-file-h)
    (advice-remove 'org-timer--get-timer-title 'orb-agenda--get-timer-title-a)
    (advice-remove 'org-icalendar-export-current-agenda 'orb-agenda--open-files-a)
    (advice-remove 'org-agenda-columns 'orb-agenda--columns-a)
    (advice-remove 'org-agenda-columns 'orb-agenda--with-transaction-a)
    (advice-remove 'org-agenda-bulk-action 'orb-agenda--bulk-action-a)
    (advice-remove 'org-agenda-do-context-action 'orb-agenda--do-context-action-a)
    (advice-remove 'org-find-top-headline 'orb-agenda--find-top-headline-a)
    (advice-remove 'org-entries-lessp 'orb-agenda--entries-lessp-a)
    (advice-remove 'org-agenda-show-clocking-issues 'orb-agenda--show-clocking-issues-a)
    (advice-remove 'org-tags-view 'orb-agenda--convert-skip-function-a)
    (advice-remove 'org-tags-view 'orb-agenda--tags-view-a)
    (advice-remove 'org-agenda-get-day-entries 'orb-agenda--get-day-entries-a)
    (advice-remove 'org-todo-list 'orb-agenda--convert-skip-function-a)
    (advice-remove 'org-agenda-list 'orb-agenda--convert-skip-function-a)
    (advice-remove 'org-agenda--mark-blocked-entry 'orb-agenda--mark-blocked-entry-a)
    (advice-remove 'org-agenda-mark-clocking-task 'org-clock-load)
    (advice-remove 'org-agenda-finalize 'orb-agenda--finalize-a)
    (advice-remove 'org-agenda-prepare-buffers 'orb-agenda--prepare-buffers-a)
    (advice-remove 'org-entry-get 'orb-agenda--entry-get-a)
    (advice-remove 'goto-char 'orb-agenda--marker-a)
    (advice-remove 'marker-position 'orb-agenda--marker-a)
    (advice-remove 'marker-buffer 'orb-agenda--marker-a)
    (advice-remove 'markerp 'orb-agenda--marker-p))))

(provide 'orb-agenda)
;;; orb-agenda.el ends here
