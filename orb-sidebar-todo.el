;;; orb-sidebar-todo.el --- TODO  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

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
(require 'orb-sidebar-link)

;;;###autoload(autoload 'org-compile-prefix-format "org-agenda")
(declare-function org-compile-prefix-format "org-agenda" (key))
(declare-function org-agenda-format-item
                  "org-agenda"
                  (extra txt &optional level category tags dotime
                         remove-re habitp))
(declare-function org-agenda-deadline-face "org-agenda" (fraction))
(declare-function org-agenda--timestamp-to-absolute "org-agenda" (&rest args))

(defvar org-agenda-deadline-leaders)
(defvar org-agenda-scheduled-leaders)
(defvar org-agenda-prefer-last-repeat)
(defvar org-agenda-skip-deadline-prewarning-if-scheduled)
(defvar org-agenda-show-inherited-tags)
(defvar org-agenda-use-tag-inheritance)

(defun orb-sidebar-todo--collect ()
  (let ((table (make-hash-table :test 'equal))
        (table1 (make-hash-table :test 'equal))
        (regexp (concat "^" org-outline-regexp " *" org-todo-regexp))
        (all-matches nil)
        (all-regexps nil))
    (goto-char (point-min))
    (while (let (case-fold-search)
             (re-search-forward regexp nil t))
      (pcase-let
          ((`(,matches . ,regexps)
            (save-excursion
              (goto-char (match-beginning 0))
              (with-restriction
                  (point)
                  (save-excursion
                    (org-next-visible-heading 1)
                    (point))
                (orb-sidebar-link--collect-links table t (list (point-min)))))))
        (push matches all-matches)
        (push regexps all-regexps)))
    (maphash
     (lambda (key value)
       (puthash key (seq-uniq value) table1))
     table)
    (list (apply 'append all-matches) (apply 'append all-regexps) table1)))

(defun orb-sidebar-todo--find-timestamp (bol key re)
  (save-excursion
    (when (search-backward re bol t)
      (goto-char (match-end 0))
      (skip-chars-forward " \t")
      (when (looking-at org-ts-regexp-both)
        key))))

(defun orb-sidebar-todo--find-item-time ()
  (forward-line)
  (when (looking-at-p org-planning-line-re)
    (end-of-line)
    (let ((bol (line-beginning-position)))
      (or (orb-sidebar-todo--find-timestamp bol 'closed org-closed-string)
          (orb-sidebar-todo--find-timestamp bol 'deadline org-deadline-string)
          (orb-sidebar-todo--find-timestamp bol 'scheduled org-scheduled-string)))))

(defun orb-sidebar-todo--get-item-extra ()
  (pcase (orb-sidebar-todo--find-item-time)
    ('closed
     (cons nil "Closed:    "))
    ('deadline
     (let* ((today (org-today))
            (today? t)
            (current today)
            (s (match-string 0))
            (pos (point))
            (todo-state (save-match-data (org-get-todo-state)))
            (sexp? (string-prefix-p "%%" s))
            ;; DEADLINE is the deadline date for the entry.  It is
            ;; either the base date or the last repeat, according
            ;; to `org-agenda-prefer-last-repeat'.
            (deadline
             (cond
              (sexp? (org-agenda--timestamp-to-absolute s current))
              ((or (eq org-agenda-prefer-last-repeat t)
                   (member todo-state org-agenda-prefer-last-repeat))
               (org-agenda--timestamp-to-absolute
                s today 'past (current-buffer) pos))
              (t (org-agenda--timestamp-to-absolute s))))
            (diff (- deadline current))
            (suppress-prewarning
             (let ((scheduled
                    (and org-agenda-skip-deadline-prewarning-if-scheduled
                         (org-entry-get nil "SCHEDULED"))))
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
            (wdays (or suppress-prewarning (org-get-wdays s)))
            (extra
             ;; Insert appropriate suffixes before deadlines.
             ;; Those only apply to today agenda.
             (pcase-let ((`(,now ,future ,past)
                          org-agenda-deadline-leaders))
               (cond
                ((and today? (< deadline today)) (format past (- diff)))
                ((and today? (> deadline today)) (format future diff))
                (t now))))
            (face (org-agenda-deadline-face
                   (- 1 (/ (float diff) (max wdays 1))))))
       (cons face extra)))
    ('scheduled
     (let* ((today (org-today))
            (todayp t)
            (current today)
            (s (match-string 0))
            (pos (point))
            (todo-state (save-match-data (org-get-todo-state)))
            (sexp? (string-prefix-p "%%" s))
            ;; SCHEDULE is the scheduled date for the entry.  It is
            ;; either the bare date or the last repeat, according
            ;; to `org-agenda-prefer-last-repeat'.
            (schedule
             (cond
              (sexp? (org-agenda--timestamp-to-absolute s current))
              ((or (eq org-agenda-prefer-last-repeat t)
                   (member todo-state org-agenda-prefer-last-repeat))
               (org-agenda--timestamp-to-absolute
                s today 'past (current-buffer) pos))
              (t (org-agenda--timestamp-to-absolute s))))
            (diff (- current schedule))
            (pastschedp (< schedule today))
            (futureschedp (> schedule today))
            (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
            (extra
             (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
               ;; Show a reminder of a past scheduled today.
               (if (and todayp pastschedp)
                   (format past diff)
                 first)))
            (face (cond ((and (not habitp) pastschedp)
                         'org-scheduled-previously)
                        ((and habitp futureschedp)
                         'org-agenda-done)
                        (todayp 'org-scheduled-today)
                        (t 'org-scheduled))))
     (cons face extra)))
    (_
     (cons nil "           "))))

(defun orb-sidebar-todo--insert-item (buffer last-pos start)
  (let* ((last-file-overlay (when last-pos (car last-pos)))
         (last-file-name
          (when last-file-overlay
            (overlay-get last-file-overlay 'orb-file)))
         (file-changed (not (equal last-file-name buffer-file-name)))
         (file-overlay
          (if file-changed
              (orb-sidebar-link--insert-file buffer)
            last-file-overlay))
         (last-org-pos (unless file-changed (cadr last-pos))))
    (unless (eq last-org-pos start)
      (let* ((todo-state (save-match-data (org-get-todo-state)))
             (donep (member todo-state org-done-keywords))
             (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
             (category (org-get-category))
             (inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'agenda org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'agenda
                                 org-agenda-use-tag-inheritance)))))
             (tags (org-get-tags nil (not inherited-tags)))
             (level (make-string (org-reduced-level (org-outline-level)) ?\s))
             (head
              (progn
                (re-search-forward org-outline-regexp-bol)
                (buffer-substring (point) (line-end-position))))
             (face-extra (orb-sidebar-todo--get-item-extra))
             (extra (cdr face-extra))
             (face (if donep 'org-agenda-done (car face-extra)))
             (item (org-agenda-format-item extra head level category tags nil nil habitp))
             (txt (if face (propertize item 'face face) item)))
        (with-current-buffer buffer
          (orb-sidebar-section--with-restriction file-overlay
            (orb-sidebar-link--insert-section start txt)))))
    (list file-overlay start)))

(defun orb-sidebar-todo--display-links (buffer links)
  (org-compile-prefix-format 'agenda)
  (orb-sidebar-link--display-links buffer links 'orb-sidebar-todo--insert-item))

;;;###autoload
(defun orb-sidebar-todo-section (overlay)
  (orb-sidebar-link-back-section overlay t 'orb-sidebar-todo--collect 'orb-sidebar-todo--display-links))

(provide 'orb-sidebar-todo)
;;; orb-sidebar-todo.el ends here
