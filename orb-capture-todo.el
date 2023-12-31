;;; orb-capture-todo.el --- Org Capture模板  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Free Software Foundation, Inc.

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

;; 最初是从org-capture.el里抄来的

;;; Commentary:

;;; Code:
(require 'orb-capture)

(defvar org-capture-link-is-already-stored)

(defcustom orb-capture-todo-target-alist nil
  "orb capture todo"
  :type '(alist :key-type string :value-type sexp)
  :group 'orb-capture)

(add-to-list 'orb-capture-targets '("todo" . orb-capture-todo-target-alist))

;;;###autoload
(defun orb-capture-todo ()
  (interactive)
  (let ((org-capture-link-is-already-stored t))
    (orb-capture
     "todo"
     "* %?
:PROPERTIES:
:CAPTURED: %u
:ID: %(org-id-new)
:END:

"
     :jump-to-captured nil
     :empty-lines 1)))

;;;###autoload
(defun orb-capture-todo-with-context ()
  (interactive)
  (let ((org-capture-link-is-already-stored nil))
    (orb-capture
     "todo"
     "* TODO %?%:description
:PROPERTIES:
:CAPTURED: %u
:ID: %(org-id-new)
:FIX: %L
:CATEGORY: %:category
:END:

"
     :jump-to-captured nil
     :empty-lines 1)))

(provide 'orb-capture-todo)
;;; orb-capture-todo.el ends here
