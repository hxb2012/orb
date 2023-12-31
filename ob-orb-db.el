;;; ob-orb-db.el --- orb-db代码块 -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Maintainer: Nick Savage <nick@nicksavage.ca>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; 最初是从ob-sqlite.el抄来的

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'org-src (add-to-list 'org-src-lang-modes '("orb-db" . sql)))
;;;###autoload(autoload 'sql-set-product "sql")
(declare-function sql-set-product "sql" (product))

;;;###autoload
(defun org-babel-execute:orb-db (body _params)
  "Execute a block of Sqlite code in orb-db"
  (orb-db-with-transaction
    (orb-db-select body nil 'full)))

;;;###autoload
(defun org-babel-prep-session:orb-db (_session _params)
  "Raise an error because support for orb-db sessions isn't implemented."
  (error "SQLite sessions not yet implemented"))

;;;###autoload
(defun org-babel-edit-prep:orb-db (_info)
  (sql-set-product 'sqlite))

(provide 'ob-orb-db)
;;; ob-orb-db.el ends here
