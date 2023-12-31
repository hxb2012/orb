;;; orb-extract-nov.el --- Extract Nov  -*- coding: utf-8; lexical-binding: t; -*-

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
(defvar nov-metadata)
(declare-function html2org "html2org" (start end &optional replace url))

;;;###autoload
(defun orb-extract:nov-mode ()
  (list
   (assoc-default 'title nov-metadata nil "")
   (seq-filter
    (lambda (item)
      (and (cdr item)
           (not (memq (car item) '(title description)))))
    nov-metadata)
   (let ((description (assoc-default 'description nov-metadata nil "")))
     (with-temp-buffer
       (insert description)
       (html2org (point-min) (point-max))))))

(provide 'orb-extract-nov)
;;; orb-extract-nov.el ends here
