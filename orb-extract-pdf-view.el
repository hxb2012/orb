;;; orb-extract-pdf-view.el --- Extract PDFTools  -*- coding: utf-8; lexical-binding: t; -*-

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

(declare-function pdf-info-metadata "pdf-info" (&optional file-or-buffer))

;;;###autoload
(defun orb-extract:pdf-view-mode ()
  (let ((metadata (pdf-info-metadata)))
    (list
     (assoc-default 'title metadata nil "")
     (seq-filter
      (lambda (item)
        (and (cdr item)
             (not (memq (car item) '(title)))))
      metadata)
     "")))

(provide 'orb-extract-pdf-view)
;;; orb-extract-pdf-view.el ends here
