;;; orb-extract.el --- Extract  -*- coding: utf-8; lexical-binding: t; -*-

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

(defgroup orb-extract nil
  "extract metadata"
  :group 'orb)

;;;###autoload
(defun orb-extract ()
  (let ((symbol (intern (format "orb-extract:%s" major-mode))))
    (if (fboundp symbol)
        (pcase-let ((`(,title ,props ,body) (funcall symbol)))
          (list
           title
           (mapcar
            (lambda (item)
              (cons (upcase (format "%s" (car item)))
                    (cdr item)))
            props)
           body))
      (list (file-name-base buffer-file-name) '() ""))))

(provide 'orb-extract)
;;; orb-extract.el ends here
