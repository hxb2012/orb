;;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'orb)
(require 'orb-db)

(defconst orb-test--empty-hash
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (secure-hash 'sha256 (current-buffer))))

(defmacro orb-test-with-temp-orb-directory (&rest body)
  "Execute BODY with a temporary orb directory"
  (declare (indent 0) (debug (form body)))
  `(let* ((orb-directory (make-temp-file "orb" t)))
     (unwind-protect
         (progn ,@body)
       (orb-db-clear-all)
       (delete-directory orb-directory t))))

(defmacro orb-test-with-new-temporary-file-directory (&rest body)
  "Execute BODY with a new temporary file directory"
  (declare (indent 0) (debug (form body)))
  `(let* ((temporary-file-directory (make-temp-file "orb" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temporary-file-directory t))))

(defmacro orb-test-with-buffer (file-name &rest body)
  "Execute BODY in a orb buffer"
  (declare (indent 1) (debug (form body)))
  `(with-current-buffer (find-file-noselect (file-name-concat orb-directory ,file-name))
     (unwind-protect
         (progn ,@body)
       (kill-buffer))))

(defun orb-test--files-p (files)
  (and
   (equal
    (orb-db-with-transaction
      (orb-db-select "SELECT path, hash FROM main.file ORDER BY path ASC"))
    files)
   (seq-every-p
    (lambda (x)
      (equal
       (orb-db--file-name-levels (car x))
       (when (cadr x)
         (json-parse-string (cadr x) :null-object nil :array-type 'list))))
    (orb-db-with-transaction
      (orb-db-select "SELECT path, levels FROM main.file ORDER BY path ASC")))))

(defconst orb-test--base-directory (file-name-directory (or load-file-name buffer-file-name)))

(defun orb-test-load ()
  (mapc
   (lambda (path)
     (let ((base-path (file-name-sans-extension path)))
       (require
        (intern
         (file-name-nondirectory base-path))
        base-path)))
   (directory-files
    orb-test--base-directory
    t
    "^orb-test-.*\\.el$")))

(defun orb-test-run-all-tests ()
  (interactive)
  (orb-test-load)
  (ert t))

(provide 'orb-test)
;;; orb-test.el ends here
