;;; -*- coding: utf-8; lexical-binding: t; -*-

(defmacro orb-test-with-orb-db-autosync-mode (&rest body)
  "Execute BODY with orb autosync mode enabled"
  (declare (indent 0) (debug (form body)))
  `(progn
     (orb-db-autosync-mode 1)
     (unwind-protect
         (progn ,@body)
       (orb-db-autosync-mode -1))))

(ert-deftest orb-test-db-autosync/setup-sync ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-autosync-mode
      (should
       (orb-test--files-p
        `(("test.org" ,orb-test--empty-hash)))))))

(ert-deftest orb-test-db-autosync/setup ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-autosync-mode
      (orb-test-with-buffer "test.org"
        (should (orb-test--files-p nil))
        (save-buffer))
      (should
       (orb-test--files-p
        `(("test.org" ,orb-test--empty-hash)))))))

(ert-deftest orb-test-db-autosync/setup-existing-buffer ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org"
      (orb-test-with-orb-db-autosync-mode
        (should (orb-test--files-p nil))
        (save-buffer)
        (should
         (orb-test--files-p
          `(("test.org" ,orb-test--empty-hash))))))))

(ert-deftest orb-test-db-autosync/rename-file ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-autosync-mode
      (orb-test-with-buffer "a.org"
        (should (orb-test--files-p nil))
        (save-buffer)
        (should
         (orb-test--files-p
          `(("a.org" ,orb-test--empty-hash))))
        (rename-file "a.org" "b.org")
        (should
         (orb-test--files-p
          `(("b.org" ,orb-test--empty-hash))))))))

(ert-deftest orb-test-db-autosync/rename-file-down ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-autosync-mode
      (orb-test-with-buffer "a.org"
        (should (orb-test--files-p nil))
        (save-buffer)
        (should
         (orb-test--files-p
          `(("a.org" ,orb-test--empty-hash))))
        (make-directory "b")
        (rename-file "a.org" "b/")
        (should
         (orb-test--files-p
          `(("b/a.org" ,orb-test--empty-hash))))))))

(ert-deftest orb-test-db-autosync/rename-file-up ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-autosync-mode
      (orb-test-with-buffer "a.org"
        (make-directory "a")
        (orb-test-with-buffer "a/test.org"
          (should (orb-test--files-p nil))
          (save-buffer))
        (should
         (orb-test--files-p
          `(("a/test.org" ,orb-test--empty-hash))))
        (rename-file "a/test.org" "b.org")
        (should
         (orb-test--files-p
          `(("b.org" ,orb-test--empty-hash))))))))

(ert-deftest orb-test-db-autosync/rename-directory ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-autosync-mode
      (orb-test-with-buffer "a.org"
        (make-directory "a/bb/ccc" t)
        (orb-test-with-buffer "a/bb/ccc/test.org"
          (should (orb-test--files-p nil))
          (save-buffer))
        (should
         (orb-test--files-p
          `(("a/bb/ccc/test.org" ,orb-test--empty-hash))))
        (rename-file "a/bb" "ddddd")
        (should
         (orb-test--files-p
          `(("ddddd/ccc/test.org" ,orb-test--empty-hash))))))))

(ert-deftest orb-test-db-autosync/rename-file-in ()
  (orb-test-with-new-temporary-file-directory
    (orb-test-with-temp-orb-directory
      (orb-test-with-orb-db-autosync-mode
        (let ((file-name (make-temp-file "" nil ".org" "")))
          (should (orb-test--files-p nil))
          (orb-test-with-buffer "a.org"
            (rename-file file-name "test.org"))
          (should
           (orb-test--files-p
            `(("test.org" ,orb-test--empty-hash)))))))))

(ert-deftest orb-test-db-autosync/rename-file-out ()
  (orb-test-with-new-temporary-file-directory
    (orb-test-with-temp-orb-directory
      (orb-test-with-orb-db-autosync-mode
        (orb-test-with-buffer "test.org"
          (should (orb-test--files-p nil))
          (save-buffer))
        (should
         (orb-test--files-p
          `(("test.org" ,orb-test--empty-hash))))
        (orb-test-with-buffer "test.org"
          (rename-file "test.org" "../test.org"))
        (should (orb-test--files-p nil))))))

(ert-deftest orb-test-db-autosync/rename-directory-in ()
  (orb-test-with-new-temporary-file-directory
    (orb-test-with-temp-orb-directory
      (orb-test-with-orb-db-autosync-mode
        (let ((dir-file-name (make-temp-file "" t)))
          (with-temp-file (file-name-concat dir-file-name "test.org"))
          (should (orb-test--files-p nil))
          (orb-test-with-buffer "a.org"
            (rename-file dir-file-name "a"))
          (should
           (orb-test--files-p
            `(("a/test.org" ,orb-test--empty-hash)))))))))

(ert-deftest orb-test-db-autosync/rename-directory-out ()
  (orb-test-with-new-temporary-file-directory
    (orb-test-with-temp-orb-directory
      (orb-test-with-orb-db-autosync-mode
        (orb-test-with-buffer "a.org"
          (make-directory "a")
          (orb-test-with-buffer "a/test.org"
            (should (orb-test--files-p nil))
            (save-buffer))
          (should
           (orb-test--files-p
            `(("a/test.org" ,orb-test--empty-hash))))
          (rename-file "a" "../")
          (should (orb-test--files-p nil)))))))

(ert-deftest orb-test-db-autosync/delete ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "a.org" (save-buffer))
    (orb-test-with-buffer "b.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-autosync-mode
      (should
       (orb-test--files-p
        `(("a.org" ,orb-test--empty-hash)
          ("b.org" ,orb-test--empty-hash))))
      (orb-test-with-buffer "b.org"
        (delete-file "b.org"))
      (should
       (orb-test--files-p
        `(("a.org" ,orb-test--empty-hash)))))))

(provide 'orb-test-db-autosync)
;;; orb-test-db-autosync.el ends here
