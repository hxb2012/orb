;;; -*- coding: utf-8; lexical-binding: t; -*-

(defmacro orb-test-with-orb-db-filewatch-mode (&rest body)
  "Execute BODY with orb filewatch mode enabled"
  (declare (indent 0) (debug (form body)))
  `(progn
     (orb-db-filewatch-mode 1)
     (unwind-protect
         (progn ,@body)
       (orb-db-filewatch-mode -1))))

(ert-deftest orb-test-db-filewatch/toggle-sync-add ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-filewatch-mode
      (should
       (orb-test--files-p
        `(("test.org" ,orb-test--empty-hash)))))))


(ert-deftest orb-test-db-filewatch/toggle-sync-delete ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-db-sync)
    (orb-test-with-buffer "test.org"
      (delete-file "test.org"))
    (should
     (orb-test--files-p
      `(("test.org" ,orb-test--empty-hash))))
    (orb-test-with-orb-db-filewatch-mode
      (should (orb-test--files-p nil)))))

(ert-deftest orb-test-db-filewatch/add ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-orb-db-filewatch-mode
      (orb-test-with-buffer "test.org"
        (should (orb-test--files-p nil))
        (save-buffer))
      (sit-for 1)
      (should
       (orb-test--files-p
        `(("test.org" ,orb-test--empty-hash)))))))

(ert-deftest orb-test-db-filewatch/update ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org"
      (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-filewatch-mode
      (should
       (orb-test--files-p
        `(("test.org" ,orb-test--empty-hash))))
      (orb-test-with-buffer "test.org"
        (insert "1")
        (save-buffer))
      (sit-for 1)
      (should
       (orb-test--files-p
        `(("test.org" "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865")))))))

(ert-deftest orb-test-db-filewatch/rename-file ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "a.org"
      (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-filewatch-mode
      (should
       (orb-test--files-p
        `(("a.org" ,orb-test--empty-hash))))
      (orb-test-with-buffer "a.org"
        (rename-file "a.org" "b.org"))
      (sit-for 1)
      (should
       (orb-test--files-p
        `(("b.org" ,orb-test--empty-hash)))))))

(ert-deftest orb-test-db-filewatch/delete ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "a.org" (save-buffer))
    (orb-test-with-buffer "b.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-test-with-orb-db-filewatch-mode
      (should
       (orb-test--files-p
        `(("a.org" ,orb-test--empty-hash)
          ("b.org" ,orb-test--empty-hash))))
      (orb-test-with-buffer "b.org"
        (delete-file "b.org"))
      (sit-for 1)
      (should
       (orb-test--files-p
        `(("a.org" ,orb-test--empty-hash)))))))

(provide 'orb-test-db-filewatch)
;;; orb-test-db-filewatch.el ends here
