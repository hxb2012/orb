;;; -*- coding: utf-8; lexical-binding: t; -*-

(ert-deftest orb-test-db/sync-file-new ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-db-sync)
    (should
     (orb-test--files-p
      `(("test.org" ,orb-test--empty-hash))))))

(ert-deftest orb-test-db/sync-file-update ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "test.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-db-sync)
    (should
     (orb-test--files-p
      `(("test.org" ,orb-test--empty-hash))))
    (orb-test-with-buffer "test.org"
      (insert "1")
      (save-buffer))
    (orb-db-sync)
    (should
     (orb-test--files-p
      '(("test.org" "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865"))))))

(ert-deftest orb-test-db/sync-file-delete ()
  (orb-test-with-temp-orb-directory
    (orb-test-with-buffer "a.org" (save-buffer))
    (orb-test-with-buffer "b.org" (save-buffer))
    (should (orb-test--files-p nil))
    (orb-db-sync)
    (should
     (orb-test--files-p
      `(("a.org" ,orb-test--empty-hash)
        ("b.org" ,orb-test--empty-hash))))
    (orb-test-with-buffer "b.org"
      (delete-file "b.org"))
    (orb-db-sync)
    (should
     (orb-test--files-p
      `(("a.org" ,orb-test--empty-hash))))))

(provide 'orb-test-db)
;;; orb-test-db.el ends here
