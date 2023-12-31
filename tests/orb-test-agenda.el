;;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'ert-x)
(require 'orb-agenda)

(defmacro orb-test-with-temp-org-directory (&rest body)
  "Execute BODY with a temporary org directory"
  (declare (indent 0) (debug (form body)))
  `(orb-test-with-temp-orb-directory
     (let ((org-directory orb-directory)
           (org-agenda-files
            (let ((temporary-file-directory orb-directory))
              (list (make-temp-file "agenda" nil ".org")))))
       (unwind-protect
           (progn ,@body)
         (when-let ((buffer (org-find-base-buffer-visiting (car org-agenda-files))))
           (with-current-buffer buffer
             (save-buffer))
           (kill-buffer buffer))))))

(defmacro orb-test-with-agenda-file-buffer (&rest body)
  "Execute BODY in agenda file buffer"
  (declare (indent 0) (debug (form body)))
  `(with-current-buffer (find-file-noselect (car org-agenda-files)) nil nil
                        (unwind-protect
                            (progn ,@body)
                          (kill-buffer))))

(defmacro orb-test-with-orb-agenda-mode (&rest body)
  "Execute BODY with orb agenda mode enabled"
  (declare (indent 0) (debug (form body)))
  `(progn
     (orb-agenda-mode 1)
     (unwind-protect
         (progn ,@body)
       (orb-agenda-mode -1))))

(defun orb-test-agenda--list-items-text-properties ()
  (let ((pom 0)
        (items nil))
    (while (/= pom (progn (org-agenda-next-item 1) (point)))
      (setq pom (point))
      (push (cl--plist-to-alist (text-properties-at pom)) items))
    (nreverse items)))

(defun orb-test-agenda--list-items-overlays ()
  (let ((pom 0)
        (items nil))
    (while (/= pom (progn (org-agenda-next-item 1) (point)))
      (setq pom (point))
      (push
       (mapcar 'cl--plist-to-alist
               (mapcar 'overlay-properties
                       (overlays-in pom (line-end-position))))
       items))
    (nreverse items)))

(defun orb-test-agenda--compare-text-properties (got expected)
  ;; (message "EXPECTED:\n%s" expected)
  ;; (message "GOT:\n%s" got)
  (let ((mismatch-exp nil)
        (mismatch-got nil))
    (dolist (pair-exp expected)
      (let* ((key (car pair-exp))
             (pair-got (assoc key got)))
        (cond
         ((and (member key '(org-marker org-hd-marker))
               (not (null pair-got)))
          (let* ((mrk-exp (cdr pair-exp))
                 (mrk-got (cdr pair-got))
                 (value-exp
                  (cons (buffer-file-name (marker-buffer mrk-exp))
                        (marker-position mrk-exp)))
                 (value-got
                  (if (markerp mrk-got)
                      (cons (buffer-file-name (marker-buffer mrk-got))
                            (marker-position mrk-got))
                    (cons (orb-agenda--file-path (orb-agenda--marker-file mrk-got))
                          (orb-agenda--marker-position mrk-got)))))
            (unless (equal value-exp value-got)
              (push (cons key value-exp) mismatch-exp)
              (push (cons key value-got) mismatch-got))))
         ((not (equal pair-exp pair-got))
          (push pair-exp mismatch-exp)
          (push pair-got mismatch-got)))))
    (dolist (pair-got got)
      (let ((key (car pair-got)))
        (unless (assoc key expected)
          (push (cons key nil) mismatch-exp)
          (push pair-got mismatch-got))))
    (should (equal mismatch-exp mismatch-got))))


(defun orb-test-agenda--compare-items (got expected)
  (cl-mapc 'orb-test-agenda--compare-text-properties got expected)
  (let ((lg (length got))
        (le (length expected)))
    (cond
     ((< lg le)
      (should (equal (seq-drop expected lg) nil)))
     ((> lg le)
      (should (equal nil (seq-drop got le)))))))

(defun orb-test-agenda--compare-overlays-items (got expected)
  (cl-mapc 'orb-test-agenda--compare-items got expected)
  (let ((lg (length got))
        (le (length expected)))
    (cond
     ((< lg le)
      (should (equal (seq-drop expected lg) nil)))
     ((> lg le)
      (should (equal nil (seq-drop got le)))))))

(defmacro orb-test-agenda--compare (&rest body)
  "Execute BODY with orb agenda mode enabled and disabled, and
 compare the result"
  (declare (indent 0) (debug (form body)))
  `(orb-test-agenda--compare-items
    (orb-test-with-orb-agenda-mode
      (save-excursion
        ,@body
        (orb-test-agenda--list-items-text-properties)))
    (save-excursion
      ,@body
      (orb-test-agenda--list-items-text-properties))))

(defmacro orb-test-agenda--compare-overlays (&rest body)
  "Execute BODY with orb agenda mode enabled and disabled, and
 compare the result"
  (declare (indent 0) (debug (form body)))
  `(orb-test-agenda--compare-overlays-items
    (orb-test-with-orb-agenda-mode
      (save-excursion
        ,@body
        (orb-test-agenda--list-items-overlays)))
    (save-excursion
      ,@body
      (orb-test-agenda--list-items-overlays))))

(ert-deftest orb-test-agenda/get-blocks ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-blocks-src-block ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-insert-heading)
      (insert "task 2\n")
      (insert "#+BEGIN_SRC org\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (insert "\n#+END_SRC")
      (org-insert-heading)
      (insert "task 3\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-blocks-before-range ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-time-stamp (- (float-time nil) (* 5 86400)))
      (insert "--")
      (org-insert-time-stamp (- (float-time nil) (* 3 86400)))
      (insert "\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))


(ert-deftest orb-test-agenda/get-blocks-after-range ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-time-stamp (+ (float-time nil) (* 3 86400)))
      (insert "--")
      (org-insert-time-stamp (+ (float-time nil) (* 5 86400)))
      (insert "\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-blocks-skip-if-done ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-todo)
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-timestamps ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16) nil)
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16) nil)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-timestamps-sexp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (insert
       (let ((now (decode-time)))
         (format
          "<%%%%(org-date %s %s %s)>\n"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-timestamps-repeat ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (backward-char)
      (insert " +1d")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-timestamps-skip-if-done ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16) nil)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-todo)
      (org-time-stamp '(16) nil)
      (org-insert-heading)
      (insert "task 3\n")
      (org-time-stamp '(16) nil)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-timestamps-skip-same-entry ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16) nil)
      (insert "\n")
      (org-time-stamp '(16) nil)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-additional-timestamps-same-entry t))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/get-sexps ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (let ((now (decode-time)))
        (insert
         (format
          "%%%%(org-date %s %s %s) test sexp\n"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (org-insert-heading)
      (insert "task 2\n")
      (let ((now (decode-time)))
        (insert
         (format
          "%%%%(org-date %s %s %s) test sexp\n"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-sexps-src-block ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (let ((now (decode-time)))
        (insert
         (format
          "%%%%(org-date %s %s %s) test sexp"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (org-insert-heading)
      (insert "task 2\n")
      (insert "#+BEGIN_SRC org\n")
      (let ((now (decode-time)))
        (insert
         (format
          "%%%%(org-date %s %s %s) test sexp 1"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (insert "\n#+END_SRC\n")
      (org-insert-heading)
      (insert "task 3\n")
      (let ((now (decode-time)))
        (insert
         (format
          "%%%%(org-date %s %s %s) test sexp"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-progress-closed ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (let ((org-log-done 'time))
        (org-todo))
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (let ((org-log-done 'time))
        (org-todo))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode))))

(ert-deftest orb-test-agenda/get-progress-state ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+TODO: TODO(!) | DONE(!) CANCELLED(!)\n\n")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (forward-line 2)
      (org-insert-heading)
      (insert "task 1\n")
      (ert-simulate-command '(org-todo))
      (forward-line 2)
      (org-insert-heading)
      (insert "task 2\n")
      (ert-simulate-command '(org-todo))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode 'special))))

(ert-deftest orb-test-agenda/get-progress-state-src-block ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+TODO: TODO(!) | DONE(!) CANCELLED(!)\n\n")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (forward-line 2)
      (org-insert-heading)
      (insert "task 1\n")
      (ert-simulate-command '(org-todo))
      (forward-line 2)
      (org-insert-heading)
      (insert "task 2\n")
      (save-excursion
        (ert-simulate-command '(org-todo)))
      (insert "#+BEGIN_SRC org\n")
      (org-end-of-subtree)
      (insert "\n#+END_SRC\n")
      (org-insert-heading)
      (insert "task 3\n")
      (ert-simulate-command '(org-todo))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode 'special))))

(ert-deftest orb-test-agenda/get-progress-state-note ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+TODO: TODO(@) | DONE(@) CANCELLED(@)\n\n")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (forward-line 2)
      (org-insert-heading)
      (insert "task 1\n")
      (ert-simulate-command '(org-todo))
      (with-current-buffer "*Org Note*"
        (insert "my note")
        (org-ctrl-c-ctrl-c))
      (forward-line 2)
      (org-insert-heading)
      (insert "task 2\n")
      (ert-simulate-command '(org-todo))
      (with-current-buffer "*Org Note*"
        (insert "my note")
        (org-ctrl-c-ctrl-c))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode 'special))))

(ert-deftest orb-test-agenda/get-progress-not-add-notes ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+TODO: TODO(@) | DONE(@) CANCELLED(@)\n\n")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (forward-line 4)
      (org-insert-heading)
      (insert "task 1\n")
      (ert-simulate-command '(org-todo))
      (with-current-buffer "*Org Note*"
        (insert "my note")
        (org-ctrl-c-ctrl-c))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-log-mode-add-notes nil))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)
        (org-agenda-log-mode 'special)))))

(ert-deftest orb-test-agenda/get-progress-clocked ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-clock-in)
      (org-clock-out)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode))))

(ert-deftest orb-test-agenda/get-progress-clocked-note ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+STARTUP: lognoteclock-out\n\n")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (forward-line 4)
      (org-insert-heading)
      (insert "task 1")
      (org-todo)
      (org-clock-in)
      (ert-simulate-command '(org-clock-out))
      (with-current-buffer "*Org Note*"
        (insert "my note")
        (org-ctrl-c-ctrl-c))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil)
      (org-agenda-log-mode))))

(ert-deftest orb-test-agenda/get-deadlines ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-deadline nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-deadlines-sexp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (insert
       (let ((now (decode-time)))
         (format
          "DEADLINE: <%%%%(org-date %s %s %s)>\n"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-deadlines-repeat ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (insert "DEADLINE: ")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (backward-char)
      (insert " +1d")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-deadlines-skip-if-done ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-todo)
      (org-deadline nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-comment ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1")
      (org-toggle-comment)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-comment-trees nil))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/get-scheduled-comment-no-text ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (org-toggle-comment)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-file-archive ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (insert "#+FILETAGS: :ARCHIVE:\n\n")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-archive-tree ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "archive tree\n")
      (org-toggle-archive-tag)
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-demote-subtree)
      (org-insert-heading)
      (org-promote-subtree)
      (insert "task 3\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))


(ert-deftest orb-test-agenda/get-scheduled-archive-tree-no-skip ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "archive tree\n")
      (org-toggle-archive-tag)
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-demote-subtree)
      (org-insert-heading)
      (org-promote-subtree)
      (insert "task 3\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-archived-trees nil))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/get-scheduled-comment-tree ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "comment tree\n")
      (org-toggle-comment)
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-demote-subtree)
      (org-insert-heading)
      (org-promote-subtree)
      (insert "task 3\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-sexp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (insert
       (let ((now (decode-time)))
         (format
          "SCHEDULED: <%%%%(org-date %s %s %s)>\n"
          (decoded-time-year now)
          (decoded-time-month now)
          (decoded-time-day now))))
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-repeat ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (insert "SCHEDULED: ")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (backward-char)
      (insert " +1d")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/get-scheduled-skip-if-done ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))


(ert-deftest orb-test-agenda/property-effort ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1")
      (org-todo)
      (org-set-effort nil "1d")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/property-category-lower-case ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-set-property "category" "cat1")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/property-category-at-keyword ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (insert "#+CATEGORY: cat\n")
      (org-insert-heading)
      (insert "task 1")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/property-category-at-parent ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (insert ":PROPERTIES:\n:CATEGORY: cat1\n:END:\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (org-set-property "CATEGORY" "cat2")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/property-category-literal-nil ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (insert ":PROPERTIES:\n:CATEGORY: cat1\n:END:\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-set-property "CATEGORY" "nil")
      (org-todo)
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (org-set-property "CATEGORY" "cat2")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/property-warntime ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-set-property "APPT_WARNTIME" "60")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))

(ert-deftest orb-test-agenda/property-warntime-inherit ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-set-property "APPT_WARNTIME" "60")
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-use-property-inheritance '("APPT_WARNTIME")))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/property-warntime-no-inherit ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-set-property "APPT_WARNTIME" "60")
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-demote-subtree)
      (save-buffer))
    (let ((org-use-property-inheritance '("APPT_WARNTIME")))
      (orb-db-sync))
    (orb-test-agenda--compare
      (org-agenda-list nil "-1d" 3 nil))))


(ert-deftest orb-test-agenda/get-todos ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/get-todos-with-sublevels ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/get-todos-without-sublevels ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-list-sublevels nil))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-with-date ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-with-date t))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-with-date-block ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-with-date t))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-future ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-scheduled 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-future-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-scheduled 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-positive ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-scheduled 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-positive-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-scheduled 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-past ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-scheduled 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-past-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-scheduled 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))


(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-negative ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-scheduled -1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-scheduled-negative-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-scheduled -1))
      (orb-test-agenda--compare
        (org-todo-list)))))



(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-future ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-future-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-deadlines 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-positive ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-positive-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "-1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "-1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-deadlines 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-past ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-past-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-deadlines 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))


(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-negative ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines -1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-negative-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-deadline nil "+1d")
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "+1d")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-deadlines -1))
      (orb-test-agenda--compare
        (org-todo-list)))))


(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-all ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "-1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 'all))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-far ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+15d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-deadline nil "+1d")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 'far))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-deadlines-near ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-deadline nil "+15d")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-deadline nil "+1d")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-deadline nil "+15d")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-deadlines 'near))
      (orb-test-agenda--compare
        (org-todo-list)))))



(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-future ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-timestamp 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-future-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-timestamp 'future))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-positive ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-timestamp 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-positive-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (- (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-timestamp 1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-past ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-timestamp 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-past-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-timestamp 'past))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-negative ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-timestamp -1))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/get-todos-ignore-timestamp-negative-seconds ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (org-insert-heading)
      (insert "task 2\n")
      (org-insert-time-stamp (- (float-time nil) 86400))
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-insert-time-stamp (+ (float-time nil) 86400))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-todo-ignore-time-comparison-use-seconds t)
          (org-agenda-todo-ignore-timestamp -1))
      (orb-test-agenda--compare
        (org-todo-list)))))



(ert-deftest orb-test-agenda/breadcrumbs ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-prefix-format '((todo . " %i %-12:c %b"))))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/breadcrumbs-priority ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-priority-up)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-demote-subtree)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-prefix-format '((todo . " %i %-12:c %b"))))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/sorting-strategy ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-sorting-strategy '((agenda todo-state-up))))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/tags-order ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-toggle-tag "e")
      (org-toggle-tag "a")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-toggle-tag "b")
      (org-toggle-tag "d")
      (org-todo)
      (org-demote-subtree)
      (org-insert-heading)
      (insert "task 3\n")
      (org-toggle-tag "c")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/tags-duplicated ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-toggle-tag "a")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-toggle-tag "a")
      (org-todo)
      (org-demote-subtree)
      (org-insert-heading)
      (insert "task 3\n")
      (org-toggle-tag "a")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/tags-exclude-from-inheritance ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-toggle-tag "a")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-toggle-tag "b")
      (org-todo)
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-tags-exclude-from-inheritance '("a")))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/file-tag-inherit ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (save-buffer))
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (insert "#+FILETAGS: :a:\n\n")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/priority ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-priority-up)
      (org-priority-up)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-todo-list))))

(ert-deftest orb-test-agenda/skip-function-if-scheduled ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'scheduled)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-not-scheduled ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'notscheduled)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-deadline ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-deadline nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'deadline)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-scheduled-deadline ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "now")
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'scheduled 'deadline)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-timestamp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "now")
      (insert "task 4\n")
      (org-todo)
      (org-time-stamp '(16))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'timestamp)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-not-timestamp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-deadline nil "now")
      (insert "task 4\n")
      (org-todo)
      (org-time-stamp '(16))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottimestamp)))
      (orb-test-agenda--compare
        (org-todo-list)))))

(ert-deftest orb-test-agenda/skip-function-if-todo ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'todo 'todo)))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/skip-function-if-done ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "now")
      (org-todo)
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'todo 'done)))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(ert-deftest orb-test-agenda/tags-view-tag ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-toggle-tag "tag")
      (org-insert-heading)
      (insert "task 3\n")
      (org-toggle-tag "notag")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "tag"))))

(ert-deftest orb-test-agenda/tags-view-todo ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-todo)
      (org-insert-heading)
      (insert "task 3\n")
      (org-todo)
      (org-todo)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "TODO=\"TODO\""))))

(ert-deftest orb-test-agenda/tags-view-level ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-demote-subtree)
      (org-insert-heading)
      (insert "task 3\n")
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "LEVEL=2"))))

(ert-deftest orb-test-agenda/tags-view-planning ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-insert-heading)
      (insert "task 2\n")
      (org-schedule nil "+10d")
      (org-insert-heading)
      (insert "task 3\n")
      (org-schedule nil "now")
      (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "SCHEDULED>=\"<+7d>\""))))

(ert-deftest orb-test-agenda/tags-view-properties-timestamp ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (let ((now (float-time nil)))
        (insert "\n\n")
        (org-insert-heading)
        (insert "task 1\n")
        (org-insert-heading)
        (insert "task 2\n")
        (insert ":PROPERTIES:\n")
        (insert ":CUSTOM_PROP: ")
        (org-insert-time-stamp (+ now 864000))
        (insert "\n:END:\n\n")
        (org-insert-heading)
        (insert "task 3\n")
        (insert ":PROPERTIES:\n")
        (insert ":CUSTOM_PROP: ")
        (org-insert-time-stamp now)
        (insert "\n:END:\n\n")
        (save-buffer)))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "CUSTOM_PROP>=\"<+7d>\""))))

(ert-deftest orb-test-agenda/tags-view-category ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
     (insert ":PROPERTIES:\n:CATEGORY: cat\n:END:\n")
     (insert "\n\n")
     (org-insert-heading)
     (insert "task 1\n")
     (org-insert-heading)
     (insert "task 2\n")
     (org-set-property "category" "cat1")
     (org-insert-heading)
     (insert "task 3\n")
     (org-set-property "category" "cat2")
     (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "CATEGORY=\"cat1\""))))

(ert-deftest orb-test-agenda/tags-view-category-parent ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
     (insert ":PROPERTIES:\n:CATEGORY: cat\n:END:\n")
     (insert "\n\n")
     (org-insert-heading)
     (insert "task 1\n")
     (org-insert-heading)
     (insert "task 2\n")
     (org-set-property "category" "cat1")
     (org-insert-heading)
     (insert "task 3\n")
     (org-set-property "category" "cat2")
     (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil "CATEGORY=\"cat\""))))

(ert-deftest orb-test-agenda/tags-view-category-file ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
     (insert "\n\n")
     (org-insert-heading)
     (insert "task 1\n")
     (org-insert-heading)
     (insert "task 2\n")
     (org-set-property "category" "cat1")
     (org-insert-heading)
     (insert "task 3\n")
     (org-set-property "category" "cat2")
     (save-buffer))
    (orb-db-sync)
    (orb-test-agenda--compare
      (org-tags-view nil
                     (format "CATEGORY=\"%s\""
                             (file-name-base (car org-agenda-files)))))))

(ert-deftest orb-test-agenda/view-columns-extend-properties ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+COLUMNS: %EXTEND\n\n")
      (org-insert-heading)
      (insert "test extend\n")
      (org-todo)
      (org-set-property "EXTEND" "1")
      (org-set-property "EXTEND+" "2")
      (org-end-of-subtree)
      (org-insert-heading)
      (insert "test extend\n")
      (org-todo)
      (org-set-property "EXTEND+" "2")
      (org-set-property "EXTEND" "1")
      (org-end-of-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-view-columns-initially t))
      (orb-test-agenda--compare-overlays
        (org-todo-list "*")))))

(ert-deftest orb-test-agenda/view-columns-special-properties ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+COLUMNS: %ITEM %FILE %TODO %CATEGORY %PRIORITY %SCHEDULED %DEADLINE %CLOSED %TIMESTAMP %TIMESTAMP_IA %TAGS %ALLTAGS\n\n")
      (org-insert-heading)
      (insert "test category\n")
      (org-todo)
      (org-set-property "CATEGORY" "cat")
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "test priority\n")
      (org-todo)
      (org-priority-up)
      (org-priority-up)
      (org-insert-heading)
      (insert "test scheduled\n")
      (org-todo)
      (org-schedule nil "now")
      (org-insert-heading)
      (insert "test deadline\n")
      (org-todo)
      (org-deadline nil "now")
      (org-insert-heading)
      (insert "test closed\n")
      (org-todo)
      (let ((org-log-done 'time))
        (org-todo))
      (org-insert-heading)
      (insert "test timestamp\n")
      (org-todo)
      (org-time-stamp '(16) nil)
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "test timestamp ia\n")
      (org-todo)
      (org-time-stamp '(16) t)
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "test blocks\n")
      (org-todo)
      (org-time-stamp '(16))
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16)))
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "test blocks inactive\n")
      (org-todo)
      (org-time-stamp '(16) t)
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16) t))
      (org-end-of-subtree)
      (insert "\n")
      (org-insert-heading)
      (insert "test tags\n")
      (org-todo)
      (org-toggle-tag "a")
      (org-insert-heading)
      (insert "test all tags\n")
      (org-todo)
      (org-toggle-tag "b")
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-view-columns-initially t))
      (orb-test-agenda--compare-overlays
        (org-todo-list "*")))))

(ert-deftest orb-test-agenda/view-columns-summaries-number ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+COLUMNS: %ITEM %P(sum){+} %P(min){min} %P(max){max} %P(mean){mean} %P($){$}\n\n")
      (org-insert-heading)
      (insert "test 1\n")
      (org-todo)
      (org-insert-heading)
      (insert "test 2\n")
      (org-todo)
      (org-demote-subtree)
      (org-set-property "P" "2")
      (org-insert-heading)
      (insert "test 3\n")
      (org-todo)
      (org-set-property "P" "3")
      (org-insert-heading)
      (insert "test 4\n")
      (org-todo)
      (org-set-property "P" "6")
      (org-insert-heading)
      (insert "test 5\n")
      (org-todo)
      (org-set-property "P" "5")
      (org-demote-subtree)
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-view-columns-initially t))
      (orb-test-agenda--compare-overlays
        (org-todo-list "*")))))

(ert-deftest orb-test-agenda/view-columns-summaries-clocksum ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "#+COLUMNS: %ITEM %CLOCKSUM %CLOCKSUM_T\n\n")
      (let ((now (float-time nil)))
        (org-insert-heading)
        (insert "test 1\n")
        (org-todo)
        (insert ":LOGBOOK:\n")
        (insert "CLOCK: ")
        (org-insert-time-stamp (- now 60) 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert " => 00:01\n")
        (org-insert-time-stamp (- now 60) 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert " => 00:01\n")
        (insert ":END:\n\n")
        (org-insert-heading)
        (insert "test 2\n")
        (org-todo)
        (insert ":LOGBOOK:\n")
        (insert "CLOCK: ")
        (org-insert-time-stamp (- now 86400) 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert " => 24:00\n")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp (+ now 86400) 'with-hm 'inactive)
        (insert " => 24:00\n")
        (insert ":END:\n\n")
        (org-insert-heading)
        (insert "test 3\n")
        (org-todo)
        (org-insert-heading)
        (insert "test 4\n")
        (org-todo)
        (org-demote-subtree)
        (insert ":LOGBOOK:\n")
        (insert "CLOCK: ")
        (org-insert-time-stamp (- now 60) 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert " => 00:01\n")
        (insert ":END:\n\n")
        (insert "test 5\n")
        (org-todo)
        (org-demote-subtree)
        (insert ":LOGBOOK:\n")
        (insert "CLOCK: ")
        (org-insert-time-stamp (- now 60) 'with-hm 'inactive)
        (insert "--")
        (org-insert-time-stamp now 'with-hm 'inactive)
        (insert " => 00:01\n")
        (insert ":END:\n\n"))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-view-columns-initially t))
      (orb-test-agenda--compare-overlays
        (org-todo-list "*")))))


(ert-deftest orb-test-agenda/include-inactive ()
  (orb-test-with-temp-org-directory
    (orb-test-with-agenda-file-buffer
      (insert "\n\n")
      (org-insert-heading)
      (insert "task 1\n")
      (org-time-stamp '(16) t)
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16) t))
      (org-insert-heading)
      (insert "task 2\n")
      (org-time-stamp '(16) t)
      (let ((this-command 'org-time-stamp)
            (last-command 'org-time-stamp))
        (org-time-stamp '(16) t))
      (save-buffer))
    (orb-db-sync)
    (let ((org-agenda-include-inactive-timestamps t))
      (orb-test-agenda--compare
        (org-agenda-list nil "-1d" 3 nil)))))

(provide 'orb-test-agenda)
