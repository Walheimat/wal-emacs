;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest test-wal/with-project-bounded-compilation ()
  (with-mock ((project-current . #'ignore)
              (project-buffers . #'buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (wal/with-project-bounded-compilation fun)))))

(ert-deftest test-wal/project-command--buffer-name ()
  (let ((wal/project-current-command "test"))
    (should (string-equal (wal/project-command--buffer-name nil) "*project-test*")))

  (should (string-equal (wal/project-command--buffer-name nil) "*project-compile*")))

(ert-deftest test-wal/project-command ()
  (defvar wal/project-test-default-cmd nil)

  (let ((wal/project-commands (list 'test (make-hash-table :test 'equal)))
        (wal/project-test-default-cmd "untest")
        (wal/project-command-history nil)
        (entered-command nil))

    (with-mock ((project-current . #'always)
                (project-root . (lambda (_) "/tmp/cmd"))
                (project-name . (lambda (_) "Test Project"))
                (project--value-in-dir . (lambda (&rest _) wal/project-test-default-cmd))
                compile
                (read-shell-command . (lambda (&rest _) entered-command)))

      (setq entered-command "test")

      (wal/project-command 'test)
      (was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'wal/project-command-history))
      (was-called-with compile "test")

      (setq entered-command "best")
      (wal/project-command 'test)
      (was-called-with compile "best")

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get wal/project-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get wal/project-commands 'test)) 0))))))

(ert-deftest test-wal/project-create-command ()
  (with-mock ((make-hash-table . (lambda (&rest _) 'hash-table)))
    (match-expansion
     (wal/project-create-command test)
     `(progn
        (defvar-local wal/project-test-default-cmd nil)
        (defun wal/project-test nil "Test the current project."
               (interactive)
               (wal/project-command 'test))
        (setq wal/project-commands (plist-put wal/project-commands 'test hash-table))
        (advice-add 'wal/project-test :around #'wal/with-project-bounded-compilation)
        (bind-key "t" 'wal/project-test wal/project-prefix-map)))))

(ert-deftest test-wal/project-select-command ()
  (with-mock ((completing-read . (lambda (&rest _) "test"))
              wal/project-command)
    (call-interactively 'wal/project-select-command)

    (was-called-with wal/project-command (list 'test))))

(ert-deftest test-wal/project-find-rg ()
  (with-mock ((rg-read-pattern . #'wal/rt)
              (rg-read-files . #'wal/rt)
              (project-root . (lambda (&rest _) "/tmp/project"))
              project-current
              rg-run)

    (defvar rg-command-line-flags-function)
    (let ((rg-command-line-flags-function #'wal/rt))
      (call-interactively 'wal/project-find-rg))

    (was-called-with rg-run (list 'testing 'testing "/tmp/project" nil nil 'testing))))

(ert-deftest test-wal/project-consult-buffer ()
  (defvar consult-project-buffer-sources)
  (with-mock (consult-buffer)
    (let ((consult-project-buffer-sources 'testing))

      (wal/project-consult-buffer)

      (was-called-with consult-buffer (list 'testing)))))

(ert-deftest test-wal/project-magit-status ()
  (with-mock (magit-status
              (project-root . (lambda (_) "/tmp/test"))
              (project-current . (lambda (&rest _) (list 'vc 'Git "/tmp/test"))))

    (wal/project-magit-status)

    (was-called-with magit-status (list "/tmp/test"))))

(ert-deftest test-wal/project-magit-status--ignores-if-no-vc ()
  (ert-with-message-capture messages
    (with-mock ((project-current . (lambda (&rest _) (list 'vc nil "/tmp/test")))
                (project-root . (lambda (&rest _) "/tmp/test"))
                magit-status)

    (wal/project-magit-status)

    (was-not-called magit-status)

    (should (string= "Project at ’/tmp/test’ is not version-controlled\n" messages)))))

(ert-deftest test-wal/project-dired-root ()
  (with-mock (project-current (project-root . (lambda (&rest _) "/tmp/test")) dired)

    (wal/project-dired-root)

    (was-called-with dired (list "/tmp/test"))))

(ert-deftest test-wal/project--buffer-root ()
  (with-mock ((project-current . (lambda (&rest _r) '(vc Git "/tmp")))
              project-root)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/test-buffer/file.test")

      (wal/project--buffer-root (current-buffer))

      (was-called-with project-current '(nil "/tmp/test-buffer/"))
      (was-called-with project-root (list '(vc Git "/tmp"))))

    (with-temp-buffer
      (setq dired-directory "/tmp/test-buffer/")

      (wal/project--buffer-root (current-buffer))

      (was-called-with project-current '(nil "/tmp/test-buffer/"))
      (was-called-with project-root (list '(vc Git "/tmp"))))))

;;; wal-workspace-test.el ends here
