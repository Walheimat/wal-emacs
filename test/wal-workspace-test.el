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
  (let ((wal/project-commands (list 'test (make-hash-table :test 'equal)))
        (wal/project-test-default-cmd "untest")
        (wal/project-command-history nil)
        (entered-command nil))

    (with-mock ((project-current . #'always)
                (project-root . (lambda (_) "/tmp/cmd"))
                (project-name . (lambda (_) "Test Project"))
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

(ert-deftest test-wal/project-compile ()
  (with-mock (wal/project-command)
    (wal/project-compile)

    (was-called-with wal/project-command '(compile))))

(ert-deftest test-wal/project-test ()
  (with-mock (wal/project-command)
    (wal/project-test)

    (was-called-with wal/project-command '(test))))

(ert-deftest test-wal/project-install ()
  (with-mock (wal/project-command)

    (wal/project-install)

    (was-called-with wal/project-command '(install))))

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

;;; wal-workspace-test.el ends here
