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
        (wal/project-test-default-cmd "untest"))

    (with-mock ((project-current . #'always)
                (project-root . (lambda (_) "/tmp/cmd"))
                (read-shell-command . (lambda (&_rest _) "test"))
                compile)

      (wal/project-command 'test "Testing: ")

      (was-called-with read-shell-command (list "Testing: " "untest"))
      (was-called-with compile "test")
      (should (string-equal "test" (gethash "/tmp/cmd" (plist-get wal/project-commands 'test)))))))

(ert-deftest test-wal/project-compile ()
  (with-mock (wal/project-command)
    (wal/project-compile)

    (was-called-with wal/project-command '(compile "Compile project: "))))

(ert-deftest test-wal/project-test ()
  (with-mock (wal/project-command)
    (wal/project-test)

    (was-called-with wal/project-command '(test "Test project: "))))

(ert-deftest test-wal/project-install ()
  (with-mock (wal/project-command)

    (wal/project-install)

    (was-called-with wal/project-command '(install "Install project: "))))

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

;;; wal-workspace-test.el ends here
