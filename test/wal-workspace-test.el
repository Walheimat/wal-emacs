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

(ert-deftest test-wal/project-test ()
  (with-mock (wal/project-command)
    (wal/project-test)

    (was-called-with wal/project-command '(test "Test project: "))))

(ert-deftest test-wal/project-install ()
  (with-mock (wal/project-command)

    (wal/project-install)

    (was-called-with wal/project-command '(install "Install project: "))))

;;; wal-workspace-test.el ends here
