;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest test-wal/with-project-bounded-compilation ()
  (with-mock-all ((project-current . #'ignore)
                  (project-buffers . #'buffer-list))
    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))
      (should (wal/with-project-bounded-compilation fun)))))

(ert-deftest test-wal/project-command ()
  (let ((out nil)
        (wal/project-commands (list 'test (make-hash-table :test 'equal)))
        (wal/project-test-default-cmd "untest"))
    (with-mock-all ((project-current . #'always)
                    (project-root . (lambda (_) "/tmp/cmd"))
                    (read-shell-command . (lambda (_p prev)
                                            (setq out prev)
                                            "test"))
                    (compile . #'wal/rf))

      (should (string-equal "test" (wal/project-command 'test "Testing: ")))

      (should (string-equal "untest" out))

      (should (string-equal "test" (gethash "/tmp/cmd" (plist-get wal/project-commands 'test)))))))

(ert-deftest test-wal/project-test ()
  (with-mock-history (wal/project-command)
    (wal/project-test)
    (was-called-with wal/project-command '(test "Test project: "))))

(ert-deftest test-wal/project-install ()
  (with-mock-history (wal/project-command)
    (wal/project-install)
    (was-called-with wal/project-command '(install "Install project: "))))

;;; wal-workspace-test.el ends here
