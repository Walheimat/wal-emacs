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
  (with-mock wal/project-command #'wal/ra
    (should (equal (list 'test "Test project: ") (wal/project-test)))))

(ert-deftest test-wal/project-install ()
  (with-mock wal/project-command #'wal/ra
    (should (equal (list 'install "Install project: ") (wal/project-install)))))

;;; wal-workspace-test.el ends here
