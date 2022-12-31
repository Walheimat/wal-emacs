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

;;; wal-workspace-test.el ends here
