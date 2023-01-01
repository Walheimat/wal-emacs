;;; wal-visuals-test.el --- Tests for visuals package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test custom functionality.

;;; Code:

(require 'wal-visuals nil t)

(ert-deftest test-wal/with-recent-files-excluded ()
  (should (equal '(testing this) (wal/with-recent-files-excluded #'wal/ra 'testing 'this))))

(ert-deftest test-wal/instead-show-biased-random ()
  (defvar dashboard-footer-messages '("Testing"))
  (should (equal "Testing" (wal/instead-show-biased-random))))

(ert-deftest test-wal/in-case-of-daemonp-add-different-hook ()
  (with-mock-all ((daemonp . #'always)
                  (add-hook . #'wal/rf))
    (should (equal 'server-after-make-frame-hook (wal/in-case-of-daemonp-add-different-hook)))))

(ert-deftest test-wal/with-common-ligatures ()
  (defvar wal/common-ligatures)
  (let ((fun #'wal/ra)
        (wal/common-ligatures '("?")))
    (should (equal '((test-mode) ("!" "?")) (wal/with-common-ligatures fun '(test-mode) '("!"))))))

;;; wal-visuals-test.el ends here
