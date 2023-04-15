;;; wal-edit-test.el --- Tests for edit package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-edit nil t)

(ert-deftest test-wal/before-mc ()
  (let ((wal/mc-conflicting-modes '(abbrev-mode)))

    (with-temp-buffer
      (abbrev-mode)

      (should abbrev-mode)

      (wal/before-mc)

      (should-not abbrev-mode)
      (should (equal wal/mc-disabled '(abbrev-mode))))))

(ert-deftest test-wal/after-mc ()
  (let ((wal/mc-disabled '(abbrev-mode)))

    (with-temp-buffer

      (should-not abbrev-mode)

      (wal/after-mc)

      (should abbrev-mode)
      (should-not wal/mc-disabled))))

(ert-deftest test-wal/tempel-comment ()
  (with-temp-buffer
    (emacs-lisp-mode)

    (should (string-equal (wal/tempel-comment (list 'c "testing")) ";; testing")))
  (with-temp-buffer
    (js-mode)

    (should (string-equal (wal/tempel-comment (list 'c "testing")) "// testing"))))

(ert-deftest test-wal/in-case-of-mc-mode-do-not-default ()
  (defvar multiple-cursors-mode)
  (let ((multiple-cursors-mode t))

    (should (wal/in-case-of-mc-mode-do-not-default)))
  (let ((multiple-cursors-mode nil))

    (should-not (wal/in-case-of-mc-mode-do-not-default))))

;;; wal-edit-test.el ends here
