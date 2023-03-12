;;; wal-emacs-test.el --- Tests for Emacs package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-emacs nil t)

(ert-deftest test-wal/flyspell-commit-messages ()
  (with-mock (flyspell-mode)

    (with-temp-buffer
      (wal/flyspell-commit-messages)
      (was-not-called flyspell-mode)

      (rename-buffer "COMMIT_EDITMSG")
      (wal/flyspell-commit-messages)

      (was-called flyspell-mode))))

(ert-deftest test-wal/flycheck-on-save ()
  (with-mock (flycheck-mode)

    (defvar flycheck-check-syntax-automatically nil)

    (with-temp-buffer
      (wal/flycheck-on-save)
      (was-called flycheck-mode)

      (should (equal '(save) flycheck-check-syntax-automatically)))))


(ert-deftest test-wal/kmacro ()
  (with-mock (kmacro-end-macro
              kmacro-start-macro)

    (let ((defining-kbd-macro t))
      (wal/kmacro nil)

      (was-called kmacro-end-macro))

    (let ((defining-kbd-macro nil))
      (wal/kmacro nil)

      (was-called kmacro-start-macro))))

(ert-deftest test-wal/clear-registers ()
  (let ((register-alist '((a . b))))

    (wal/clear-registers)

    (should-not register-alist)))

(ert-deftest test-wal/lighthouse ()
  (with-mock pulse-momentary-highlight-one-line
    (with-temp-buffer
      (insert "testing")
      (goto-char (point-max))

      (wal/lighthouse)

      (was-called-with pulse-momentary-highlight-one-line (list 8 'cursor)))))

;;; wal-emacs-test.el ends here
