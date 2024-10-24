;;; wal-edit-test.el --- Tests for edit package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-edit nil t)

(ert-deftest wal-before-mc ()
  :tags '(edit)

  (let ((wal-mc-conflicting-modes '(abbrev-mode)))

    (with-temp-buffer
      (setq abbrev-mode t)

      (wal-before-mc)

      (should-not abbrev-mode)
      (should (equal wal-mc-disabled '(abbrev-mode))))))

(ert-deftest wal-after-mc ()
  :tags '(edit)

  (let ((wal-mc-disabled '(abbrev-mode)))

    (bydi (abbrev-mode)
      (with-temp-buffer

        (should-not abbrev-mode)

        (wal-after-mc)

        (bydi-was-called abbrev-mode)
        (should-not wal-mc-disabled)))))

(ert-deftest wal-in-case-of-mc-mode-do-not-default ()
  :tags '(edit)

  (defvar multiple-cursors-mode nil)

  (let ((multiple-cursors-mode t))

    (should (wal-in-case-of-mc-mode-do-not-default)))
  (let ((multiple-cursors-mode nil))

    (should-not (wal-in-case-of-mc-mode-do-not-default))))

(ert-deftest wal-kmacro ()
  :tags '(emacs user-facing)

  (bydi (kmacro-end-macro
         kmacro-start-macro)

    (let ((defining-kbd-macro t))
      (wal-kmacro nil)

      (bydi-was-called kmacro-end-macro))

    (let ((defining-kbd-macro nil))
      (wal-kmacro nil)

      (bydi-was-called kmacro-start-macro))))

;;; wal-edit-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
