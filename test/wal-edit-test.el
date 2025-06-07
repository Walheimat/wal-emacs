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

(ert-deftest wal-hs-cycle ()
  :tags '(edit)

  (bydi (hs-hide-level
         hs-show-block
         hs-hide-block
         (:sometimes hs-already-hidden-p)
         (:watch this-command))

    (call-interactively 'wal-hs-cycle)
    (bydi-was-called-with hs-hide-level 1 :clear t)
    (bydi-was-set-to this-command 'hs-cycle-children)

    (bydi-toggle-volatile 'hs-already-hidden-p)
    (call-interactively 'wal-hs-cycle)
    (bydi-was-called hs-hide-block)
    (bydi-was-set-to-last this-command 'hs-hide-block)

    (setq last-command 'hs-cycle-children)
    (call-interactively 'wal-hs-cycle)
    (bydi-was-called-n-times hs-show-block 2)
    (bydi-was-set-to-last this-command 'hs-cycle-subtree)

    (setq last-command 'hs-cycle-subtree)
    (call-interactively 'wal-hs-cycle)
    (bydi-was-called hs-hide-block)
    (bydi-was-set-to-last this-command 'hs-hide-block)

    (setq last-command 'hs-cycle)
    (call-interactively 'wal-hs-cycle)
    (bydi-was-called-with hs-hide-level 1)

    (funcall 'wal-hs-cycle 3)
    (bydi-was-called-last-with hs-hide-level 3)
    (bydi-was-set-to-last this-command 'hs-hide-level)))

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
