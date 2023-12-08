;;; wal-emacs-test.el --- Tests for Emacs package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-emacs nil t)

(ert-deftest wal-flyspell-commit-messages ()
  :tags '(emacs)

  (bydi (flyspell-mode)

    (with-temp-buffer
      (wal-text-mode-flyspell-commit-messages)
      (bydi-was-not-called flyspell-mode)

      (rename-buffer "COMMIT_EDITMSG")
      (wal-text-mode-flyspell-commit-messages)

      (bydi-was-called flyspell-mode))))

(ert-deftest wal-with-page-offset ()
  :tags '(emacs)

  (let ((this-command 'doc-view-goto-page)
        (wal-doc-view-page-offset 4))

    (bydi-with-mock doc-view-goto-page
      (wal-with-page-offset #'doc-view-goto-page 3)
      (bydi-was-called-with doc-view-goto-page 7)))

  (let ((this-command 'doc-view-next-page)
        (wal-doc-view-page-offset 4))

    (bydi-with-mock doc-view-goto-page
      (wal-with-page-offset #'doc-view-goto-page 3)
      (bydi-was-called-with doc-view-goto-page 3))))

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

(ert-deftest wal-clear-registers ()
  :tags '(emacs user-facing)

  (let ((register-alist '((a . b))))

    (wal-clear-registers)

    (should-not register-alist)))

(ert-deftest wal-lighthouse ()
  :tags '(emacs user-facing)

  (bydi pulse-momentary-highlight-one-line
    (with-temp-buffer
      (insert "testing")
      (goto-char (point-max))

      (wal-lighthouse)

      (bydi-was-called-with pulse-momentary-highlight-one-line (list 8 'cursor)))))

(ert-deftest wal-compilation-buffer-p ()
  :tags '(emacs)

  (with-temp-buffer
    (compilation-mode)

    (should (wal-compilation-buffer-p (current-buffer)))))

(ert-deftest wal-consult-compilation-buffer--query ()
  :tags '(emacs)

  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-compilation-buffer--query) '(visibility buffer-name wal-compilation-buffer-p))))

;;; wal-emacs-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
