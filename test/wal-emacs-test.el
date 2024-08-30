;;; wal-emacs-test.el --- Tests for Emacs package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-emacs nil t)

(ert-deftest wal-account-for-commit-buffer ()
  :tags '(emacs)

  (bydi (electric-pair-local-mode)

    (with-temp-buffer
      (wal-account-for-commit-buffer)
      (bydi-was-not-called electric-pair-local-mode)

      (rename-buffer "COMMIT_EDITMSG")
      (wal-account-for-commit-buffer)

      (bydi-was-called electric-pair-local-mode))))

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

(ert-deftest wal-point-to-register ()
  :tags '(emacs)

  (ert-with-test-buffer (:name "point-to-register")

    (bydi ((:mock current-window-configuration :return 'window)
           (:mock point-marker :return 'point)
           (:mock register-read-with-preview :return "t")
           register-swap-out
           set-register)

      (call-interactively 'wal-point-to-register)

      (bydi-was-called-with set-register '("t" point) :clear t)

      (let ((current-prefix-arg '(4)))
        (call-interactively 'wal-point-to-register)

        (bydi-was-called-with set-register '("t" (window point)))))))

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

;;; wal-emacs-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
