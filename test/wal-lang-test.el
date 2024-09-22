;;; wal-lang-test.el --- Tests for language package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lang nil t)

(ert-deftest wal-in-python-project-p ()
  :tags '(lang)

  (should-not (wal-in-python-project-p "noexist"))
  (should (wal-in-python-project-p "test")))

(ert-deftest wal-instead-delay-prettier-errors ()
  :tags '(lang)

  (bydi (delay-warning)
    (wal-instead-delay-prettier-errors "We are just %s %s" "testing" "this")

    (bydi-was-called-with delay-warning (list 'prettier "We are just testing this" :warning))))

(ert-deftest wal-livedown ()
  :tags '(lang user-facing)

  (bydi ((:othertimes executable-find)
         call-process-shell-command
         start-process-shell-command)

    (shut-up
      (ert-with-temp-file livedown
        :buffer livedown-buffer

        (with-current-buffer livedown-buffer
          (setq major-mode 'markdown-mode)

          (should-error (wal-livedown))

          (bydi-toggle-volatile 'executable-find)

          (wal-livedown t)

          (bydi-was-called call-process-shell-command :clear t)
          (bydi-was-not-called start-process-shell-command)

          (wal-livedown)

          (bydi-was-called-with start-process-shell-command
            (list '...
                  (format
                   "livedown start %s --open"
                   (buffer-file-name)))))))))

(ert-deftest wal-markdown-view ()
  :tags '(lang user-facing)

  (bydi (mixed-pitch-mode
         markdown-mode
         markdown-view-mode)

    (with-temp-buffer
      (setq major-mode 'markdown-mode)

      (wal-markdown-view)

      (bydi-was-called markdown-view-mode)
      (bydi-was-called-with mixed-pitch-mode 1)

      (bydi-clear-mocks)

      (setq major-mode 'markdown-view-mode)
      (wal-markdown-view)

      (bydi-was-called markdown-mode)
      (bydi-was-called-with mixed-pitch-mode -1)

      (setq major-mode 'text-mode)

      (should-error (wal-markdown-view)))))

(ert-deftest wal-with-bash-shell ()
  :tags '(lang)

  (should (equal "/bin/bash" (wal-with-bash-shell (lambda () shell-file-name)))))

(ert-deftest wal-java-test-dwim ()
  :tags '(lang user-facing)

  (bydi (dap-java-run-test-method
         transient-set
         (:mock wal-transient-grab :with (lambda (s) (if (string-equal s "mode") "run" "method"))))

    (wal-java-test-dwim)

    (bydi-was-called dap-java-run-test-method))

  (bydi (dap-java-run-test-method
         transient-set
         (:ignore wal-transient-grab))

    (shut-up (wal-java-test-dwim))

    (bydi-was-not-called dap-java-run-test-method)))

(ert-deftest wal-junit-match-file ()
  :tags '(lang)

  (let ((matched nil))

    (bydi ((:mock buffer-list :return (list "a" "b"))
           (:mock buffer-file-name :with (lambda (b) (concat "buffer-" b)))
           (:mock match-string :return matched))

      (setq matched "a")

      (should (string= "buffer-a" (wal-junit-match-file)))

      (setq matched "d")

      (should (string= "d" (wal-junit-match-file))))))

(ert-deftest wal-maybe-use-custom-css-checker ()
  :tags '(lang)

  (defvar lsp-after-open-hook)
  (defvar flycheck-checker)
  (bydi ((:always executable-find))
    (let ((lsp-after-open-hook nil)
          (flycheck-checker nil))
      (with-temp-buffer
        (setq-local major-mode 'scss-mode)
        (wal-maybe-use-custom-css-checker)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'scss-stylelint))

        (setq-local major-mode 'test-mode)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'css-stylelint))

        (setq-local major-mode 'less-css-mode)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'less-stylelint))))))

;;; wal-lang-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
