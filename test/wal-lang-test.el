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

(ert-deftest wal-lsp-pyright-install-stubs ()
  :tags '(lang user-facing)

  (bydi ((:ignore wal-in-python-project-p))

    (should-error (wal-lsp-pyright-install-stubs) :type 'user-error))

  (let ((directory-exists t))

    (bydi ((:always wal-in-python-project-p)
           (:always project-current)
           make-directory
           (:mock project-root :return default-directory)
           display-buffer-in-side-window
           async-shell-command
           (:mock file-directory-p :return directory-exists))

      (should-error (wal-lsp-pyright-install-stubs) :type 'user-error)

      (setq directory-exists nil)

      (wal-lsp-pyright-install-stubs)
      (bydi-was-called make-directory)

      (let ((buf (get-buffer "*Pyright Stubs*"))
            (cmd (concat
                  "git clone https://github.com/microsoft/python-type-stubs "
                  (expand-file-name "typings" default-directory))))

        (bydi-was-called-with display-buffer-in-side-window (list buf '((side . bottom))))

        (bydi-was-called-with async-shell-command (list cmd buf))))))

(ert-deftest wal-otherwise-return-argument ()
  :tags '(lang)

  (should (equal 'testing (wal-otherwise-return-argument 'testing))))

(ert-deftest wal-instead-delay-prettier-errors ()
  :tags '(lang)

  (bydi (delay-warning)
    (wal-instead-delay-prettier-errors "We are just %s %s" "testing" "this")

    (bydi-was-called-with delay-warning (list 'prettier "We are just testing this" :warning))))

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

(ert-deftest wal-find-dart-flutter-sdk-dir ()
  :tags '(lang)

  (bydi ((:mock executable-find :with (lambda (x) (format "/usr/bin/%s" x)))
         (:mock shell-command-to-string :return "/tmp/sdk\n"))

    (should (string-equal "/tmp/sdk" (wal-find-dart-flutter-sdk-dir)))))

(ert-deftest wal-find-dart-sdk-dir ()
  :tags '(lang)

  (bydi ((:mock executable-find :with (lambda (x) (format "/usr/bin/%s" x)))
         (:mock shell-command-to-string :return "/tmp/sdk\n"))

    (should (string-equal "/tmp/sdk/bin/cache/dart-sdk" (wal-find-dart-sdk-dir)))))

(ert-deftest wal-lsp-dart-set-process-query-on-exit-flag ()
  :tags '(lang)

  (with-temp-buffer
    (setq lsp-dart-flutter-daemon-buffer-name (buffer-name))
    (start-process "sleep-test" (current-buffer) "sleep" "10")

    (should (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))

    (wal-lsp-dart-set-process-query-on-exit-flag)

    (should-not (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))))

(ert-deftest wal-lsp-dart-service-uri ()
  :tags '(lang user-facing)

  (bydi ((:mock lsp-workspace-get-metadata :with (lambda (_) (error "Testing"))))

    (shut-up
      (ert-with-message-capture messages
        (wal-lsp-dart-service-uri)
        (should (string-equal "Couldn’t get service URI: Testing\n" messages)))))

  (bydi ((:mock lsp-workspace-get-metadata :return "test-uri"))

    (shut-up
      (ert-with-message-capture messages
        (wal-lsp-dart-service-uri)
        (should (string-equal "Service URI (test-uri) copied to kill ring\n" messages))))

    (should (string-equal (car kill-ring) "test-uri"))))

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
