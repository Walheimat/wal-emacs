;;; wal-lang-test.el --- Tests for language package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lang nil t)

(ert-deftest test-wal-in-python-project-p ()
  (should-not (wal-in-python-project-p "noexist"))
  (should (wal-in-python-project-p "test")))

(ert-deftest test-wal-lsp-pyright-install-stubs ()
  (with-mock ((wal-in-python-project-p . #'ignore))

    (should-error (wal-lsp-pyright-install-stubs) :type 'user-error))

  (with-mock ((wal-in-python-project-p . #'always)
              (project-root . (lambda () default-directory))
              display-buffer-in-side-window
              async-shell-command)

    (make-directory "typings")

    (should-error (wal-lsp-pyright-install-stubs) :type 'user-error)

    (delete-directory "typings")

    (wal-lsp-pyright-install-stubs)

    (let ((buf (get-buffer "*Pyright Stubs*"))
          (cmd (concat
                "git clone https://github.com/microsoft/python-type-stubs "
                (expand-file-name "typings" default-directory))))

      (was-called-with display-buffer-in-side-window (list buf '((side . bottom))))

      (was-called-with async-shell-command (list cmd buf)))

    (delete-directory "typings")))

(ert-deftest test-wal-otherwise-return-argument ()
  (should (equal 'testing (wal-otherwise-return-argument 'testing))))

(ert-deftest test-wal-prettier-refresh ()
  (defvar prettier-processes)
  (let ((wal-prettier-timer t))

    (should-error (wal-prettier-refresh) :type 'user-error))

  (let ((prettier-processes nil))

    (with-mock (prettier--quit-all-processes
                prettier-prettify
                run-with-timer
                (hash-table-count . (lambda (_) 0))
                cancel-timer)

      (wal-prettier-refresh)

      (let ((fun (nth 2 wal-prettier-timer)))

        (funcall fun)

        (was-called cancel-timer)
        (was-called prettier-prettify)))))

(ert-deftest test-wal-instead-delay-prettier-errors ()
  (with-mock (delay-warning)
    (wal-instead-delay-prettier-errors "We are just %s %s" "testing" "this")

    (was-called-with delay-warning (list 'prettier "We are just testing this" :warning))))

(ert-deftest test-wal-markdown-view ()
  (with-mock (mixed-pitch-mode
              markdown-mode
              markdown-view-mode)

    (with-temp-buffer
      (setq major-mode 'markdown-mode)

      (wal-markdown-view)

      (was-called markdown-view-mode)
      (was-called-with mixed-pitch-mode 1)

      (wal-clear-mocks)

      (setq major-mode 'markdown-view-mode)
      (wal-markdown-view)

      (was-called markdown-mode)
      (was-called-with mixed-pitch-mode -1)

      (text-mode)

      (should-error (wal-markdown-view)))))

(ert-deftest test-wal-find-dart-flutter-sdk-dir ()
  (with-mock ((executable-find . (lambda (x) (format "/usr/bin/%s" x)))
              (shell-command-to-string . (lambda (_) "/tmp/sdk\n")))

    (should (string-equal "/tmp/sdk" (wal-find-dart-flutter-sdk-dir)))))

(ert-deftest test-wal-find-dart-sdk-dir ()
  (with-mock ((executable-find . (lambda (x) (format "/usr/bin/%s" x)))
              (shell-command-to-string . (lambda (_) "/tmp/sdk\n")))

    (should (string-equal "/tmp/sdk/bin/cache/dart-sdk" (wal-find-dart-sdk-dir)))))

(ert-deftest test-wal-lsp-dart-set-process-query-on-exit-flag ()
  (with-temp-buffer
    (setq lsp-dart-flutter-daemon-buffer-name (buffer-name))
    (start-process "sleep-test" (current-buffer) "sleep" "10")

    (should (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))

    (wal-lsp-dart-set-process-query-on-exit-flag)

    (should-not (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))))

(ert-deftest test-wal-lsp-dart-service-uri ()
  (with-mock ((lsp-workspace-get-metadata . (lambda (_) (error "testing"))))

    (should (string-equal "Couldn’t get service URI: testing" (wal-lsp-dart-service-uri))))

  (with-mock ((lsp-workspace-get-metadata . (lambda (_) "test-uri")))

    (should (string-equal "Service URI (test-uri) copied to kill ring" (wal-lsp-dart-service-uri)))

    (should (string-equal (car kill-ring) "test-uri"))))

(ert-deftest test-wal-with-bash-shell ()
  (should (equal "/bin/bash" (wal-with-bash-shell (lambda () shell-file-name)))))

(ert-deftest test-wal-java-test-dwim ()
  (with-mock (dap-java-run-test-method
              transient-set
              (wal-transient-grab . (lambda (s) (if (string-equal s "mode") "run" "method"))))

    (wal-java-test-dwim)

    (was-called dap-java-run-test-method))

  (with-mock (dap-java-run-test-method
              transient-set
              (wal-transient-grab . #'ignore))

    (wal-java-test-dwim)

    (was-not-called dap-java-run-test-method)))

(ert-deftest test-wal-junit-match-file ()
  (let ((matched nil))

    (with-mock ((buffer-list . (lambda () (list "a" "b")))
                (buffer-file-name . (lambda (b) (concat "buffer-" b)))
                (match-string . (lambda (_) matched)))

      (setq matched "a")

      (should (string= "buffer-a" (wal-junit-match-file)))

      (setq matched "d")

      (should (string= "d" (wal-junit-match-file))))))

(ert-deftest test-wal-maybe-use-custom-css-checker ()
  (defvar lsp-after-open-hook)
  (defvar flycheck-checker)
  (with-mock ((executable-find . #'always))
    (let ((lsp-after-open-hook nil)
          (flycheck-checker nil))
      (with-temp-buffer
        (setq-local major-mode 'scss-mode)
        (wal-maybe-use-custom-css-checker)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'wal-scss-stylelint))

        (setq-local major-mode 'test-mode)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'css-stylelint))

        (setq-local major-mode 'less-css-mode)
        (run-hooks 'lsp-after-open-hook)

        (should (equal flycheck-checker 'wal-less-stylelint))))))

(ert-deftest test-wal-with-json-data-ignored-for-gdscript ()
  (let ((table (make-hash-table :test 'equal)))

    (puthash "jsonrpc" "2.0" table)

    (with-temp-buffer
      (setq major-mode 'gdscript-mode)

      (should-not (wal-with-json-data-ignored-for-gdscript #'always table))

      (remhash "jsonrpc" table)

      (should (wal-with-json-data-ignored-for-gdscript #'always table))

      (text-mode)
      (puthash "jsonrpc" "2.0" table)

      (should (wal-with-json-data-ignored-for-gdscript #'always table)))))

;;; wal-lang-test.el ends here
