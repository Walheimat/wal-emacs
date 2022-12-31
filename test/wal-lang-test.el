;;; wal-lang-test.el --- Tests for language package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lang nil t)

(ert-deftest test-wal/in-python-project-p ()
  (should-not (wal/in-python-project-p "noexist"))
  (should (wal/in-python-project-p "test")))

(ert-deftest test-wal/lsp-pyright-install-stubs ()
  (with-mock wal/in-python-project-p #'ignore
    (should-error (wal/lsp-pyright-install-stubs) :type 'user-error))

  (with-mock-all ((wal/in-python-project-p . #'always)
                  (project-root . (lambda () default-directory))
                  (display-buffer-in-side-bffer . #'wal/rf)
                  (async-shell-command . #'wal/rf))
    (make-directory "typings")
    (should-error (wal/lsp-pyright-install-stubs) :type 'user-error)
    (delete-directory "typings")

    (should (string-equal (wal/lsp-pyright-install-stubs) (concat "git clone https://github.com/microsoft/python-type-stubs " (expand-file-name "typings" default-directory))))
    (delete-directory "typings")))

(ert-deftest test-wal/otherwise-return-argument ()
  (should (equal 'testing (wal/otherwise-return-argument 'testing))))

(ert-deftest test-wal/prettier-refresh ()
  (defvar prettier-processes)
  (let ((wal/prettier-timer t))
    (should-error (wal/prettier-refresh) :type 'user-error))

  (let ((out nil)
        (prettier-processes nil))
    (with-mock-all ((prettier--quit-all-processes . #'ignore)
                    (prettier-prettify . (lambda () (setq out 'prettified)))
                    (run-with-timer . #'wal/ra)
                    (hash-table-count . (lambda (_) 0))
                    (cancel-timer . #'ignore))
      (wal/prettier-refresh)

      (let ((fun (nth 2 wal/prettier-timer)))
        (funcall fun)
        (should (equal 'prettified out))))))

(ert-deftest test-wal/markdown-view ()
  (with-mock-all ((mixed-pitch-mode . #'wal/rf)
                  (markdown-mode . (lambda () (setq major-mode 'markdown-mode)))
                  (markdown-view-mode . (lambda () (setq major-mode 'markdown-view-mode))))
    (with-temp-buffer
      (markdown-mode)
      (should (eq 1 (wal/markdown-view)))
      (should (equal major-mode 'markdown-view-mode))
      (should (eq -1 (wal/markdown-view)))
      (should (equal major-mode 'markdown-mode))
      (text-mode)
      (should-error (wal/markdown-view)))))

(ert-deftest test-wal/find-dart-flutter-sdk-dir ()
  (with-mock-all ((executable-find . (lambda (x) (format "/usr/bin/%s" x)))
                  (shell-command-to-string . (lambda (_) "/tmp/sdk\n")))
    (should (string-equal "/tmp/sdk" (wal/find-dart-flutter-sdk-dir)))))

(ert-deftest test-wal/find-dart-sdk-dir ()
  (with-mock-all ((executable-find . (lambda (x) (format "/usr/bin/%s" x)))
                  (shell-command-to-string . (lambda (_) "/tmp/sdk\n")))
    (should (string-equal "/tmp/sdk/bin/cache/dart-sdk" (wal/find-dart-sdk-dir)))))

(ert-deftest test-wal/lsp-dart-set-process-query-on-exit-flag ()
  (with-temp-buffer
    (setq lsp-dart-flutter-daemon-buffer-name (buffer-name))
    (start-process "sleep-test" (current-buffer) "sleep" "10")
    (should (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))
    (wal/lsp-dart-set-process-query-on-exit-flag)
    (should-not (process-query-on-exit-flag (get-buffer-process lsp-dart-flutter-daemon-buffer-name)))))

(ert-deftest test-wal/lsp-dart-service-uri ()
  (with-mock lsp-workspace-get-metadata (lambda (_) (error "testing"))
    (should (string-equal "Couldn’t get service URI: testing" (wal/lsp-dart-service-uri))))
  (with-mock lsp-workspace-get-metadata (lambda (_) "test-uri")
    (should (string-equal "Service URI (test-uri) copied to kill ring" (wal/lsp-dart-service-uri)))
    (should (string-equal (car kill-ring) "test-uri"))))

(ert-deftest test-wal/with-bash-shell ()
  (should (equal "/bin/bash" (wal/with-bash-shell (lambda () shell-file-name)))))

(ert-deftest test-wal/java-test-dwim ()
  (with-mock-all ((dap-java-run-test-method . (lambda () (interactive) 'run-method))
                  (transient-set . #'ignore))
    (with-mock wal/transient-grab (lambda (s) (if (string-equal s "mode") "run" "method"))
      (should (equal 'run-method (wal/java-test-dwim))))))

(ert-deftest test-wal/with-json-data-ignored-for-gdscript ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "jsonrpc" "2.0" table)
    (with-temp-buffer
      (setq major-mode 'gdscript-mode)
      (should-not (wal/with-json-data-ignored-for-gdscript #'always table))
      (remhash "jsonrpc" table)
      (should (wal/with-json-data-ignored-for-gdscript #'always table))
      (text-mode)
      (puthash "jsonrpc" "2.0" table)
      (should (wal/with-json-data-ignored-for-gdscript #'always table)))))

;;; wal-lang-test.el ends here
