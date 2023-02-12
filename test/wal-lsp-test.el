;;; wal-lsp-test.el --- Tests for LSP package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lsp nil t)

(ert-deftest test-wal/slow-lsp-p ()
  (let ((wal/lsp-slow-modes '(test-mode)))

    (should (wal/slow-lsp-p 'test-mode))))

(ert-deftest test-wal/lsp--does-not-set-styles-for-slow-modes ()
  (with-mock lsp-deferred

    (let ((wal/lsp-slow-modes '(text-mode)))

      (with-temp-buffer
        (text-mode)
        (wal/lsp)

        (was-called lsp-deferred)
        (should (equal '(basic partial-completion emacs22) completion-styles))))))

(ert-deftest test-wal/lsp--otherwise-sets-styles ()
  (with-mock lsp-deferred

    (let ((wal/lsp-slow-modes nil))

      (with-temp-buffer
        (text-mode)
        (wal/lsp)

        (was-called lsp-deferred)
        (should (equal '(orderless partial-completion basic) completion-styles))))))

(ert-deftest test-wal/lsp-completion ()
  (let ((completion-category-defaults '((lsp-capf (styles other))))
        (completion-styles '(testful)))

    (wal/lsp-completion)

    (should (equal '((lsp-capf (styles testful))) completion-category-defaults))))

(ert-deftest test-wal/first-prevent-adding-other-projects ()
  (with-mock eval

    (wal/first-prevent-adding-other-projects)

    (was-called-with eval (list '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))))

(ert-deftest test-wal/lsp-ignore-directory--escape ()
  (should (string= "[/\\\\]tests\\'" (wal/lsp-ignore-directory--escape "tests")))
  (should (string= "[/\\\\]\\.tests\\'" (wal/lsp-ignore-directory--escape ".tests"))))

(ert-deftest test-wal/lsp-ignore-directory ()
(with-mock (wal/append (wal/lsp-ignore-directory--escape . (lambda (it) it)))

  (wal/lsp-ignore-directory "test")

  (was-called-with wal/append (list 'lsp-file-watch-ignored-directories '("test")))

  (wal/clear-mocks)

  (wal/lsp-ignore-directory '("test" "best"))

  (was-called-with wal/append (list 'lsp-file-watch-ignored-directories '("test" "best")))))

(ert-deftest test-wal/dap-terminated ()
  (with-mock (hydra-disable
              set-window-configuration)

    (let ((wal/dap-before 'test))
      (wal/dap-terminated nil)

      (was-called hydra-disable)
      (was-called set-window-configuration))))

(ert-deftest test-wal/dap-session-created ()
  (with-mock ((current-window-configuration . #'wal/rt)
              delete-other-windows)

    (wal/dap-session-created)

    (was-called delete-other-windows)
    (should (equal 'testing wal/dap-before))))

(ert-deftest test-wal/dap-stopped ()
  (with-mock dap-hydra

    (wal/dap-stopped nil)

    (was-called dap-hydra)))

(ert-deftest test-wal/ignore-if-no-lsp ()
  (defvar lsp-mode)
  (with-mock (message)
    (let ((lsp-mode nil))
      (should-not (wal/ignore-if-no-lsp))
      (was-called-with message "Not in a LSP buffer")
      (wal/clear-mocks))

    (let ((lsp-mode t))
      (should (wal/ignore-if-no-lsp))
      (was-not-called message))))

;;; wal-lsp-test.el ends here
