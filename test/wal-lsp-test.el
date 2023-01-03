;;; wal-lsp-test.el --- Tests for LSP package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lsp nil t)

(ert-deftest test-wal/slow-lsp-p ()
  (let ((wal/lsp-slow-modes '(test-mode)))

    (should (wal/slow-lsp-p 'test-mode))))

(ert-deftest test-wal/lsp--sets-styles-for-slow-modes ()
  (with-mock lsp-deferred

    (let ((wal/lsp-slow-modes '(text-mode)))

      (with-temp-buffer
        (text-mode)
        (wal/lsp)

        (was-called lsp-deferred)
        (should (equal '(orderless partial-completion basic) completion-styles))))))

(ert-deftest test-wal/lsp--sets-corfu ()
  (defvar corfu-auto-delay nil)
  (defvar corfu-auto-prefix nil)

  (with-mock lsp-deferred
    (with-temp-buffer
      (wal/lsp)

      (should (equal 0.1 corfu-auto-delay))
      (should (equal 0 corfu-auto-prefix)))))

(ert-deftest test-wal/lsp-completion-with-corfu ()
  (let ((completion-category-defaults '((lsp-capf (styles other))))
        (completion-styles '(testful)))

    (wal/lsp-completion-with-corfu)

    (should (equal '((lsp-capf (styles testful))) completion-category-defaults))))

(ert-deftest test-wal/first-prevent-adding-other-projects ()
  (with-mock eval

    (wal/first-prevent-adding-other-projects)

    (was-called-with eval (list '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))))

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
  (with-mock wal/dap-hydra/body

    (wal/dap-stopped nil)

    (was-called wal/dap-hydra/body)))

;;; wal-lsp-test.el ends here
