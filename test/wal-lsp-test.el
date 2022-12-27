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
  (with-mock lsp-deferred #'wal/rt
    (let ((wal/lsp-slow-modes '(text-mode)))
      (with-temp-buffer
        (text-mode)
        (should (equal 'testing (wal/lsp)))
        (should (equal '(orderless partial-completion basic) completion-styles))))))

(ert-deftest test-wal/lsp--sets-corfu ()
  (defvar corfu-auto-delay nil)
  (defvar corfu-auto-prefix nil)
  (with-mock lsp-deferred #'ignore
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
  (with-rf-mock eval
    (should (equal '(setf (lsp-session-server-id->folders (lsp-session)) (ht))
                   (wal/first-prevent-adding-other-projects)))))

(ert-deftest test-wal/dap-terminated ()
  (let ((out nil))
    (with-mock-all ((hydra-disable . (lambda () (add-to-list 'out 'hydra)))
                    (set-window-configuration . #'wal/rf))
      (let ((wal/dap-before 'test))
        (should (equal 'test (wal/dap-terminated nil)))))))

(ert-deftest test-wal/dap-session-created ()
  (with-mock-all ((current-window-configuration . #'wal/rt)
                  (delete-other-windows . #'wal/rt))

    (should (equal 'testing (wal/dap-session-created)))
    (should (equal 'testing wal/dap-before))))

(ert-deftest test-wal/dap-stopped ()
  (with-mock wal/dap-hydra/body #'wal/rt
    (should (equal 'testing (wal/dap-stopped nil)))))

;;; wal-lsp-test.el ends here
