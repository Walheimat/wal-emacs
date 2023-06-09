;;; wal-lsp-test.el --- Tests for LSP package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-lsp nil t)

(ert-deftest test-wal-lsp-completion ()
  (let ((completion-category-defaults '((lsp-capf (styles other))))
        (completion-styles '(testful)))

    (bydi ((:ignore harpoon-slow-lsp-p))
      (wal-lsp-completion)

      (should (equal '((lsp-capf (styles testful))) completion-category-defaults)))))

(ert-deftest test-wal-first-prevent-adding-other-projects ()
  (bydi eval

    (wal-first-prevent-adding-other-projects)

    (bydi-was-called-with eval (list '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))))

(ert-deftest test-wal-dap-terminated ()
  (bydi (hydra-disable
         set-window-configuration)

    (let ((wal-dap-before 'test))
      (wal-dap-terminated nil)

      (bydi-was-called hydra-disable)
      (bydi-was-called set-window-configuration))))

(ert-deftest test-wal-dap-session-created ()
  (bydi ((:mock current-window-configuration :with bydi-rt)
         delete-other-windows)

    (wal-dap-session-created)

    (bydi-was-called delete-other-windows)
    (should (equal 'testing wal-dap-before))))

(ert-deftest test-wal-dap-stopped ()
  (bydi dap-hydra

    (wal-dap-stopped nil)

    (bydi-was-called dap-hydra)))

(ert-deftest test-wal-ignore-if-no-lsp ()
  (defvar lsp-mode)
  (bydi (message)
    (let ((lsp-mode nil))
      (should-not (wal-ignore-if-no-lsp))
      (bydi-was-called-with message "Not in a LSP buffer")
      (bydi-clear-mocks))

    (let ((lsp-mode t))
      (should (wal-ignore-if-no-lsp))
      (bydi-was-not-called message))))

;;; wal-lsp-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
