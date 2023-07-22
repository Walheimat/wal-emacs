;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest test-wal-tab-bar-switch-to-buffer-tab ()
  (let ((found nil))

    (bydi ((:mock tab-bar-get-buffer-tab :return found)
           tab-bar-switch-to-tab
           switch-to-buffer
           select-window
           get-buffer-window)
      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (bydi-was-called-with switch-to-buffer (list 'buffer))
      (bydi-clear-mocks)
      (setq found '((name . "test-tab")))

      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (bydi-was-called-with tab-bar-switch-to-tab "test-tab")
      (bydi-was-called-with get-buffer-window (list 'buffer))
      (bydi-was-called select-window))))

(ert-deftest wal-tab-bar-rename-from-project ()
  (bydi ((:always project-current)
         (:mock project-name :return "Test")
         tab-bar-rename-tab)

    (wal-tab-bar-rename-from-project)

    (bydi-was-called-with tab-bar-rename-tab "Test")))

;;; wal-windows-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
