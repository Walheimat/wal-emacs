;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest test-wal-tab-bar-switch-to-buffer-tab ()
  (let ((found nil))

    (with-mock ((tab-bar-get-buffer-tab . (lambda (_) found))
                tab-bar-switch-to-tab
                switch-to-buffer
                select-window
                get-buffer-window)

      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (was-called-with switch-to-buffer (list 'buffer))
      (wal-clear-mocks)
      (setq found '((name . "test-tab")))

      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (was-called-with tab-bar-switch-to-tab "test-tab")
      (was-called-with get-buffer-window (list 'buffer))
      (was-called select-window))))

;;; wal-windows-test.el ends here
