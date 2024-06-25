;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest wal-aw ()
  :tags '(window)

  (bydi-match-expansion
   (wal-aw quit-window)
   '(defun wal-aw-quit-window (window)
     "Switch to WINDOW and then call `quit-window'."
     (aw-switch-to-window window)
     (call-interactively 'quit-window))))

;;; wal-windows-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
