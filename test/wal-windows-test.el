;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest wal-in-case-of-single-window ()
  :tags '(windows)

  (let ((windows nil))
    (bydi ((:mock wal-interesting-windows :return windows)
           wal-switch-to-other-buffer)

      (should-not (wal-in-case-of-single-window-switch-to-other-buffer))

      (setq windows '(a b))

      (wal-in-case-of-single-window-switch-to-other-buffer)

      (bydi-was-called wal-switch-to-other-buffer))))

;;; wal-windows-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
