;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest wal-aw-consult-buffer ()
  :tags '(window)

  (bydi (aw-switch-to-window
         consult-buffer)

    (wal-aw-consult-buffer 'window)

    (bydi-was-called-with aw-switch-to-window 'window)
    (bydi-was-called consult-buffer)))

;;; wal-windows-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
