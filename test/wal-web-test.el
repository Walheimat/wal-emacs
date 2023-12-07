;;; wal-web-test.el --- Tests for web package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-web nil t)

(ert-deftest wal-pick-windows-being-kept ()
  :tags '(web)

  (should (equal (wal-pick-windows-being-kept) (list t))))

;;; wal-web-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
