;;; wal-terminal-test.el --- Test terminal package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-terminal nil t)

(ert-deftest test-wal-instead-truncate-buffer ()
  (bydi-with-mock eshell-truncate-buffer
    (wal-instead-truncate-buffer)

    (bydi-was-called eshell-truncate-buffer)))

;;; wal-terminal-test.el ends here
