;;; wal-find-test.el --- Tests for finding package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package currently doesn't have custom functionality.

;;; Code:

(require 'wal-find nil t)

(ert-deftest test-wal/rg-rerun-toggle-hidden ()
  (with-mock (rg-rerun-toggle-flag)

    (wal/rg-rerun-toggle-hidden)

    (was-called-with rg-rerun-toggle-flag (list "--hidden"))))

;;; wal-find-test.el ends here
