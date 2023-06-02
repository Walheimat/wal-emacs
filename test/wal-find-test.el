;;; wal-find-test.el --- Tests for finding package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package currently doesn't have custom functionality.

;;; Code:

(require 'wal-find nil t)

(ert-deftest test-wal-rg-rerun-toggle-hidden ()
  (bydi-with-mock (rg-rerun-toggle-flag)

    (wal-rg-rerun-toggle-hidden)

    (bydi-was-called-with rg-rerun-toggle-flag (list "--hidden"))))

(ert-deftest test-wal-rg-project-literal ()
  (defvar rg-command-line-flags-function)

  (bydi-with-mock ((rg-read-pattern . (lambda (_) 'pattern))
                   (rg-read-files . (lambda () 'files))
                   (rg-project-root . (lambda (_) "/tmp/test"))
                   rg-run)

    (let ((rg-command-line-flags-function (lambda (_) 'flags)))

      (call-interactively 'wal-rg-project-literal)

      (bydi-was-called-with rg-run (list 'pattern 'files "/tmp/test" t nil 'flags)))))

;;; wal-find-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
