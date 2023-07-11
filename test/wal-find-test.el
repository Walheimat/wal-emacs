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

  (bydi ((:mock rg-read-pattern :return 'pattern)
         (:mock rg-read-files :return 'files)
         (:mock rg-project-root :return "/tmp/test")
         rg-run)
    (let ((rg-command-line-flags-function (lambda (_) 'flags)))

      (call-interactively 'wal-rg-project-literal)
      (bydi-was-called-with rg-run (list 'pattern 'files "/tmp/test" t nil 'flags)))))

(ert-deftest wal-dumb-jump-go ()
  (bydi (xref-find-definitions
         (:mock thing-at-point :return 'thing))
    (wal-dumb-jump-go)

    (bydi-was-called-with thing-at-point '(symbol))
    (bydi-was-called-with xref-find-definitions '(thing))))

;;; wal-find-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
