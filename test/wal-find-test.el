;;; wal-find-test.el --- Tests for finding package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package currently doesn't have custom functionality.

;;; Code:

(require 'wal-find nil t)

(ert-deftest wal-rg-rerun-toggle-hidden ()
  :tags '(find user-facing)

  (bydi-with-mock (rg-rerun-toggle-flag)
    (wal-rg-rerun-toggle-hidden)
    (bydi-was-called-with rg-rerun-toggle-flag (list "--hidden"))))

(ert-deftest wal-rg-project-literal ()
  :tags '(find user-facing)

  (defvar rg-command-line-flags-function)

  (bydi ((:mock rg-read-pattern :return 'pattern)
         (:mock rg-read-files :return 'files)
         (:mock rg-project-root :return "/tmp/test")
         rg-run)
    (let ((rg-command-line-flags-function (lambda (_) 'flags)))

      (call-interactively 'wal-rg-project-literal)
      (bydi-was-called-with rg-run (list 'pattern 'files "/tmp/test" t nil 'flags)))))

(ert-deftest wal-dumb-jump-go ()
  :tags '(find user-facing)

  (bydi (xref-find-definitions
         (:mock thing-at-point :return 'thing))
    (wal-dumb-jump-go)

    (bydi-was-called-with thing-at-point '(symbol))
    (bydi-was-called-with xref-find-definitions '(thing))))

(ert-deftest wal-dogears-list ()
  :tags '(movement user-facing)

  (defvar dogears-list-buffer)

  (let ((dogears-list-buffer nil))

    (bydi (dogears-list
           (:spy quit-window))

      (wal-dogears-list)

      (bydi-was-called dogears-list t)

      (ert-with-test-buffer (:name "dogears")

        (setq dogears-list-buffer (current-buffer))

        (display-buffer dogears-list-buffer)

        (wal-dogears-list)

        (bydi-was-not-called dogears-list)
        (bydi-was-called quit-window)))))

;;; wal-find-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
