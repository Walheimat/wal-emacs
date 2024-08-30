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

(ert-deftest wal-rg-rerun-toggle-context ()
  :tags '(find user-facing just-testing)

  (defvar rg-cur-search nil)

  (bydi ((:mock rg-search-flags :var flags :initial (list "--context=4" "-i"))
         rg-rerun-toggle-flag)

    (wal-rg-rerun-toggle-context 5)

    (bydi-was-called-with rg-rerun-toggle-flag "--context=4" :clear t)

    (setq flags (list "-i"))

    (wal-rg-rerun-toggle-context 5)

    (bydi-was-called-with rg-rerun-toggle-flag "--context=5")))

(ert-deftest wal-rg--run ()
  :tags '(find)

  (defvar rg-command-line-flags-function)

  (bydi ((:mock rg-read-pattern :return 'pattern)
         (:mock rg-read-files :return 'files)
         (:mock rg-project-root :return "/tmp/test")
         rg-run)

    (let ((rg-command-line-flags-function (lambda (_) 'flags)))

      (wal-rg--run)
      (bydi-was-called-with rg-run (list 'pattern 'files "/tmp/test" nil nil 'flags)))))

(ert-deftest wal-rg-project-literal ()
  :tags '(find user-facing)

  (bydi wal-rg--run

    (wal-rg-project-literal)

    (bydi-was-called-with wal-rg--run '(nil t))))

(ert-deftest wal-rg-todos ()
  :tags '(find user-facing)

  (defvar hl-todo-keyword-faces)

  (let ((hl-todo-keyword-faces '(("TODO" . 0) ("TEST" . 1))))
    (bydi wal-rg--run
      (wal-rg-project-todos)
      (bydi-was-called-with wal-rg--run " (TODO|TEST): "))))

(ert-deftest wal-dumb-jump-go ()
  :tags '(find user-facing)

  (defvar xref-backend-functions)
  (defvar xref-prompt-for-identifier)

  (bydi ((:always xref-find-definitions)
         (:watch xref-prompt-for-identifier)
         (:watch xref-backend-functions))

    (wal-dumb-jump-go)

    (bydi-was-called xref-find-definitions)))

;;; wal-find-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
