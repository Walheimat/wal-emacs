;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Set up paths, stumping and report.

;;; Code:

(require 'bydi)
(require 'bydi-report)

(declare-function bydi-path-setup "ext:bydi.el")
(declare-function bydi-ert-runner-setup "ext:bydi.el")
(declare-function bydi-undercover-setup "ext:bydi.el")

(defun wal-test-helper--cold-p ()
  "Check if we're loading this file from a cold start."
  (not (bound-and-true-p wal-loaded)))

(defun wal-test-helper--path-setup ()
  "Set up paths."
  (defvar wal-emacs-config-default-path)
  (defvar wal-emacs-config-build-path)
  (defvar wal-emacs-config-lib-path)

  (cl-destructuring-bind (source-dir build-dir lib-dir _tools-dir)
      (bydi-path-setup (list "build" "lib" "tools"))

    (setq wal-emacs-config-default-path source-dir
          wal-emacs-config-build-path build-dir
          wal-emacs-config-lib-path lib-dir)))

(defvar wal-test-helper--stumps nil)

(defun wal-test-helper--use-package-setup ()
  "Stump `use-package' forms."
  (message "Stumping `use-package'")

  (defmacro use-package (package-name &rest _args)
    "Push message that PACKAGE-NAME would have been loaded."
    `(push ',package-name wal-test-helper--stumps)))

(defun wal-test-helper--report (&rest _)
  "Show the stumped packages."
  (when wal-test-helper--stumps
    (message
     "\nStumped the following `use-package' forms:\n%s"
     wal-test-helper--stumps)))

(defun wal-test-helper--setup ()
  "Set up everything."
  (when (wal-test-helper--cold-p)
    (bydi-undercover-setup (list "build/*.el"
                             "wal-prelude.el"
                             "tools/*.el"))

    (message "Cold start, setting up test helper")

    (wal-test-helper--path-setup)
    (wal-test-helper--use-package-setup)

    (setq auto-mode-alist nil)

    (bydi-ert-runner-setup #'wal-test-helper--report)))

(wal-test-helper--setup)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
