;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros.

;;; Code:

(when (require 'undercover nil t)
  (cond
    ((getenv "CI")
     (undercover "wal/*.el"
                 (:report-format 'lcov)
                 (:send-report nil)))
    ((getenv "COVERAGE_WITH_JSON")
     (setq undercover-force-coverage t)
     (undercover "wal/*.el"
                 (:report-format 'simplecov)
                 (:report-file "./coverage/.resultset.json")
                 (:send-report nil)))
    (t
     (setq undercover-force-coverage t)
     (undercover "wal/*.el"
                 (:report-format 'text)
                 (:report-file "coverage.txt")
                 (:send-report nil)))))

(setq wal/booting nil)

;; Create a dummy `use-package' definition.
(defmacro use-package (package-name &rest _args)
  "Message that PACKAGE-NAME would have been loaded."
  `(message "Would have loaded %s" ',package-name))

(defmacro with-mock (name fun &rest body)
  "Evaluate BODY while mocking function NAME using FUN."
  (declare (indent defun))

  `(cl-letf (((symbol-function ',name) ,fun))
     ,@body))

(defmacro with-mock-all (flist &rest body)
  "Evaluate BODY mocking alist of functions FLIST."
  (declare (indent defun))

  `(cl-letf (,@(mapcar (lambda (it) `((symbol-function ',(car it)) ,(cdr it))) flist))
     ,@body))

(defmacro match-expansion (form &rest value)
  "Match expansion of FORM against VALUE."
  `(should (pcase (macroexpand-1 ',form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(defmacro wal/with-temp-file (filename &rest body)
  "Create and discard a file.

FILENAME is the name of the file, BODY the form to execute while
the file is alive.

The associated file buffer is also killed."
  (declare (indent defun))

  (let ((tmp-file (expand-file-name filename "/tmp")))

    `(progn
       (let ((wal/tmp-file ,tmp-file))
         (make-empty-file ,tmp-file)
         ,@body
         (kill-buffer (get-file-buffer ,tmp-file))
         (delete-file ,tmp-file)
         (message "deleted %s" ,tmp-file)))))

(when (getenv "CI")
  (add-to-list 'load-path (expand-file-name "wal" (getenv "GITHUB_WORKSPACE"))))

(require 'ert-x)
(require 'wal-func)
(require 'wal-look)
(require 'wal-fluff)
(require 'wal-fonts)

;;; test-helper.el ends here
