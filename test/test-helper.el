;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)

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

(defvar wal/booting nil)

(defun wal/rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun wal/ra (&rest r)
  "Return all arguments R."
  r)

(defun wal/rt (&rest _r)
  "Return symbol `testing'."
  'testing)

;; Create a dummy `use-package' definition.
(defmacro use-package (package-name &rest _args)
  "Message that PACKAGE-NAME would have been loaded."
  `(message "Would have loaded %s" ',package-name))

(defmacro with-mock (name fun &rest body)
  "Evaluate BODY while mocking function NAME using FUN."
  (declare (indent defun))

  `(cl-letf (((symbol-function ',name) ,fun))
     ,@body))

(defvar wal/mock-history nil)

(defmacro with-mock-history (flist &rest body)
  "Evaluate BODY mocking list of functions FLIST.

The arguments passed to the mocked functions will be recorded in
a hash table.

Each item in FLIST can either be a function symbol or a cons cell
of shape (FUNCTION . MOCK-IMPLEMENTATION). The return value is
either the argument list or the result of the mock
implementation."
  (declare (indent defun))

  `(cl-letf ((wal/mock-history (make-hash-table :test 'equal))
             ,@(mapcar (lambda (it)
                         (cond
                          ((consp it)
                           (let ((fun (car it))
                                 (instead (cdr it)))
                             `((symbol-function ',fun)
                               (lambda (&rest r)
                                 (puthash ',fun r wal/mock-history)
                                 (apply ,instead r)))))
                           (t
                            `((symbol-function ',it)
                              (lambda (&rest r)
                                (puthash ',it r wal/mock-history))))))
                       flist))
     ,@body))

(defmacro was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  `(should (equal (gethash ',fun wal/mock-history) ,expected)))

(defmacro was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun wal/mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun wal/mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro with-default-mock (name &rest body)
  "Evaluate BODY while mocking function NAME using default mock."
  (declare (indent defun))

  `(with-mock ,name 'wal/ra ,@body))

(defmacro with-rf-mock (name &rest body)
  "Evaluate BODY while mocking function NAME returning first argument."
  (declare (indent defun))

  `(with-mock ,name 'wal/rf ,@body))

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
         (condition-case err
             (progn
               ,@body
               (kill-buffer (get-file-buffer ,tmp-file))
               (delete-file ,tmp-file)
               (message "Deleted %s" ,tmp-file))
           (error
            (delete-file ,tmp-file)
            (error "Deleted %s after error: %s" ,tmp-file err)))))))

(defvar wal/emacs-config-default-path)
(defvar wal/emacs-config-package-path)

(let* ((source-dir (or (getenv "GITHUB_WORKSPACE") default-directory))
       (package-dir (expand-file-name "wal" source-dir)))
  (message "Setting source dir to %s, package dir to %s" source-dir package-dir)
  (when (getenv "CI")
    (add-to-list 'load-path package-dir))
  (setq wal/emacs-config-default-path source-dir
        wal/emacs-config-package-path package-dir))

;;; test-helper.el ends here
