;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat)

(when (require 'undercover nil t)
  (cond
   ((getenv "CI")
    (undercover "wal/*.el"
                (:report-format 'lcov)
                (:send-report nil)))
   ((getenv "COVERAGE_WITH_JSON")
    (setq undercover-force-coverage t
          undercover--merge-report nil)
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

(defun wal/cold-start-p ()
  "Check if we're loading this file from a cold start."
  (not (featurep 'use-package)))

(defun wal/rf (a &rest _r)
  "Return first argument passed A."
  a)

(defun wal/ra (&rest r)
  "Return all arguments R."
  r)

(defun wal/rt (&rest _r)
  "Return symbol `testing'."
  'testing)

(defmacro with-mock-old (name fun &rest body)
  "Evaluate BODY while mocking function NAME using FUN."
  (declare (indent defun))

  `(cl-letf (((symbol-function ',name) ,fun))
     ,@body))

(defvar wal/mock-history nil)

(defmacro with-mock (to-mock &rest body)
  "Evaluate BODY mocking list of function(s) TO-MOCK.

TO-MOCK maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol or a cons
cell of shape (FUNCTION . MOCK-IMPLEMENTATION). The return value
is either the argument list or the result of the mock
implementation."
  (declare (indent defun))

  `(cl-letf* ((wal/mock-history (make-hash-table :test 'equal))
              (remember (lambda (fun args)
                          (let* ((prev (gethash fun wal/mock-history))
                                 (val (if prev (push args prev) (list args))))
                            (puthash fun val wal/mock-history)
                            args)))
              ,@(mapcar (lambda (it)
                          (cond
                           ((consp it)
                            `((symbol-function ',(car it))
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',(car it) r))
                                (apply ,(cdr it) r))))
                           (t
                            `((symbol-function ',it)
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',it r)))))))
                        (if (listp to-mock) to-mock (list to-mock))))
     ,@body))

(defun wal/clear-mocks ()
  "Clear mock history."
  (setq wal/mock-history (make-hash-table :test 'equal)))

(defmacro was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun wal/mock-history))))))

(defmacro was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun wal/mock-history)))))))

(defmacro was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun wal/mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun wal/mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(should (equal ,expected (length (gethash ',fun wal/mock-history)))))

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

         (unwind-protect
             (progn ,@body)
           (when (get-buffer ,filename)
             (kill-buffer ,filename)
             (message "Killed buffer '%s'." ,filename))

           (delete-file ,tmp-file)
           (message "Deleted file '%s'." ,tmp-file))))))

(when (wal/cold-start-p)
  (message "Stumping `use-package'.")

  (defmacro use-package (package-name &rest _args)
    "Message that PACKAGE-NAME would have been loaded."
    `(message "Would have loaded %s" ',package-name))

  (defvar wal/emacs-config-default-path)
  (defvar wal/emacs-config-package-path)

  (let* ((source-dir (or (getenv "GITHUB_WORKSPACE") default-directory))
         (package-dir (expand-file-name "wal" source-dir)))

    (message "Setting source dir to %s, package dir to %s" source-dir package-dir)

    (when (getenv "CI")
      (add-to-list 'load-path package-dir))
    (setq wal/emacs-config-default-path source-dir
          wal/emacs-config-package-path package-dir)))

;;; test-helper.el ends here
