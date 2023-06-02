;;; wal-pacify-test.el --- Test pacify package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests byte-compiler checker. This file makes sure that actual
;; `flycheck' is not loaded and `use-package' forms are stumped.

;;; Code:

(provide 'flymake)

(require 'wal-pacify nil t)

(defvar flymake--state nil)

(defmacro with-flymake-state (&rest body)
  "Execute BODY with `flymake' state."
  (declare (indent 0))

  `(let ((flymake--state ,(make-hash-table)))

     (puthash 'elisp-flymake-byte-compile 'byte flymake--state)
     (puthash 'elisp-flymake-checkdoc 'docs flymake--state)

     ,@body))

(ert-deftest wp-get-state ()
  (with-flymake-state
   (should-not (wal-pacify--get-state 'elisp-flymake-non-existence))
   (should (wal-pacify--get-state 'elisp-flymake-byte-compile))))

(ert-deftest wp--ready-p ()
  (with-flymake-state
   (bydi-with-mock (flymake--state-reported-p)
     (should (wal-pacify--ready-p)))))

(ert-deftest wp--get-diags--appends ()
  (bydi-with-mock ((flymake--state-diags . (lambda (_) '(test)))
                   wal-pacify--get-state)
    (should (equal '(test test) (wal-pacify--get-diags)))))

(ert-deftest wp--get-severity-diags ()
  (bydi-with-mock ((wal-pacify--get-diags . (lambda () '(1 2 3 4 3)))
                   (wal-pacify--get-info . #'bydi-rf)
                   (flymake--severity . #'bydi-rf)
                   (flymake-diagnostic-type . #'bydi-rf))
    (should (equal '(3 3) (wal-pacify--get-severity-diags 3)))))

(ert-deftest wp--get-info ()
  (bydi-with-temp-file "testing"

    (bydi-with-mock ((flymake--diag-text . (lambda (_) "test"))
                     (flymake--diag-beg . (lambda (_) 1))
                     (flymake--diag-locus . (lambda (_) (get-buffer bydi-tmp-file))))

      (should (equal (wal-pacify--get-info nil)
                     (list :file (buffer-file-name (get-buffer bydi-tmp-file))
                           :line 1
                           :text "test"))))))

(ert-deftest wp--collect--collects-if-ready ()
  (let ((severities '(warning error debug info)))
    (bydi-with-temp-file "testing"
      (bydi-with-mock (flymake-mode
                       flymake-start
                       (wal-pacify--ready-p . #'always)
                       (wal-pacify--get-severity-diags . (lambda (_) (pop severities))))

        (wal-pacify--collect bydi-tmp-file)

        (bydi-was-called-n-times wal-pacify--get-severity-diags 4)))))

(ert-deftest wp--collect--errors-if-never-ready ()
  (bydi-with-temp-file "testing"
    (bydi-with-mock (flymake-mode
                     flymake-start
                     (wal-pacify--ready-p . #'ignore)
                     sit-for)

      (should-error (wal-pacify--collect bydi-tmp-file) :type 'error))))

(ert-deftest wp--format ()
  (let ((info '(:file "test.txt" :line 42 :text "Answer revealed")))

    (should (string= "test.txt:42: Answer revealed" (wal-pacify--format info)))))

(ert-deftest wp-check--get-package-files ()
  (defvar wal-pacify-check--not-testable)

  (let ((wal-pacify-check--not-testable "notest\\|leaveme"))

    (bydi-with-mock ((wal-prelude-package-files . (lambda () '("wal-notest.el"
                                                          "wal-dotest.el"
                                                          "wal-do-leaveme.el"
                                                          "wal-useme.el"))))

      (should (equal '("wal-dotest.el" "wal-useme.el") (wal-pacify-check--get-package-files))))))

(defvar wal-pacify--errors)
(defvar wal-pacify--warnings)
(defvar wal-pacify--debugs)
(defvar wal-pacify--infos)

(ert-deftest wp-check--exits-with-0-on-check-error ()
  (let ((wal-pacify--errors nil)
        (wal-pacify--warnings nil)
        (wal-pacify--debugs nil)
        (wal-pacify--infos nil))

    (bydi-with-mock ((wal-pacify-check--get-package-files . (lambda () '(one two three)))
                     (wal-pacify--collect . (lambda (_) "" (error "Oops")))
                     wal-pacif--format
                     kill-emacs)

      (wal-pacify-check)

      (bydi-was-called-with kill-emacs 0))))

(ert-deftest wp-check--prints-infos-and-debugs-without-exit ()
  (let ((wal-pacify--errors nil)
        (wal-pacify--warnings nil)
        (wal-pacify--debugs '(one two))
        (wal-pacify--infos '(three four)))

    (bydi-with-mock ((wal-pacify-check--get-package-files . (lambda () '(one two three)))
                     wal-pacify--collect
                     wal-pacify--format
                     message
                     kill-emacs)

      (wal-pacify-check)

      (bydi-was-not-called kill-emacs)

      (bydi-was-called-n-times wal-pacify--format 4))))

(ert-deftest wp-check--exits-with-0-on-severe-errors ()
  (let ((wal-pacify--errors '(one two))
        (wal-pacify--warnings '(three four))
        (wal-pacify--debugs nil)
        (wal-pacify--infos nil))

    (bydi-with-mock ((wal-pacify-check--get-package-files . (lambda () '(one two three)))
                     wal-pacify--collect
                     wal-pacify--format
                     message
                     kill-emacs)

      (wal-pacify-check)

      (bydi-was-called-with kill-emacs 1)
      (bydi-was-called-n-times wal-pacify--format 4))))

;;; wal-pacify-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
