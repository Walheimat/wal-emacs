;;; wal-key-bindings-test.el --- Tests for key-bindings package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test macros and helpers.

;;; Code:

(require 'wal-key-bindings nil t)

(ert-deftest test-wal/create-leader-sink ()
  (let ((out nil))
    (with-mock general-define-key (lambda (&rest r) (setq out r))
      (match-expansion
       (wal/create-leader-sink wal/tester-sink :definer wal/tester :prefix "C-t")
       '(defmacro wal/tester-sink
            (&rest args)
          `(,'wal/tester ,@(mapcar (lambda (it)
                                     (if (stringp it)
                                         (concat "t" it)
                                       it))
                                   args))))
      (should (equal (list :prefix "C-t" "t" (list :ignore t :wk "TESTER!")) out)))))

(ert-deftest test-wal/lieutenant! ()
  (match-expansion
   (wal/lieutenant! "t" #'ignore #'always)
   '(progn
      (wal/lieutenant "t" #'ignore)
      (wal/lieutenant-sink "t" #'always))))

(ert-deftest test-wal/key-by-leader ()
  (defvar wal/key-reach)
  (let ((wal/key-reach '("a" "b" "c")))
    (should (string-equal "c" (wal/key-by-leader 'wal/major)))))

(ert-deftest test-wal/key-combo-for-leader ()
  (defvar wal/key-reach)
  (with-mock wal/prefix-user-key (lambda (x) (concat "C-t " x))
    (let ((wal/key-reach '("a" "b" "c")))
      (should (string-equal "C-t a k" (wal/key-combo-for-leader 'wal/lieutenant "k")))
      (should (string-equal "C-t a a k" (wal/key-combo-for-leader 'wal/lieutenant "k" t)))
      (should (string-equal "C-t a" (wal/key-combo-for-leader 'wal/lieutenant))))))

(ert-deftest test-wal/general-create-definer ()
  (let ((out nil))
    (with-mock-all ((wal/key-combo-for-leader . (lambda (_) "C-t"))
                    (eval-after-load . #'ignore)
                    (eval . (lambda (x) (push x out))))
      (wal/general-create-definer 'wal/tester)
      (should (equal '((wal/create-leader-sink wal/tester-sink :definer wal/tester :prefix "C-t") (general-create-definer wal/tester :prefix "C-t")) out)))))

(ert-deftest test-wal/transient-grab ()
  (defvar transient-current-command)
  (let ((transient-current-command "test"))
    (with-mock-all ((transient-arg-value . #'concat)
                    (transient-args . #'wal/rf))
      (should (string-equal "--testing=test" (wal/transient-grab "testing"))))))

;;; wal-key-bindings-test.el ends here
