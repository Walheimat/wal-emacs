;;; wal-key-bindings-test.el --- Tests for key-bindings package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test macros and helpers.

;;; Code:

(require 'wal-key-bindings nil t)

(ert-deftest test-wal/create-leader-sink ()
  (with-mock general-define-key

    (match-expansion
     (wal/create-leader-sink wal/tester-sink :definer wal/tester :prefix "C-t")
     '(defmacro wal/tester-sink
          (&rest args)
        `(,'wal/tester ,@(mapcar (lambda (it)
                                   (if (stringp it)
                                       (concat "t" it)
                                     it))
                                 args))))

    (was-called-with general-define-key
                     (list :prefix "C-t" "t" (list :ignore t :wk "TESTER!")))))

(ert-deftest test-wal/lieutenant! ()
  (match-expansion
   (wal/lieutenant! "t" #'ignore #'always)
   '(progn
      (wal/lieutenant "t" #'ignore)
      (wal/lieutenant-sink "t" #'always))))

(ert-deftest test-wal/key-by-leader ()
  (defvar wal/key-reach)
  (defvar wal/leaders)
  (let ((wal/key-reach '("a" "b" "c"))
        (wal/leaders '(hunk bunk trunk)))

    (should (string-equal "c" (wal/key-by-leader 'trunk)))))

(ert-deftest test-wal/key-combo-for-leader ()
  (defvar wal/key-reach)
  (defvar wal/leaders)
  (with-mock ((wal/prefix-user-key . (lambda (x) (concat "C-t " x))))

    (let ((wal/key-reach '("a" "b" "c"))
          (wal/leaders '(hunk bunk trunk)))

      (should (string-equal "C-t a k" (wal/key-combo-for-leader 'hunk :key "k")))
      (should (string-equal "C-t b b k" (wal/key-combo-for-leader 'bunk :key "k" :in-sink t)))
      (should (string-equal "C-t c" (wal/key-combo-for-leader 'trunk)))
      (should (equal (kbd "C-t b") (wal/key-combo-for-leader 'bunk :translate t))))))

(ert-deftest test-wal/general-create-definer ()
  (with-mock ((wal/key-combo-for-leader . (lambda (_) "C-t"))
              (eval-after-load . #'ignore)
              eval)

    (wal/general-create-definer 'wal/tester)

    (was-called-nth-with eval (list '(general-create-definer wal/tester :prefix "C-t")) 0)
    (was-called-nth-with eval (list '(wal/create-leader-sink wal/tester-sink :definer wal/tester :prefix "C-t")) 1)))

(ert-deftest test-wal/captain? ()
  (with-mock (message (wal/key-combo-for-leader . (lambda (&rest _) "C-t")))

    (with-temp-buffer
      (text-mode)

      (wal/captain?)

      (was-called-with message (list "Captain (%s) has no binding in %s" "C-t" "text-mode")))))

(ert-deftest test-wal/transient-grab ()
  (defvar transient-current-command)
  (let ((transient-current-command "test"))
    (with-mock((transient-arg-value . #'concat)
               (transient-args . #'wal/rf))

              (should (string-equal "--testing=test" (wal/transient-grab "testing"))))))

;;; wal-key-bindings-test.el ends here
