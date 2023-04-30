;;; wal-key-bindings-test.el --- Tests for key-bindings package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test macros and helpers.

;;; Code:

(require 'wal-key-bindings nil t)

(ert-deftest test-wal-create-leader-sink ()
  (with-mock general-define-key

    (match-expansion
     (wal-create-leader-sink tester-sink :definer tester :prefix "C-t")
     '(defmacro tester-sink
          (&rest args)
        `(,'tester ,@(mapcar (lambda (it)
                                   (if (stringp it)
                                       (concat "t" it)
                                     it))
                                 args))))

    (was-called-with general-define-key
                     (list :prefix "C-t" "t" (list :ignore t :wk "TESTER!")))))

(ert-deftest test-editors ()
  (match-expansion
   (editors "t" #'ignore #'always)
   '(progn
      (editor "t" #'ignore)
      (editor-sink "t" #'always))))

(ert-deftest test-wal-key-by-leader ()
  (defvar wal-key-reach)
  (defvar wal-leaders)
  (let ((wal-leaders '(("a" . hunk) ("b" . bunk) ("c" . trunk))))

    (should (string-equal "c" (wal-key-by-leader 'trunk)))))

(ert-deftest test-wal-key-combo-for-leader ()
  (defvar wal-key-reach)
  (defvar wal-leaders)
  (with-mock ((wal-prefix-user-key . (lambda (x) (concat "C-t " x))))

    (let ((wal-leaders '(("a" . hunk) ("b" . bunk) ("c" . trunk))))

      (should (string-equal "C-t a k" (wal-key-combo-for-leader 'hunk :key "k")))
      (should (string-equal "C-t b b k" (wal-key-combo-for-leader 'bunk :key "k" :in-sink t)))
      (should (string-equal "C-t c" (wal-key-combo-for-leader 'trunk)))
      (should (equal (kbd "C-t b") (wal-key-combo-for-leader 'bunk :translate t))))))

(ert-deftest test-wal-general-create-definer ()
  (with-mock ((wal-key-combo-for-leader . (lambda (_) "C-t"))
              (eval-after-load . #'ignore)
              eval)

    (wal-general-create-definer 'tester)

    (was-called-nth-with eval (list '(general-create-definer tester :prefix "C-t")) 0)
    (was-called-nth-with eval (list '(wal-create-leader-sink tester-sink :definer tester :prefix "C-t")) 1)))

(ert-deftest test-major? ()
  (with-mock (message (wal-key-combo-for-leader . (lambda (&rest _) "C-t")))

    (with-temp-buffer
      (text-mode)

      (major?)

      (was-called-with message (list "Major (%s) has no binding in %s" "C-t" "text-mode")))))

(ert-deftest test-wal-transient-grab ()
  (defvar transient-current-command)
  (let ((transient-current-command "test"))
    (with-mock((transient-arg-value . #'concat)
               (transient-args . #'wal-rf))

              (should (string-equal "--testing=test" (wal-transient-grab "testing"))))))

(ert-deftest test-wal-transient-command-or-major ()
  (with-temp-buffer
    (setq-local mode-line-buffer-identification "cool-major")

    (should (string= "major" (wal-transient-command-or-major)))

    (setq-local mode-line-buffer-identification "another-transient")

    (should-not (string= "major" (wal-transient-command-or-major)))))

(ert-deftest test-wal-with-delayed-transient-popup ()
  (defvar transient-show-popup)
  (let ((transient-show-popup 0.4))

    (should (equal 0.8 (wal-with-delayed-transient-popup (lambda () transient-show-popup))))))

(ert-deftest test-that-key ()
  (with-mock ((wal-prefix-user-key . (lambda (k) (concat "H-" k)))
              (wal-key-combo-for-leader . (lambda (_l _k k) (concat "M-" k))))

    (match-expansion
     (that-key "Help me!" :key "h")
     `(with-eval-after-load 'which-key
        (which-key-add-key-based-replacements "h" "Help me!")))

    (match-expansion
     (that-key "Help me!" :user-key "h")
     `(with-eval-after-load 'which-key
        (which-key-add-key-based-replacements "H-h" "Help me!")))

    (match-expansion
     (that-key "Help me!" :leader (tester :key "h"))
     `(with-eval-after-load 'which-key
        (which-key-add-key-based-replacements "M-h" "Help me!")))

    (match-expansion
     (that-key "Help me!" :key "h" :condition (display-graphic-p))
     `(with-eval-after-load 'which-key
        (when (display-graphic-p)
          (which-key-add-key-based-replacements "h" "Help me!"))))))

;;; wal-key-bindings-test.el ends here
