;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest test-wal/corfu-enable-in-minibuffer ()
  (with-mock ((where-is-internal . #'always) corfu-mode)

    (wal/corfu-enable-in-minibuffer)

    (was-called-with corfu-mode 1)))

(ert-deftest test-wal/record-this-command ()
  (let ((this-command 'testing))
    (with-temp-buffer

      (wal/record-this-command)

      (should (equal 'testing wal/command)))))

(ert-deftest test-wal/with-dired-goto-file-ignored ()
  (let* ((wal/command 'dired-goto-file)
         (fun (lambda (_) 'test))
         (result (apply 'wal/with-dired-goto-file-ignored (list fun 'category))))

    (should-not result)))

(ert-deftest test-wal/with-dired-goto-file-ignored--ignores ()
  (let* ((wal/command 'dired-goto-file-1)
         (fun (lambda (_) 'test))
         (result (apply 'wal/with-dired-goto-file-ignored (list fun 'category))))

    (should (equal 'test result))))

(ert-deftest test-wal/consult-ripgrep-ignored ()
  (with-mock (consult--grep consult--ripgrep-builder)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args "test"))

      (wal/consult-ripgrep-ignored)

      (was-called-with consult--grep (list "Ripgrep (ignored)" #'consult--ripgrep-builder nil nil)))))

(ert-deftest test-wal/consult-unregister ()
  (with-mock ((consult--read . (lambda (&rest _r) ?r))
              consult-register--candidates
              consult--type-group
              consult--type-narrow
              consult--lookup-candidate)

    (defvar consult-register--narrow)
    (let ((register-alist '((?t . test) (?r . remove)))
          (consult-register--narrow nil))

      (wal/consult-unregister)

      (should (equal register-alist '((?t . test)))))))

(ert-deftest test-wal/consult-line ()
  (with-mock consult-line

    (wal/consult-line t)

    (was-called consult-line)

    (with-temp-buffer
      (insert "hello")
      (goto-char 1)

      (wal/consult-line)

      (was-called-with consult-line "hello"))))

(ert-deftest test-wal/consult-clock-in ()
  (with-mock (consult-org-agenda org-clock-in)

    (wal/consult-clock-in)

    (was-called consult-org-agenda)
    (was-called org-clock-in)))

(ert-deftest test-wal/then-set-active-theme ()
  (let ((out nil)
        (wal/active-theme nil))
    (with-temp-buffer
      (add-hook 'wal/theme-hook (lambda () (add-to-list 'out 'hook)))
      (wal/then-set-active-theme 'test))

    (should (equal out '(hook)))
    (should (equal wal/active-theme 'test))))

(ert-deftest test-wal/with-big-vertico ()
  (defvar vertico-count 10)
  (let ((fun (lambda () vertico-count)))

    (should (equal 20 (wal/with-big-vertico fun)))))

;;; wal-complete-test.el ends here
