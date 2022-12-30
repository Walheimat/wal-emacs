;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest test-wal/corfu-enable-in-minibuffer ()
  (with-mock-all ((where-is-internal . (lambda (_ &optional _ _ _) t))
                  (corfu-mode . (lambda (&optional _) 'corfu)))
    (should (equal (wal/corfu-enable-in-minibuffer) 'corfu))))

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
  (with-mock-all ((consult--grep . (lambda (m &rest _r) m))
                  (consult--ripgrep-builder . #'ignore))
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args "test"))
      (should (string-equal "Ripgrep (ignored)" (wal/consult-ripgrep-ignored))))))

(ert-deftest test-wal/consult-unregister ()
  (with-mock-all ((consult--read . (lambda (&rest _r) ?r))
                  (consult-register--candidates . #'ignore)
                  (consult--type-group . #'ignore)
                  (consult--type-narrow . #'ignore)
                  (consult--lookup-candidate . #'ignore))
    (defvar consult-register--narrow)
    (let ((register-alist '((?t . test) (?r . remove)))
          (consult-register--narrow nil))
      (wal/consult-unregister)
      (should (equal register-alist '((?t . test)))))))

(ert-deftest test-wal/consult-line ()
  (with-mock consult-line (lambda (&optional i _) i)
    (should-not (wal/consult-line t))
    (with-temp-buffer
      (insert "hello")
      (goto-char 1)
      (should (string-equal (wal/consult-line) "hello")))))

(ert-deftest test-wal/consult-clock-in ()
  (let ((out nil))
    (with-mock-all ((consult-org-agenda . (lambda (&rest _) (add-to-list 'out 'consult)))
                    (org-clock-in . (lambda (&rest _) (add-to-list 'out 'org))))
      (wal/consult-clock-in)
      (should (equal out '(org consult))))))

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
