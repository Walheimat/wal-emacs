;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest test-wal-corfu-enable-in-minibuffer ()
  (with-mock ((where-is-internal . #'always) corfu-mode)

    (wal-corfu-enable-in-minibuffer)

    (was-called-with corfu-mode 1)))

(ert-deftest test-wal-corfu-auto--sets-corfu ()
  (defvar corfu-auto-delay nil)
  (defvar corfu-auto-prefix nil)

  (with-temp-buffer
    (wal-corfu-auto (list 2.1 4))

    (should (equal 2.1 corfu-auto-delay))
    (should (equal 4 corfu-auto-prefix))))

(ert-deftest test-wal-record-this-command ()
  (let ((this-command 'testing))
    (with-temp-buffer

      (wal-record-this-command)

      (should (equal 'testing wal-command)))))

(ert-deftest test-wal-with-dired-goto-file-ignored ()
  (let* ((wal-command 'dired-goto-file)
         (fun (lambda (_) 'test))
         (result (apply 'wal-with-dired-goto-file-ignored (list fun 'category))))

    (should-not result)))

(ert-deftest test-wal-with-dired-goto-file-ignored--ignores ()
  (let* ((wal-command 'dired-goto-file-1)
         (fun (lambda (_) 'test))
         (result (apply 'wal-with-dired-goto-file-ignored (list fun 'category))))

    (should (equal 'test result))))

(ert-deftest test-wal-browse-html-file ()
  (with-mock (browse-url)
    (should-error (wal-browse-html-file "~/test/file.txt") :type 'user-error)

    (wal-browse-html-file "/tmp/test.html")
    (was-called-with browse-url "/tmp/test.html")))

(ert-deftest test-wal-consult-ripgrep-ignored ()
  (with-mock (consult--grep consult--ripgrep-builder)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args "test"))

      (wal-consult-ripgrep-ignored)

      (was-called-with consult--grep (list "Ripgrep (ignored)" #'consult--ripgrep-builder nil nil)))))

(ert-deftest test-wal-consult-unregister ()
  (with-mock ((consult--read . (lambda (&rest _r) ?r))
              consult-register--candidates
              consult--type-group
              consult--type-narrow
              consult--lookup-candidate)

    (defvar consult-register--narrow)
    (let ((register-alist '((?t . test) (?r . remove)))
          (consult-register--narrow nil))

      (wal-consult-unregister)

      (should (equal register-alist '((?t . test)))))))

(ert-deftest test-wal-consult-line ()
  (with-mock consult-line

    (wal-consult-line)

    (was-called consult-line)

    (with-temp-buffer
      (insert "hello")
      (goto-char 1)

      (wal-consult-line t)

      (was-called-with consult-line "hello"))))

(ert-deftest test-wal-consult-clock-in ()
  (with-mock (consult-org-agenda org-clock-in wal-org-clock-in-from-now)

    (wal-consult-clock-in)

    (was-called consult-org-agenda)
    (was-called org-clock-in)

    (funcall-interactively 'wal-consult-clock-in '(4))

    (was-called consult-org-agenda)
    (was-called wal-org-clock-in-from-now)))

(ert-deftest test-wal-then-set-active-theme ()
  (let ((out nil)
        (wal-active-theme nil))
    (with-temp-buffer
      (add-hook 'wal-theme-hook (lambda () (add-to-list 'out 'hook)))
      (wal-then-set-active-theme 'test))

    (should (equal out '(hook)))
    (should (equal wal-active-theme 'test))))

(ert-deftest test-wal-with-big-vertico ()
  (defvar vertico-count 10)
  (let ((fun (lambda () vertico-count)))

    (should (equal 20 (wal-with-big-vertico fun)))))

(ert-deftest test-wal-consult--pre-narrow ()
    (defvar wal-consult-pre-narrow)
    (with-mock ((consult--project-root . #'always)
                (consult--buffer-query . #'always)
                (consult--open-project-items . #'always))

      (setq wal-consult-pre-narrow t
            unread-command-events nil)

      (wal-consult--pre-narrow)

      (should-not unread-command-events)

      (setq this-command 'consult-buffer)

      (wal-consult--pre-narrow)

      (should unread-command-events)

      (setq unread-command-events nil
            this-command 'wal-consult-project)

      (wal-consult--pre-narrow)

      (should unread-command-events)

      (setq wal-consult-pre-narrow nil
            unread-command-events nil)

      (wal-consult--pre-narrow)

      (should-not unread-command-events)))

(ert-deftest test-wal-consult-toggle-pre-narrow ()
  (defvar wal-consult-pre-narrow)
  (let ((wal-consult-pre-narrow nil))

    (wal-consult-toggle-pre-narrowing)
    (should wal-consult-pre-narrow)))

(ert-deftest test-consult--open-project-items ()
  (with-mock ((buffer-list . (lambda () '("a" "c" "b" "c" "a")))
              (wal-project--buffer-root . (lambda (b) b)))

    (should (equal '("b" "c" "a") (consult--open-project-items)))))

(ert-deftest test-wal-consult-project ()
  (with-mock (consult--multi)

    (call-interactively 'wal-consult-project)

    (was-called-with consult--multi (list '(consult--source-open-projects consult--source-projects) :prompt "Select project: "))))

(ert-deftest test-wal-consult-org-heading ()
  (with-mock (consult-org-heading org-up-heading-safe)

    (funcall-interactively 'wal-consult-org-heading t)

    (was-called org-up-heading-safe)
    (was-called-with consult-org-heading (list nil 'tree))

    (wal-clear-mocks)

    (funcall-interactively 'wal-consult-org-heading)

    (was-called-with consult-org-heading nil)))

;;; wal-complete-test.el ends here
