;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest test-wal-corfu-enable-in-minibuffer ()
  (bydi-with-mock ((where-is-internal . #'always) corfu-mode)

    (wal-corfu-enable-in-minibuffer)

    (bydi-was-called-with corfu-mode 1)))

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
  (bydi-with-mock (browse-url)
    (should-error (wal-browse-html-file "~/test/file.txt") :type 'user-error)

    (wal-browse-html-file "/tmp/test.html")
    (bydi-was-called-with browse-url "/tmp/test.html")))

(ert-deftest test-wal-consult-ripgrep-ignored ()
  (bydi-with-mock (consult--grep consult--ripgrep-builder)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args "test"))

      (wal-consult-ripgrep-ignored)

      (bydi-was-called-with consult--grep (list "Ripgrep (ignored)" #'consult--ripgrep-builder nil nil)))))

(ert-deftest test-wal-consult-unregister ()
  (bydi-with-mock ((consult--read . (lambda (&rest _r) ?r))
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
  (bydi-with-mock consult-line

    (wal-consult-line)

    (bydi-was-called consult-line)

    (with-temp-buffer
      (insert "hello")
      (goto-char 1)

      (wal-consult-line t)

      (bydi-was-called-with consult-line "hello"))))

(ert-deftest test-wal-consult-clock-in ()
  (bydi-with-mock (consult-org-agenda org-clock-in wal-org-clock-in-from-now)

    (wal-consult-clock-in)

    (bydi-was-called consult-org-agenda)
    (bydi-was-called org-clock-in)

    (funcall-interactively 'wal-consult-clock-in '(4))

    (bydi-was-called consult-org-agenda)
    (bydi-was-called wal-org-clock-in-from-now)))

(ert-deftest test-wal-then-set-active-theme ()
  (defvar wal-active-theme nil)
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
  (defvar wal-consult-buffer-narrow-source)
  (defvar wal-consult-pre-narrowed-commands)

  (let ((wal-consult-buffer-narrow-source 'project)
        (wal-consult-pre-narrowed-commands '(consult-buffer wal-consult-project)))

    (bydi-with-mock ((consult--project-root . #'always)
                     (consult--buffer-query . #'always)
                     (consult--open-project-items . #'always)
                     (partial-recall--has-buffers-p . #'always))

      (setq wal-consult-pre-narrow t
            unread-command-events nil)

      (wal-consult--pre-narrow)

      (should-not unread-command-events)

      (setq this-command 'consult-buffer)

      (wal-consult--pre-narrow)

      (should unread-command-events)
      (bydi-was-called consult--project-root)

      (bydi-clear-mocks)

      (setq wal-consult-buffer-narrow-source 'recall)

      (wal-consult--pre-narrow)

      (should unread-command-events)
      (bydi-was-not-called consult--project-root)
      (bydi-was-called partial-recall--has-buffers-p)

      (setq unread-command-events nil
            this-command 'wal-consult-project)

      (wal-consult--pre-narrow)

      (should unread-command-events)

      (setq wal-consult-pre-narrow nil
            unread-command-events nil)

      (wal-consult--pre-narrow)

      (should-not unread-command-events))))

(ert-deftest test-wal-consult-toggle-pre-narrow ()
  (defvar wal-consult-pre-narrow)
  (let ((wal-consult-pre-narrow nil))

    (wal-consult-toggle-pre-narrowing)
    (should wal-consult-pre-narrow)))

(ert-deftest test-consult--open-project-items ()
  (bydi-with-mock ((buffer-list . (lambda () '("a" "c" "b" "c" "a")))
                   (wal-project--buffer-root . (lambda (b) b)))

    (should (equal '("b" "c" "a") (consult--open-project-items)))))

(ert-deftest test-wal-consult-project ()
  (bydi-with-mock (consult--multi)

    (call-interactively 'wal-consult-project)

    (bydi-was-called-with consult--multi (list '(consult--source-open-projects consult--source-projects) :prompt "Select project: "))))

(ert-deftest test-wal-adjust-by-putting-current-buffer-first ()
  (bydi-with-mock ((current-buffer . (lambda () 'current)))

    (let ((buffers '(one two current)))

      (should (equal '(current one two) (wal-adjust-by-putting-current-buffer-first buffers)))

      (setq buffers '(one two three))

      (should (equal '(one two three) (wal-adjust-by-putting-current-buffer-first buffers))))))

(ert-deftest test-wal-consult-org-heading ()
  (bydi-with-mock (consult-org-heading org-up-heading-safe)

    (funcall-interactively 'wal-consult-org-heading t)

    (bydi-was-called org-up-heading-safe)
    (bydi-was-called-with consult-org-heading (list nil 'tree))

    (bydi-clear-mocks)

    (funcall-interactively 'wal-consult-org-heading)

    (bydi-was-called-with consult-org-heading nil)))

;;; wal-complete-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
