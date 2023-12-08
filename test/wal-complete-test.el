;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest wal-corfu-enable-in-minibuffer ()
  :tags '(complete)

  (bydi ((:mock where-is-internal :with always)
         corfu-mode)
    (wal-corfu-enable-in-minibuffer)
    (bydi-was-called-with corfu-mode 1)))

(ert-deftest wal-record-this-command ()
  :tags '(complete)

  (let ((this-command 'testing))

    (with-temp-buffer
      (wal-record-this-command)
      (should (equal 'testing wal-command)))))

(ert-deftest wal-with-dired-goto-file-ignored ()
  :tags '(complete)

  (let* ((wal-command 'dired-goto-file)
         (fun (lambda (_) 'test))
         (result (apply 'wal-with-dired-goto-file-ignored (list fun 'category))))

    (should-not result)))

(ert-deftest wal-with-dired-goto-file-ignored--ignores ()
  :tags '(complete)

  (let* ((wal-command 'dired-goto-file-1)
         (fun (lambda (_) 'test))
         (result (apply 'wal-with-dired-goto-file-ignored (list fun 'category))))

    (should (equal 'test result))))

(ert-deftest wal-browse-html-file ()
  :tags '(complete)

  (bydi (browse-url)
    (should-error (wal-browse-html-file "~/test/file.txt") :type 'user-error)
    (wal-browse-html-file "/tmp/test.html")
    (bydi-was-called-with browse-url "/tmp/test.html")))

(ert-deftest wal-consult-ripgrep-ignored ()
  :tags '(complete user-facing)

  (bydi (consult--grep consult--ripgrep-builder)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args "test"))

      (wal-consult-ripgrep-ignored)
      (bydi-was-called-with consult--grep (list "Ripgrep (ignored)" #'consult--ripgrep-builder nil nil)))))

(ert-deftest wal-consult-unregister ()
  :tags '(complete user-facing)

  (defvar consult-register--narrow)

  (let ((register-alist '((?t . test) (?r . remove)))
        (consult-register--narrow nil))

    (bydi ((:mock consult--read :with (lambda (&rest _r) ?r))
           consult-register--candidates
           consult--type-group
           consult--type-narrow
           consult--lookup-candidate)

      (wal-consult-unregister)
      (should (equal register-alist '((?t . test)))))))

(ert-deftest wal-consult-clock-in ()
  :tags '(complete)

  (bydi (consult-org-agenda org-clock-in wal-org-clock-in-from-now)
    (wal-consult-clock-in)
    (bydi-was-called consult-org-agenda)
    (bydi-was-called org-clock-in)

    (funcall-interactively 'wal-consult-clock-in '(4))
    (bydi-was-called consult-org-agenda)
    (bydi-was-called wal-org-clock-in-from-now)))

(ert-deftest wal-then-set-active-theme ()
  :tags '(complete)

  (defvar wal-active-theme nil)
  (let ((out nil)
        (wal-active-theme nil))

    (with-temp-buffer
      (add-hook 'wal-theme-hook (lambda () (add-to-list 'out 'hook)))
      (wal-then-set-active-theme 'test))

    (should (equal out '(hook)))
    (should (equal wal-active-theme 'test))))

(ert-deftest wal-consult--pre-narrow ()
  :tags '(complete)

  (defvar wal-consult-pre-narrow)
  (defvar wal-consult-buffer-narrow-source)
  (defvar wal-consult-pre-narrowed-commands)

  (let ((wal-consult-buffer-narrow-source 'project)
        (wal-consult-pre-narrowed-commands '(consult-buffer wal-consult-project)))

    (bydi ((:mock consult--project-root :with always)
           (:mock consult--buffer-query :with always)
           (:mock consult--open-project-items :with always)
           (:mock partial-recall--has-buffers-p :with always))
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

(ert-deftest wal-consult-toggle-pre-narrow ()
  :tags '(complete)

  (defvar wal-consult-pre-narrow)

  (let ((wal-consult-pre-narrow nil))

    (wal-consult-toggle-pre-narrowing)
    (should wal-consult-pre-narrow)))

(ert-deftest consult--open-project-items ()
  :tags '(complete)

  (bydi ((:mock buffer-list :return '("a" "c" "b" "c" "a"))
         (:mock wal-project--buffer-root :with bydi-rf))
    (should (equal '("b" "c" "a") (consult--open-project-items)))))

(ert-deftest wal-consult-project ()
  :tags '(complete)

  (bydi (consult--multi)
    (call-interactively 'wal-consult-project)
    (bydi-was-called-with consult--multi (list '(consult--source-open-projects consult--source-projects) :prompt "Select project: " :require-match t))))

(ert-deftest wal-consult-outline ()
  :tags '(complete)

  (bydi ((:sometimes derived-mode-p)
         consult-org-heading
         consult-outline)

    (wal-consult-outline)
    (bydi-was-called consult-org-heading)
    (bydi-toggle-sometimes)
    (wal-consult-outline)
    (bydi-was-called consult-outline)))

;;; wal-complete-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
