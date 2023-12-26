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

  (bydi (consult-org-agenda
         org-clock-in
         wal-org-clock-in-from-now
         (:spy save-buffer)
         (:mock org-clocking-buffer :return (current-buffer)))

    (wal-consult-clock-in)
    (bydi-was-called-with consult-org-agenda "-ARCHIVE/-DONE")
    (bydi-was-called org-clock-in)
    (bydi-was-called org-clocking-buffer)
    (bydi-was-called save-buffer)

    (funcall-interactively 'wal-consult-clock-in '(4))
    (bydi-was-called consult-org-agenda)
    (bydi-was-called wal-org-clock-in-from-now)))

(ert-deftest wal-then-set-active-theme ()
  :tags '(complete)

  (defvar wal-active-theme nil)

  (bydi (customize-save-variable
         (:spy run-hooks)
         (:watch wal-active-theme))

    (wal-then-set-active-theme 'some)

    (bydi-was-set-to wal-active-theme 'some)
    (bydi-was-not-called customize-save-variable)
    (bydi-was-called-with run-hooks 'wal-theme-hook)


    (let ((current-prefix-arg t))

      (wal-then-set-active-theme 'some)

      (bydi-was-called customize-save-variable))))

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
