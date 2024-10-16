;;; wal-complete-test.el --- Tests for completion functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests all functions.

;;; Code:

(require 'wal-complete nil t)

(ert-deftest wal-corfu-enable-in-minibuffer ()
  :tags '(complete)

  (defvar corfu-auto)

  (bydi ((:mock where-is-internal :with always)
         (:watch corfu-auto)
         corfu-mode)
    (wal-corfu-enable-in-minibuffer)
    (bydi-was-called-with corfu-mode 1)
    (bydi-was-set corfu-auto)))

(ert-deftest wal-record-this-command ()
  :tags '(complete)

  (let ((this-command 'testing))

    (with-temp-buffer
      (wal-record-this-command)
      (should (equal 'testing wal-command)))))

(ert-deftest wal-vertico-and-corfu-quick-exit-in-case-of-single-match ()
  :tags '(complete)

  (defvar vertico--total)
  (defvar corfu--total)

  (let ((vertico--total 0)
        (corfu--total 0))
    (bydi (vertico-exit corfu-complete)

     (should-not (wal-vertico-quick-exit-in-case-of-single-match))
     (should-not (wal-corfu-quick-exit-in-case-of-single-match))

     (bydi-was-not-called vertico-exit)
     (bydi-was-not-called corfu-complete)

     (setq vertico--total 2
           corfu--total 2)

     (should-not (wal-vertico-quick-exit-in-case-of-single-match))
     (should-not (wal-corfu-quick-exit-in-case-of-single-match))

     (bydi-was-not-called vertico-exit)
     (bydi-was-not-called corfu-complete)

     (setq vertico--total 1
           corfu--total 1)

     (wal-vertico-quick-exit-in-case-of-single-match)
     (wal-corfu-quick-exit-in-case-of-single-match)

     (bydi-was-called vertico-exit)
     (bydi-was-called corfu-complete))))

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

(ert-deftest wal-consult-clock ()
  :tags '(complete)

  (bydi (consult-org-agenda
         org-clock-in
         org-clock-out
         (:always org-clocking-p)
         wal-org-clock-in-from-now
         (:spy save-buffer)
         (:mock org-clocking-buffer :return (current-buffer))
         (:always require))

    (wal-consult-clock)
    (bydi-was-called-with consult-org-agenda "-ARCHIVE/-DONE")
    (bydi-was-called org-clock-in)
    (bydi-was-called org-clocking-buffer)
    (bydi-was-called save-buffer)

    (funcall-interactively 'wal-consult-clock 0)
    (bydi-was-called consult-org-agenda)
    (bydi-was-called wal-org-clock-in-from-now)

    (funcall-interactively 'wal-consult-clock 4)
    (bydi-was-called org-clock-out)))

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

(ert-deftest wal-consult-narrow-for-agenda ()
  :tags '(complete)

  (ert-with-test-buffer (:name "consult narrow")

    (setq major-mode 'org-agenda-mode)

    (let ((this-command 'consult-buffer)
          (minibuffer--original-buffer (current-buffer)))

      (bydi ((:watch unread-command-events))

        (wal-consult-narrow-for-agenda)

        (bydi-was-set unread-command-events)))))

(ert-deftest consult--open-project-items ()
  :tags '(complete)

  (bydi ((:mock buffer-list :return '("a" "c" "b" "c" "a"))
         (:mock wal-project-buffer-root :with bydi-rf))
    (should (equal '("b" "c" "a") (consult--open-project-items)))))

(ert-deftest wal-consult-project ()
  :tags '(complete)

  (bydi (consult--multi)
    (call-interactively 'wal-consult-project)
    (bydi-was-called-with consult--multi (list '(consult--source-open-projects consult--source-projects) :prompt "Select project: " :require-match t))))

(ert-deftest wal-consult-agenda-buffer--query ()
  :tags '(complete org)

  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-agenda-buffer--query) '(visibility buffer-name wal-agenda-buffer-p))))

(ert-deftest wal-consult-display-buffer ()
  :tags '(complete)

  (defvar consult--buffer-display nil)
  (defvar consult-buffer-sources nil)

  (bydi ((:mock consult--multi :with (lambda (&rest _) (funcall consult--buffer-display 'cand)))
         display-buffer)

    (wal-consult-display-buffer)

    (bydi-was-called-with display-buffer '(... display-buffer-pop-up-window))))

(ert-deftest wal-consult-org-agenda-take-note ()
  :tags '(org user-facing)

  (defvar org-clock-current-task)
  (let ((org-clock-current-task nil))

    (bydi (consult-org-agenda
           org-clock-goto
           org-add-note)

      (wal-consult-org-agenda-take-note)

      (bydi-was-called consult-org-agenda :clear t)
      (bydi-was-called org-add-note)
      (bydi-was-not-called org-clock-goto)

      (funcall-interactively 'wal-consult-org-agenda-take-note t)

      (bydi-was-called consult-org-agenda :clear t)
      (bydi-was-called org-add-note)
      (bydi-was-not-called org-clock-goto)

      (setq org-clock-current-task "Test task")

      (wal-consult-org-agenda-take-note)

      (bydi-was-not-called consult-org-agenda)
      (bydi-was-called org-add-note)
      (bydi-was-called org-clock-goto :clear t)

      (funcall-interactively 'wal-consult-org-agenda-take-note t)

      (bydi-was-called consult-org-agenda)
      (bydi-was-called org-add-note)
      (bydi-was-not-called org-clock-goto))))

(ert-deftest wal-compilation-buffer-p ()
  :tags '(emacs)

  (with-temp-buffer
    (compilation-mode)

    (should (wal-compilation-buffer-p (current-buffer)))))

(ert-deftest wal-consult-compilation-buffer--query ()
  :tags '(emacs)

  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-compilation-buffer--query) '(visibility buffer-name wal-compilation-buffer-p))))

(ert-deftest wal-consult-line-symbol-at-point ()
  :tags '(complete)

  (bydi (consult-line
         (:mock thing-at-point :return 'thing))

    (wal-consult-line-symbol-at-point)

    (bydi-was-called-with consult-line 'thing)))

(ert-deftest wal-consult-place ()
  :tags '(complete)

  (let ((type 'org-mode))
    (bydi ((:mock derived-mode-p :with (lambda (x) (eq x type)))
           consult-org-heading
           consult-imenu
           consult-outline)

      (wal-consult-place)
      (bydi-was-called-with consult-org-heading "-ARCHIVE")

      (setq type 'text-mode)
      (wal-consult-place)
      (bydi-was-called consult-outline :clear t)

      (setq type 'prog-mode)
      (wal-consult-place)
      (bydi-was-called consult-imenu)

      (wal-consult-place t)
      (bydi-was-called consult-outline))))

(ert-deftest wal-consult-error ()
  :tags '(complete user-facing)

  (bydi (consult-flycheck
         consult-flymake
         (:spy fboundp))

    (bydi-when fboundp :called-with 'consult-flycheck :then-return t)

    (defvar flycheck-mode)

    (let ((flycheck-mode t))

      (wal-consult-error)

      (bydi-was-called consult-flycheck))

    (defvar flymake-mode)

    (let ((flymake-mode t))

      (wal-consult-error)

      (bydi-was-called consult-flymake))

    (should-error (wal-consult-error))))

(ert-deftest wal-cape-funs ()
  (bydi (cape-wrap-super
         (:watch completion-at-point-functions))

    (wal-cape-history-file)

    (bydi-was-called cape-wrap-super)

    (wal-cape-eshell-setup)

    (bydi-was-set completion-at-point-functions)))

(ert-deftest wal-tempel-comment ()
  :tags '(edit)

  (with-temp-buffer
    (setq major-mode 'emacs-lisp-mode)

    (should (string-equal (wal-tempel-comment (list 'c "testing")) ";; testing")))
  (with-temp-buffer
    (setq comment-start "// ")

    (should (string-equal (wal-tempel-comment (list 'c "testing")) "// testing"))))

(ert-deftest wal-vertico--no-cycle ()
  :tags '(complete)

  (defvar vertico-cycle nil)

  (bydi ((:watch vertico-cycle))

    (should (wal-vertico--no-cycle (lambda () (interactive) (always))))

    (bydi-was-set vertico-cycle)))

;;; wal-complete-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
