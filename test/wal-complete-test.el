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

(ert-deftest wal-vertico-quick-exit-in-case-of-single-match ()
  :tags '(complete)

  (defvar vertico--total)

  (let ((vertico--total 0))
    (bydi (vertico-exit)

     (should-not (wal-vertico-quick-exit-in-case-of-single-match))

     (bydi-was-not-called vertico-exit)

     (setq vertico--total 2)

     (should-not (wal-vertico-quick-exit-in-case-of-single-match))

     (bydi-was-not-called vertico-exit)

     (setq vertico--total 1)

     (wal-vertico-quick-exit-in-case-of-single-match)

     (bydi-was-called vertico-exit))))

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

(ert-deftest wal-consult-compilation-buffer--query ()
  :tags '(emacs)

  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-compilation-buffer--query) '(visibility buffer-name wal-compilation-buffer-p))))

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
      (bydi-was-called consult-outline t)

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

    (wal-cape-setup)

    (bydi-was-set completion-at-point-functions t)

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

(ert-deftest wal-tempel-setup ()
  (bydi ((:watch completion-at-point-functions))

    (wal-tempel-setup)

    (bydi-was-set completion-at-point-functions)))

;;; wal-complete-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
