;;; wal-test.el --- Tests for main package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-config nil t)

(ert-deftest waw-animate ()
  (let ((wal-ascii-whale-key-frames ["testing" "resting"])
        (wal-ascii-whale-frame-index 0))

    (wal-ascii-whale-animate)

    (with-current-buffer (get-buffer-create wal-ascii-whale-buffer)
      (should (string-equal "testing" (buffer-string))))

    (should (eq 1 wal-ascii-whale-frame-index))))

(defmacro waw-with-animation (&rest body)
  "Execute BODY with animation mocked."
  (declare (indent 0))
  `(progn
     (defvar wal-config-ascii-whale)
     (let ((wal-ascii-whale-timer nil)
           (wal-ascii-whale-key-frames nil)
           (wal-config-ascii-whale nil)
           (wal-ascii-cachalot-whale-key-frames ["cachalot"])
           (wal-ascii-blue-whale-key-frames ["blue"]))

       (bydi (wal-ascii-whale-animate run-with-timer cancel-timer kill-buffer)
         ,@body))))

(ert-deftest waw--start-animation--no-op-for-timer ()
  (waw-with-animation
    (setq wal-ascii-whale-timer 'timer)

    (wal-ascii-whale--start-animation)

    (should-not wal-ascii-whale-key-frames)))

(ert-deftest waw--start-animation ()
  (waw-with-animation
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "blue"))))

(ert-deftest waw--start-animation--cachalot ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'cachalot)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "cachalot"))))

(ert-deftest waw--start-animation--blue ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'blue)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "blue"))))

(ert-deftest waw--stop-animation--no-op-for-no-timer ()
  (waw-with-animation
    (wal-ascii-whale--stop-animation)

    (should-not wal-ascii-whale-timer)))

(ert-deftest waw--stop-animation--no-op-if-buffers-exist ()
  (waw-with-animation
    (with-temp-buffer
      (setq wal-ascii-whale-timer 'timer)
      (setq wal-ascii-whale-parent-buffer (current-buffer))

      (wal-ascii-whale--stop-animation))

    (should wal-ascii-whale-timer)))

(ert-deftest waw--stop-animation ()
  (waw-with-animation
    (setq wal-ascii-whale-timer 'timer)

    (wal-ascii-whale--stop-animation)

    (should-not wal-ascii-whale-timer)

    (bydi-was-called cancel-timer)
    (bydi-was-called kill-buffer)))

(ert-deftest waw-setup ()
  (bydi (wal-ascii-whale--start-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (bydi-was-called wal-ascii-whale--start-animation)

      (should (buffer-local-value 'kill-buffer-hook (current-buffer)))
      (should (buffer-local-value 'window-configuration-change-hook (current-buffer))))))

(ert-deftest waw-clean-up ()
  (bydi (posframe-delete
         wal-ascii-whale--start-animation
         wal-ascii-whale--stop-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (wal-ascii-whale-clean-up)

      (bydi-was-called posframe-delete)
      (bydi-was-called wal-ascii-whale--stop-animation))))

(ert-deftest waw-poshandler ()
  (let ((result (wal-ascii-whale-poshandler `(:parent-window-left 4
                                                                  :parent-window-top 4
                                                                  :parent-window-width 8
                                                                  :posframe-width 2
                                                                  :parent-window ,(selected-window)))))

    (should (equal '(9 . 5) result))))

(ert-deftest waw-hidehandler ()
  (bydi ((:ignore get-buffer-window))

    (should (wal-ascii-whale-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest waw-display ()
  (let ((wal-ascii-whale-parent-buffer 'parent)
        (wal-ascii-whale-indirect-buffer 'indirect))

    (bydi (posframe-show
           (:mock face-attribute :return "#ffffff"))

      (wal-ascii-whale-display)

      (bydi-was-called-with posframe-show (list 'indirect
                                                :accept-focus nil
                                                :border-width 12
                                                :border-color "#ffffff"
                                                :poshandler 'wal-ascii-whale-poshandler
                                                :posframe-parent-buffer 'parent
                                                :hidehandler 'wal-ascii-whale-hidehandler)))))

(ert-deftest waw-toggle-display ()
  (bydi (wal-ascii-whale-clean-up
         wal-ascii-whale-setup
         wal-ascii-whale-display
         (:always require))

    (let ((wal-ascii-whale-parent-buffer t))

      (wal-ascii-whale-toggle-display)

      (bydi-was-called wal-ascii-whale-clean-up)
      (bydi-was-not-called wal-ascii-whale-setup))

    (bydi-clear-mocks)
    (setq wal-ascii-whale-parent-buffer nil)

    (wal-ascii-whale-toggle-display)

    (bydi-was-called wal-ascii-whale-setup)
    (bydi-was-called wal-ascii-whale-display)
    (bydi-was-not-called wal-ascii-whale-clean-up)))

(ert-deftest test-wal-describe-config-version ()
  (defvar wal-emacs-config-default-path)
  (let ((out '("1.0.0" "test everything" "1.0.1" "letting the world know"))
        (wal-emacs-config-default-path "~"))
    (bydi ((:mock shell-command-to-string :with (lambda (_) (pop out)))
           (:mock message :with bydi-rf))

      (should (equal "1.0.0: test everything" (wal-describe-config-version)))

      (let ((noninteractive nil))

        (should (equal "1.0.1: letting the world know" (wal-describe-config-version)))))))

(ert-deftest test-wal-show-config-diff-range ()
  (bydi ((:mock shell-command-to-string :return " testing ")
         magit-diff-range)

    (wal-show-config-diff-range)
    (bydi-was-called-with magit-diff-range (list "testing" '("--stat")))))

(ert-deftest test-wal-tangle-config-prompt ()
  (bydi ((:always wal-tangle-do-prompt)
         (:always y-or-n-p )
         wal-prelude-tangle-config)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)
      (bydi-was-called wal-prelude-tangle-config))))

(ert-deftest test-wal-tangle-config-prompt--after ()
  (bydi ((:always wal-tangle-do-prompt)
         (y-or-n-p . #'ignore)
         wal-prelude-tangle-config
         message)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)

      (bydi-was-called-with message "To tangle, call `wal-prelude-tangle-config'")
      (should-not wal-tangle-do-prompt)

      (bydi-clear-mocks)
      (wal-tangle-config-prompt)

      (bydi-was-called-with message "Config changed. To tangle, call `wal-prelude-tangle-config'"))))

(ert-deftest test-wal-config-switch-project ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/config"))

    (bydi (project-switch-project)

      (wal-config-switch-project)

      (bydi-was-called-with project-switch-project (list "/tmp/config")))))

(ert-deftest test-wal-config-consult-org-heading ()
  (defvar wal-emacs-config-lib-path)
  (let ((wal-emacs-config-lib-path nil))

    (bydi (consult-org-heading
           (:mock directory-files :return '("." ".." "/tmp/test.org" "/tmp/test-2.org")))

      (wal-config-consult-org-heading)

      (bydi-was-called-with consult-org-heading (list nil '("/tmp/test.org" "/tmp/test-2.org"))))))

(ert-deftest test-wal-customize-group ()
  (bydi customize-group

    (wal-customize-group)

    (bydi-was-called-with customize-group (list 'wal t))))

(ert-deftest test-wal-make--handlers ()
  (ert-with-message-capture messages
    (funcall (wal-make--on-success "cold-boot"))
    (funcall (wal-make--on-failure "cold-boot") "test")

    (should (string= messages "Script ’cold-boot’ succeeded.\nScript ’cold-boot’ failed.\n\ntest\n"))))

(ert-deftest test-wal-make ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/default"))

    (bydi (wal-async-process
           (:mock wal-make--on-success :return 'success)
           (:mock wal-make--on-failure :return 'failure))

      (wal-make "cold-boot" nil "cat out.txt")

      (bydi-was-called-with wal-async-process '("cd /tmp/default && make cold-boot && cat out.txt"
                                                success
                                                failure
                                                t)))))

(ert-deftest run-test--adds-post-command ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/default"))

    (bydi (wal-async-process
           (:mock wal-run-test--on-success :return 'success)
           (:mock wal-run-test--on-failure :return 'failure))

      (wal-run-test)

      (bydi-was-called-with wal-async-process '("cd /tmp/default && make test"
                                                success
                                                failure
                                                t)))))

(ert-deftest run-test--adds-pre-command-for-json ()
  (defvar wal-emacs-config-default-path)
  (defvar wal-run-test--coverage-format)
  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-run-test--coverage-format 'json))

    (bydi (wal-async-process
           (:mock wal-make--on-success :return 'success)
           (:mock wal-make--on-failure :return 'failure))

      (wal-run-test)

      (bydi-was-called-with wal-async-process '("export COVERAGE_WITH_JSON=true && cd /tmp/default && make test"
                                                success
                                                failure
                                                t)))))

(ert-deftest run-test-file--passes-file-as-args ()
  (defvar wal-emacs-config-default-path)
  (defvar wal-run-test--coverage-format)

  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-run-test--coverage-format 'text))

    (bydi (wal-run-test
           (:mock
            read-file-name
            :with
            (lambda (_p _d _d _m _i pred &rest _)
              (let ((selected "/tmp/tests/test.el"))
                (if (funcall pred selected)
                    selected
                  nil)))))

      (call-interactively 'wal-run-test-file)

      (bydi-was-called-with wal-run-test "test TEST_ARGS=/tmp/tests/test.el"))))

(ert-deftest run-test-toggle-format ()
  (defvar wal-run-test--coverage-format)

  (let ((wal-run-test--coverage-format 'text))

    (wal-run-test-toggle-format)

    (should (eq 'json wal-run-test--coverage-format))

    (wal-run-test-toggle-format)

    (should (eq 'text wal-run-test--coverage-format))

    (setq wal-run-test--coverage-format 'else)

    (wal-run-test-toggle-format)

    (should (eq 'text wal-run-test--coverage-format))))

(ert-deftest run-test-success-handler--checks-coverage ()
  (bydi ((:mock bydi-coverage--average :return "999")
         message)
    (funcall (wal-run-test--on-success))

    (bydi-was-called bydi-coverage--average)))

(ert-deftest run-test-failure-handler--messages ()
  (bydi (message)
    (funcall (wal-run-test--on-failure) "because")

    (bydi-was-called-with message (list "Tests fail: %s" "because"))))

(ert-deftest load-test-helper ()
  (bydi (require)

    (wal-config-load-test-helper)
    (bydi-was-called-with require (list 'bydi))))

(ert-deftest test-wal-make--scripts ()
  (bydi (wal-make)

    (wal-run-pacify)
    (bydi-was-called-with wal-make "pacify")

    (wal-run-cold-boot)
    (bydi-was-called-with wal-make "cold-boot")))

;;; wal-config-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
