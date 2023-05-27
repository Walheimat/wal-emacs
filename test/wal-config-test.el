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

       (with-mock (wal-ascii-whale-animate run-with-timer cancel-timer kill-buffer)
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
    (was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "blue"))))

(ert-deftest waw--start-animation--cachalot ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'cachalot)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "cachalot"))))

(ert-deftest waw--start-animation--blue ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'blue)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (was-called wal-ascii-whale-animate)
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

    (was-called cancel-timer)
    (was-called kill-buffer)))

(ert-deftest waw-setup ()
  (with-mock (wal-ascii-whale--start-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (was-called wal-ascii-whale--start-animation)

      (should (buffer-local-value 'kill-buffer-hook (current-buffer)))
      (should (buffer-local-value 'window-configuration-change-hook (current-buffer))))))

(ert-deftest waw-clean-up ()
  (with-mock (posframe-delete
              wal-ascii-whale--start-animation
              wal-ascii-whale--stop-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (wal-ascii-whale-clean-up)

      (was-called posframe-delete)
      (was-called wal-ascii-whale--stop-animation))))

(ert-deftest waw-poshandler ()
  (let ((result (wal-ascii-whale-poshandler `(:parent-window-left 4
                                              :parent-window-top 4
                                              :parent-window-width 8
                                              :posframe-width 2
                                              :parent-window ,(selected-window)))))

    (should (equal '(9 . 5) result))))

(ert-deftest waw-hidehandler ()
  (with-mock ((get-buffer-window . #'ignore))

    (should (wal-ascii-whale-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest waw-display ()
  (let ((wal-ascii-whale-parent-buffer 'parent)
        (wal-ascii-whale-indirect-buffer 'indirect))

    (with-mock (posframe-show (face-attribute . #'wal-rf))

      (wal-ascii-whale-display)

      (was-called-with posframe-show (list 'indirect
                                           :accept-focus nil
                                           :border-width 12
                                           :border-color 'cursor
                                           :poshandler 'wal-ascii-whale-poshandler
                                           :posframe-parent-buffer 'parent
                                           :hidehandler 'wal-ascii-whale-hidehandler)))))

(ert-deftest waw-toggle-display ()
  (with-mock (wal-ascii-whale-clean-up
              wal-ascii-whale-setup
              wal-ascii-whale-display
              (require . #'always))

    (let ((wal-ascii-whale-parent-buffer t))

      (wal-ascii-whale-toggle-display)

      (was-called wal-ascii-whale-clean-up)
      (was-not-called wal-ascii-whale-setup))

    (wal-clear-mocks)
    (setq wal-ascii-whale-parent-buffer nil)

    (wal-ascii-whale-toggle-display)

    (was-called wal-ascii-whale-setup)
    (was-called wal-ascii-whale-display)
    (was-not-called wal-ascii-whale-clean-up)))

(ert-deftest test-wal-describe-config-version ()
  (defvar wal-emacs-config-default-path)
  (let ((out '("1.0.0" "test everything" "1.0.1" "letting the world know"))
        (wal-emacs-config-default-path "~"))
    (with-mock ((shell-command-to-string . (lambda (_) (pop out)))
                (message . #'wal-rf))

      (should (equal "1.0.0: test everything" (wal-describe-config-version)))

      (let ((noninteractive nil))

        (should (equal "1.0.1: letting the world know" (wal-describe-config-version)))))))

(ert-deftest test-wal-show-config-diff-range ()
  (with-mock ((shell-command-to-string . (lambda (_) " testing "))
              (magit-diff-range . #'wal-rf))

    (should (string-equal "testing" (wal-show-config-diff-range)))))

(ert-deftest test-wal-tangle-config-prompt ()
  (with-mock ((wal-tangle-do-prompt . #'always)
              (y-or-n-p . #'always)
              (wal-prelude-tangle-config . #'wal-rt))

    (let ((wal-tangle-do-prompt t))

      (should (equal 'testing (wal-tangle-config-prompt))))))

(ert-deftest test-wal-tangle-config-prompt--after ()
  (with-mock ((wal-tangle-do-prompt . #'always)
              (y-or-n-p . #'ignore)
              (wal-prelude-tangle-config . #'wal-rt)
              message)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)

      (was-called-with message "To tangle, call `wal-prelude-tangle-config'")
      (should-not wal-tangle-do-prompt)

      (wal-clear-mocks)
      (wal-tangle-config-prompt)

      (was-called-with message "Config changed. To tangle, call `wal-prelude-tangle-config'"))))

(ert-deftest test-wal-config-switch-project ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/config"))

    (with-mock (project-switch-project)

      (wal-config-switch-project)

      (was-called-with project-switch-project (list "/tmp/config")))))

(ert-deftest test-wal-config-consult-org-heading ()
  (defvar wal-emacs-config-lib-path)
  (let ((wal-emacs-config-lib-path nil))

    (with-mock (consult-org-heading (directory-files . (lambda (&rest _) '("." ".." "/tmp/test.org" "/tmp/test-2.org"))))

      (wal-config-consult-org-heading)

      (was-called-with consult-org-heading (list nil '("/tmp/test.org" "/tmp/test-2.org"))))))

(ert-deftest test-wal-check-coverage--calculate-coverage ()
  (when (get-buffer "*wal-async*")
    (kill-buffer "*wal-async*"))

  (with-temp-buffer
    (rename-buffer "*wal-async*")


   (insert "wal-windows : Percent 50% [Relevant: 40 Covered: 20 Missed: 20]\nwal-windows : Percent 40% [Relevant: 20 Covered: 8 Missed: 12]")

   (should (string-equal "46.67%" (wal-check-coverage--calculate-coverage)))))

(ert-deftest test-wal-customize-group ()
  (with-mock customize-group

    (wal-customize-group)

    (was-called-with customize-group (list 'wal t))))

(ert-deftest test-wal-make--handlers ()
  (ert-with-message-capture messages
    (funcall (wal-make--on-success "cold-boot"))
    (funcall (wal-make--on-failure "cold-boot") "test")

    (should (string= messages "Script ’cold-boot’ succeeded.\nScript ’cold-boot’ failed.\n\ntest\n"))))

(ert-deftest test-wal-make ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/default"))

    (with-mock (wal-async-process
                (wal-make--on-success . (lambda (_) 'success))
                (wal-make--on-failure . (lambda (_) 'failure)))

      (wal-make "cold-boot")

      (was-called-with wal-async-process '("cd /tmp/default && make cold-boot"
                                           success
                                           failure
                                           t)))))

(ert-deftest run-test--adds-post-command ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp/default"))

    (with-mock (wal-async-process
                (wal-run-test--on-success . (lambda () 'success))
                (wal-run-test--on-failure . (lambda () 'failure)))

      (wal-run-test)

      (was-called-with wal-async-process '("cd /tmp/default && make test && cat coverage/results.txt"
                                           success
                                           failure
                                           t)))))

(ert-deftest run-test--adds-pre-command-for-json ()
  (defvar wal-emacs-config-default-path)
  (defvar wal-run-test--coverage-format)
  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-run-test--coverage-format 'json))

    (with-mock (wal-async-process
                (wal-make--on-success . (lambda (_) 'success))
                (wal-make--on-failure . (lambda (_) 'failure)))

      (wal-run-test)

      (was-called-with wal-async-process '("export COVERAGE_WITH_JSON=true && cd /tmp/default && make test"
                                           success
                                           failure
                                           t)))))

(ert-deftest run-test-file--passes-file-as-args ()
  (defvar wal-emacs-config-default-path)
  (defvar wal-run-test--coverage-format)

  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-run-test--coverage-format 'text))

    (with-mock (wal-run-test
                (read-file-name . (lambda (_p _d _d _m _i pred &rest _)
                                    (let ((selected "/tmp/tests/test.el"))
                                      (if (funcall pred selected)
                                          selected
                                        nil)))))

      (call-interactively 'wal-run-test-file)

      (was-called-with wal-run-test "test TEST_ARGS=/tmp/tests/test.el"))))

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
  (with-mock ((wal-check-coverage--calculate-coverage . (lambda () "999%"))
              message)
    (funcall (wal-run-test--on-success))

    (was-called wal-check-coverage--calculate-coverage)
    (was-called-with message (list "All tests succeeded. Coverage: %s" "999%"))))

(ert-deftest run-test-failure-handler--messages ()
  (with-mock (message)
    (funcall (wal-run-test--on-failure) "because")

    (was-called-with message (list "Tests fail: %s" "because"))))

(ert-deftest load-test-helper ()
  (with-mock (load-file)

    (defvar wal-emacs-config-default-path)
    (let ((wal-emacs-config-default-path "/tmp"))

      (wal-config-load-test-helper)
      (was-called-with load-file (list "/tmp/test/test-helper.el")))))

(ert-deftest test-wal-make--scripts ()
  (with-mock (wal-make)

    (wal-run-pacify)
    (was-called-with wal-make "pacify")

    (wal-run-cold-boot)
    (was-called-with wal-make "cold-boot")))

;;; wal-test.el ends here
