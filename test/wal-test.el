;;; wal-test.el --- Tests for main package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal nil t)

(ert-deftest test-wal-ascii-whale-animate ()
  (let ((wal-ascii-whale-key-frames ["testing"])
        (wal-ascii-whale-frame-index 0))

    (wal-ascii-whale-animate)

    (with-current-buffer (get-buffer-create wal-ascii-whale-buffer)
      (should (string-equal "testing" (buffer-string))))))

(ert-deftest test-wal-ascii-whale-setup--fails-for-unknown-whale ()
  (with-mock ((featurep . #'always))

    (let ((wal-config-ascii-whale 'testing))

      (should-error (wal-ascii-whale-setup) :type 'user-error))))

(ert-deftest test-wal-ascii-whale-setup ()
  (with-mock (featurep
              add-hook
              run-with-timer
              wal-ascii-whale-animate
              wal-ascii-whale-display)

    (let ((wal-ascii-whale-animation-speed 8))

      (wal-ascii-whale-setup)

      (was-called-with featurep (list 'posframe))
      (was-called-nth-with add-hook (list 'kill-buffer-hook #'wal-ascii-whale-clean-up nil t) 0)
      (was-called-nth-with add-hook (list 'window-configuration-change-hook #'wal-ascii-whale-display nil t) 1)

      (was-called-with run-with-timer (list 0 8 #'wal-ascii-whale-animate))
      (was-called wal-ascii-whale-animate)
      (was-called wal-ascii-whale-display))

    (wal-clear-mocks)

    (let ((wal-config-ascii-whale 'cachalot)
          (wal-ascii-whale-timer 'exists))

      (wal-ascii-whale-setup)

      (was-called wal-ascii-whale-display))))

(ert-deftest test-wal-ascii-whale-clean-up ()
   (let ((wal-ascii-whale-timer 'timer))

    (with-mock (cancel-timer
                posframe-delete
                remove-hook)

      (wal-ascii-whale-clean-up)

      (should-not wal-ascii-whale-timer)
      (was-called-with cancel-timer (list 'timer))
      (was-called-with posframe-delete wal-ascii-whale-buffer)
      (was-called-nth-with remove-hook (list 'kill-buffer-hook #'wal-ascii-whale-clean-up t) 0)
      (was-called-nth-with remove-hook (list 'window-configuration-change-hook #'wal-ascii-whale-display t) 1)

      (wal-clear-mocks)

      (wal-ascii-whale-clean-up)

      (was-not-called cancel-timer))))

(ert-deftest test-wal-ascii-whale-poshandler ()
  (let ((result (wal-ascii-whale-poshandler `(:parent-window-left 4
                                              :parent-window-top 4
                                              :parent-window-width 8
                                              :posframe-width 2
                                              :parent-window ,(selected-window)))))

    (should (equal '(9 . 5) result))))

(ert-deftest test-wal-ascii-whale-hidehandler ()
  (with-mock ((get-buffer-window . #'ignore))

    (should (wal-ascii-whale-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest test-wal-ascii-whale-display ()
  (let ((wal-ascii-whale-parent-buffer 'parent))
    (with-mock (posframe-show (face-attribute . #'wal-rf))

      (wal-ascii-whale-display)

      (was-called-with posframe-show (list wal-ascii-whale-buffer
                                           :accept-focus nil
                                           :border-width 12
                                           :border-color 'cursor
                                           :poshandler 'wal-ascii-whale-poshandler
                                           :posframe-parent-buffer 'parent
                                           :hidehandler 'wal-ascii-whale-hidehandler))

      (wal-clear-mocks)

      (setq wal-ascii-whale-parent-buffer nil)

      (wal-ascii-whale-display)

      (was-called-with posframe-show (list wal-ascii-whale-buffer
                                           :accept-focus nil
                                           :border-width 12
                                           :border-color 'cursor
                                           :poshandler 'wal-ascii-whale-poshandler
                                           :posframe-parent-buffer (current-buffer)
                                           :hidehandler 'wal-ascii-whale-hidehandler)))))

(ert-deftest test-wal-ascii-whale-toggle-display ()
  (with-mock (wal-ascii-whale-clean-up wal-ascii-whale-setup)

    (let ((wal-ascii-whale-timer t))

      (wal-ascii-whale-toggle-display)

      (was-called wal-ascii-whale-clean-up)
      (was-not-called wal-ascii-whale-setup))

    (wal-clear-mocks)

    (let ((wal-ascii-whale-timer nil))

      (wal-ascii-whale-toggle-display)

      (was-called wal-ascii-whale-setup)
      (was-not-called wal-ascii-whale-clean-up))))

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
              (wal-tangle-config . #'wal-rt))

    (let ((wal-tangle-do-prompt t))

      (should (equal 'testing (wal-tangle-config-prompt))))))

(ert-deftest test-wal-tangle-config-prompt--after ()
  (with-mock ((wal-tangle-do-prompt . #'always)
              (y-or-n-p . #'ignore)
              (wal-tangle-config . #'wal-rt)
              message)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)

      (was-called-with message "To tangle, call `wal-tangle-config'")
      (should-not wal-tangle-do-prompt)

      (wal-clear-mocks)
      (wal-tangle-config-prompt)

      (was-called-with message "Config changed. To tangle, call `wal-tangle-config'"))))

(ert-deftest test-wal-find-config ()
  (defvar wal-emacs-config-lib-path)
  (let ((wal-emacs-config-lib-path nil))

    (with-mock (consult-org-heading (wal-directory-files . (lambda (_) '("/tmp/test.org" "/tmp/test-2.org"))))

      (wal-config-consult-org-heading)

      (was-called-with consult-org-heading (list nil '("/tmp/test.org" "/tmp/test-2.org"))))))

(ert-deftest test-wal-check-coverage--calculate-coverage ()
  (kill-buffer "*wal-async*")

  (with-temp-buffer
    (rename-buffer "*wal-async*")


   (insert "wal-windows : Percent 50% [Relevant: 40 Covered: 20 Missed: 20]\nwal-windows : Percent 40% [Relevant: 20 Covered: 8 Missed: 12]")

   (should (string-equal "46.67%" (wal-check-coverage--calculate-coverage)))))

(ert-deftest test-wal-customize-group ()
  (with-mock customize-group

    (wal-customize-group)

    (was-called-with customize-group (list 'wal t))))

(ert-deftest test-wal-check-coverage ()
  (with-mock (wal-coverage--execute
              (wal-check-coverage--calculate-coverage . #'wal-rt)
              message)

    (cl-destructuring-bind (cmd succ fail) (wal-check-coverage)

      (should (string-equal "cask exec ert-runner && cat coverage/results.txt" cmd))
      (should (equal '("All tests succeeded. Coverage: %s" testing) (funcall succ)))
      (should (equal '("Tests fail: %s" "No they don't") (apply fail '("No they don't")))))))

(ert-deftest test-wal-create-json-coverage ()
  (with-mock (wal-coverage--execute message)
    (cl-destructuring-bind (cmd succ fail) (wal-create-json-coverage)

      (should (string-equal "export COVERAGE_WITH_JSON=true && cask exec ert-runner" cmd))
      (should (equal '("Coverage created") (funcall succ)))
      (should (equal '("Failed to create coverage: %s" "No you didn't") (apply fail '("No you didn't")))))))

(ert-deftest test-wal-coverage--execute--fails-without-cask ()
  (with-mock ((executable-find . #'ignore))

    (should-error (wal-coverage--execute nil nil nil))))

(ert-deftest test-wal-coverage--execute ()
  (defvar wal-emacs-config-default-path)
  (let ((wal-emacs-config-default-path "/tmp"))

    (with-mock (wal-async-process)

      (wal-coverage--execute "test" 'success 'failure)

      (was-called-with wal-async-process '("cd /tmp && test" success failure t)))))

(ert-deftest test-wal-package-files ()
  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt")
         (clean (lambda () (delete-directory dir t))))

    (make-directory dir)
    (make-empty-file file)
    (make-empty-file other-file)

    (condition-case nil
        (with-mock ((wal-directory-files . (lambda (_) (list file))))

          (should (equal (list file) (wal-package-files)))

          (funcall clean))
      (error
       (funcall clean)))))

(ert-deftest test-wal-checkdoc-config-packages ()
  (defvar wal-emacs-config-package-path)
  (let* ((in '("/tmp/one" "/tmp/two")))

    (with-mock (require
                (wal-package-files . (lambda () in))
                checkdoc-file)

      (wal-checkdoc-config-packages)

      (was-called-with require (list 'checkdoc nil t))

      (was-called-nth-with checkdoc-file (list "/tmp/one") 0)
      (was-called-nth-with checkdoc-file (list "/tmp/two") 1))))

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

(ert-deftest test-wal-make--scripts ()
  (with-mock (wal-make)

    (wal-run-pacify)
    (was-called-with wal-make "pacify")

    (wal-run-cold-boot)
    (was-called-with wal-make "cold-boot")))

;;; wal-test.el ends here
