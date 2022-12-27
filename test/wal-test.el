;;; wal-test.el --- Tests for main package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal nil t)

(ert-deftest test-wal/ascii-whale-animate ()
  (let ((wal/ascii-whale-key-frames ["testing"])
        (wal/ascii-whale-frame-index 0))
    (wal/ascii-whale-animate)
    (with-current-buffer (get-buffer-create wal/ascii-whale-buffer)
      (should (string-equal "testing" (buffer-string))))))

(ert-deftest test-wal/ascii-whale-setup--fails-for-unknown-whale ()
  (with-mock-all ((featurep . #'always))
    (let ((wal/config-ascii-whale 'testing))
      (should-error (wal/ascii-whale-setup) :type 'user-error))))

(ert-deftest test-wal/ascii-whale-setup ()
  (with-mock-all ((featurep . #'always)
                  (add-hook . #'ignore)
                  (run-with-timer . #'wal/ra)
                  (wal/ascii-whale-animate . #'ignore)
                  (wal/ascii-whale-display . #'wal/rt))
    (let ((wal/ascii-whale-animation-speed 8))
      (should (equal 'testing (wal/ascii-whale-setup)))
      (should (eq (nth 1 wal/ascii-whale-timer) 8)))
    (let ((wal/config-ascii-whale 'cachalot))
      (should (equal 'testing (wal/ascii-whale-setup))))))

(ert-deftest test-wal/ascii-whale-clean-up ()
  (let ((wal/ascii-whale-timer 'timer)
        (out nil))
    (with-mock-all ((cancel-timer . (lambda (x) (push x out)))
                    (posframe-delete . (lambda (x) (push x out)))
                    (remove-hook . #'ignore))
      (wal/ascii-whale-clean-up)
      (should-not wal/ascii-whale-timer)
      (should (equal (list wal/ascii-whale-buffer 'timer) out)))))

(ert-deftest test-wal/ascii-whale-poshandler ()
  (let ((result (wal/ascii-whale-poshandler `(:parent-window-left 4
                                              :parent-window-top 4
                                              :parent-window-width 8
                                              :posframe-width 2
                                              :parent-window ,(selected-window)))))
    (should (equal '(9 . 5) result))))

(ert-deftest test-wal/ascii-whale-hidehandler ()
  (with-mock get-buffer-window #'ignore
    (should (wal/ascii-whale-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest test-wal/ascii-whale-display ()
  (with-mock posframe-show #'wal/rf
    (should (equal wal/ascii-whale-buffer (wal/ascii-whale-display)))))

(ert-deftest test-wal/ascii-whale-toggle-display ()
  (with-mock-all ((wal/ascii-whale-clean-up . (lambda () 'clean))
                  (wal/ascii-whale-setup . (lambda () 'setup)))
    (let ((wal/ascii-whale-timer t))
      (should (equal 'clean (wal/ascii-whale-toggle-display))))
    (let ((wal/ascii-whale-timer nil))
      (should (equal 'setup (wal/ascii-whale-toggle-display))))))

(ert-deftest test-wal/describe-config-version ()
  (defvar wal/emacs-config-default-path)
  (let ((out '("1.0.0" "test everything" "1.0.1" "letting the world know"))
        (wal/emacs-config-default-path "~"))
    (with-mock-all ((shell-command-to-string . (lambda (_) (pop out)))
                    (message . #'wal/rf))
      (should (equal "1.0.0: test everything" (wal/describe-config-version)))
      (let ((noninteractive nil))
        (should (equal "1.0.1: letting the world know" (wal/describe-config-version)))))))

(ert-deftest test-wal/show-config-diff-range ()
  (with-mock-all ((shell-command-to-string . (lambda (_) " testing "))
                  (magit-diff-range . #'wal/rf))
    (should (string-equal "testing" (wal/show-config-diff-range)))))

(ert-deftest test-wal/tangle-config-prompt ()
  (with-mock-all ((wal/tangle-do-prompt . #'always)
                  (y-or-n-p . #'always)
                  (wal/tangle-config . #'wal/rt))
    (let ((wal/tangle-do-prompt t))
      (should (equal 'testing (wal/tangle-config-prompt))))))

(ert-deftest test-wal/tangle-config-prompt--after ()
  (let ((out nil))
    (with-mock-all ((wal/tangle-do-prompt . #'always)
                    (y-or-n-p . #'ignore)
                    (wal/tangle-config . #'wal/rt)
                    (message . (lambda (x) (setq out x))))
      (let ((wal/tangle-do-prompt t))
        (wal/tangle-config-prompt)
        (should (string-equal "To tangle, call `wal/tangle-config'" out))
        (should-not wal/tangle-do-prompt)
        (setq out nil)
        (wal/tangle-config-prompt)
        (should (string-equal "Config changed. To tangle, call `wal/tangle-config'" out))))))

(ert-deftest test-wal/find-config ()
  (defvar wal/emacs-config-default-path)
  (defvar wal/config-show-whale-animation)
  (let ((out nil)
        (wal/config-show-whale-animation t))
    (with-mock-all ((add-hook . (lambda (_ x &rest _) (setq out x)))
                    (wal/ascii-whale-setup . #'wal/rt))
      (should (equal 'testing (wal/find-config)))
      (should (equal 'wal/tangle-config-prompt out)))))

(ert-deftest test-wal/find-config-changelog ()
  (wal/find-config-changelog)
  (should (string-equal "CHANGELOG.md" (buffer-name))))

(ert-deftest test-wal/check-coverage--calculate-coverage ()
  (with-temp-buffer
    (rename-buffer "*wal-async*")
    (insert "wal-windows : Percent 50% [Relevant: 40 Covered: 20 Missed: 20]\nwal-windows : Percent 40% [Relevant: 20 Covered: 8 Missed: 12]")
    (should (string-equal "45%" (wal/check-coverage--calculate-coverage)))))

(ert-deftest test-wal/find-init ()
  (with-mock file-truename (lambda (_) wal/emacs-config-default-path)
    (wal/find-init)
    (should (string-equal (buffer-name) "emacs-config"))))

(ert-deftest test-wal/customize-group ()
  (with-mock customize-group #'wal/rf
    (should (equal 'wal (wal/customize-group)))))

(ert-deftest test-wal/dired-config-packages ()
  (wal/dired-config-packages)
  (should (string-equal (buffer-name) "wal")))

(ert-deftest test-wal/dired-config-tests ()
  (wal/dired-config-tests)
  (should (string-equal (buffer-name) "test")))

(ert-deftest test-wal/check-coverage ()
  (with-mock-all ((wal/coverage--execute . #'wal/ra)
                  (wal/check-coverage--calculate-coverage . #'wal/rt)
                  (message . #'wal/ra))
    (let ((res (wal/check-coverage)))
      (should (string-equal "cask exec ert-runner && cat coverage.txt" (nth 0 res)))
      (should (equal '("All tests succeeded. Coverage: %s" testing) (funcall (nth 1 res))))
      (should (equal '("Tests fail: %s" "No they don't") (apply (nth 2 res) '("No they don't")))))))

(ert-deftest test-wal/create-json-coverage ()
  (with-mock-all ((wal/coverage--execute . #'wal/ra)
                  (message . #'wal/ra))
    (let ((res (wal/create-json-coverage)))
      (should (string-equal "export COVERAGE_WITH_JSON=true && cask exec ert-runner" (nth 0 res)))
      (should (equal '("Coverage created") (funcall (nth 1 res))))
      (should (equal '("Failed to create coverage: %s" "No you didn't") (apply (nth 2 res) '("No you didn't")))))))

(ert-deftest test-wal/coverage--execute--fails-without-cask ()
  (with-mock executable-find #'ignore
    (should-error (wal/coverage--execute nil nil nil))))

(ert-deftest test-wal/coverage--execute ()
  (defvar wal/emacs-config-default-path)
  (let ((wal/emacs-config-default-path "/tmp"))
    (with-mock-all ((wal/async-process . #'wal/ra))

      (should (equal '("cd /tmp && test" success failure t) (wal/coverage--execute "test" 'success 'failure))))))

(ert-deftest test-wal/package-files ()
  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt")
         (clean (lambda () (delete-directory dir t))))
    (make-directory dir)
    (make-empty-file file)
    (make-empty-file other-file)
    (condition-case nil
        (with-mock wal/directory-files (lambda (_) (list file))
          (should (equal (list file) (wal/package-files)))
          (funcall clean))
      (error
       (funcall clean)))))

(ert-deftest test-wal/checkdoc-config-packages ()
  (defvar wal/emacs-config-package-path)
  (let* ((out nil)
         (in '("/tmp/one" "/tmp/two")))
    (with-mock-all ((require . #'ignore)
                    (wal/package-files . (lambda () in))
                    (checkdoc-file . (lambda (x) (push x out))))
      (wal/checkdoc-config-packages)
      (should (equal (reverse in) out)))))

(ert-deftest test-wal/flycheck-config-packages ()
  (let ((out nil)
        (in '("/tmp/one" "/tmp/two")))
    (with-mock-all ((display-buffer . #'ignore)
                    (wal/package-files . (lambda () in))
                    (wal/flycheck-file . (lambda (x _ _) (push x out))))
      (wal/flycheck-config-packages)
      (should (equal (reverse in) out)))))

;;; wal-test.el ends here
