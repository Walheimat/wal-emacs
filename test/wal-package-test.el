;;; wal-package-test.el --- Tests for external package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom `use-package' normalizer and handler.

;;; Code:

(require 'wal-package nil t)

(ert-deftest test-wal-use-package-normalize-binder ()
  (bydi-with-mock ((use-package-recognize-function . #'always)
                   (wal-prefix-user-key . (lambda (x) (format "C-t %s" x)))
                   (use-package-error . (lambda (x) (error x))))

    (should (equal (list (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list (cons "w" 'testing)))))

    (should (equal (list (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list (list (cons "w" 'testing))))))

    (should-error (wal-use-package-normalize-binder ':wal-bind nil (list "testing")))

    (setq wal-up-test-sym t)
    (dolist (it (list :map :prefix-map :package))

      (should (equal (list it 'wal-up-test-sym (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list it 'wal-up-test-sym (cons "w" 'testing))))))

    (dolist (it (list :prefix-docstring :prefix :menu-name))

      (should (equal (list it "testing" (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list it "testing" (cons "w" 'testing))))))))

(ert-deftest test-use-package-handler/:wal-ways ()
  (let ((toggle t))
    (bydi-with-mock ((package-installed-p . #'ignore)
                     use-package-plist-maybe-put
                     use-package-process-keywords
                     (junk--pack-p . (lambda (_) toggle)))

      (should (equal '((when nil name nil (nil :wal-ways nil)))
                     (use-package-handler/:wal-ways 'name nil nil nil nil)))

      (setq toggle nil)
      (bydi-clear-mocks)
      (defvar wal-minimal)
      (defvar wal-flag-mini)
      (defvar wal-minimal-exclude)
      (let ((wal-minimal nil)
            (wal-flag-mini t)
            (wal-minimal-exclude '(name)))

        (should (equal '((when t name nil (nil :wal-ways t)))
                       (use-package-handler/:wal-ways 'name nil nil nil nil)))))))

(ert-deftest test-wal-ignore-if-not-installed ()
  (let ((installed nil)
        (built-in nil)
        (user nil))
    (bydi-with-mock ((package-installed-p . (lambda (_) installed))
                     (package-built-in-p . (lambda (_) built-in))
                     (package--user-selected-p . (lambda (_) user)))

      (should-not (wal-ignore-if-not-installed 'some-package))

      (setq installed t)
      (should (wal-ignore-if-not-installed 'some-package))

      (setq installed nil
            built-in t)
      (should (wal-ignore-if-not-installed 'some-package))

      (setq installed nil
            built-in nil
            user t)
      (should (wal-ignore-if-not-installed 'some-package)))))

(ert-deftest test-wal-use-package-ensure-function ()

  (defvar package-archive-contents)
  (defvar package-pinned-packages)

  (let ((built-in nil)
        (installed nil)
        (expansion nil)
        (package-archive-contents nil)
        (package-pinned-packages '((some-package . "test"))))

    (bydi-with-mock (use-package-as-symbol
                     use-package-pin-package
                     (package-installed-p . (lambda (&rest _) installed))
                     (package-built-in-p . (lambda (&rest _) built-in))
                     (junk--pack-p . (lambda (&rest _) expansion))
                     package-read-all-archive-contents
                     package-refresh-contents
                     package-install
                     require)

      (wal-use-package-ensure-elpa-if-not-built-in-or-expansion
       'some-package
       (list t)
       nil)

      (bydi-was-called-with package-install (list 'some-package))

      (bydi-clear-mocks)

      (setq package-archive-contents '((some-package . 'content)))

      (wal-use-package-ensure-elpa-if-not-built-in-or-expansion
       'some-package
       (list t)
       nil)

      (bydi-was-called-with package-install (list 'some-package))

      (bydi-clear-mocks)

      (setq package-archive-contents nil)

      (wal-use-package-ensure-elpa-if-not-built-in-or-expansion
       'some-package
       (list 'truthy)
       nil)

      (bydi-was-called package-refresh-contents)

      (bydi-was-called-with package-install (list 'truthy))

      (bydi-clear-mocks)

      (setq expansion t)

      (wal-use-package-ensure-elpa-if-not-built-in-or-expansion
       'some-package
       (list 'truthy)
       nil)

      (bydi-was-not-called package-install))))

(ert-deftest test-wal-use-package-ensure-function--displays-warning-on-error ()
  (defvar package-archive-contents nil)
  (defvar package-pinned-packages '((some-package . 'version)))
  (defvar debug-on-error)

  (let ((debug-on-error nil))

    (bydi-with-mock (use-package-as-symbol
                     use-package-pin-package
                     (package-installed-p . (lambda (&rest _) nil))
                     (package-built-in-p . (lambda (&rest _) nil))
                     (junk--pack-p . (lambda (&rest _) nil))
                     (package-read-all-archive-contents . (lambda () (error "Testing")))
                     package-refresh-contents
                     package-install
                     require
                     display-warning)

      (wal-use-package-ensure-elpa-if-not-built-in-or-expansion
       'some-package
       (list t)
       nil)

      (bydi-was-called display-warning))))

;;; wal-package-test.el ends here
