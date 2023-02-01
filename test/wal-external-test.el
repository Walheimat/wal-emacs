;;; wal-external-test.el --- Tests for external package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom `use-package' normalizer and handler.

;;; Code:

(require 'wal-external nil t)

(ert-deftest test-wal/use-package-normalize-binder ()
  (with-mock ((use-package-recognize-function . #'always)
              (wal/prefix-user-key . (lambda (x) (format "C-t %s" x)))
              (use-package-error . (lambda (x) (error x))))

    (should (equal (list (cons "C-t w" 'testing)) (wal/use-package-normalize-binder nil nil (list (cons "w" 'testing)))))

    (should (equal (list (cons "C-t w" 'testing)) (wal/use-package-normalize-binder nil nil (list (list (cons "w" 'testing))))))

    (should-error (wal/use-package-normalize-binder ':wal-bind nil (list "testing")))

    (setq wal/up-test-sym t)
    (dolist (it (list :map :prefix-map :package))

      (should (equal (list it 'wal/up-test-sym (cons "C-t w" 'testing)) (wal/use-package-normalize-binder nil nil (list it 'wal/up-test-sym (cons "w" 'testing))))))

    (dolist (it (list :prefix-docstring :prefix :menu-name))

      (should (equal (list it "testing" (cons "C-t w" 'testing)) (wal/use-package-normalize-binder nil nil (list it "testing" (cons "w" 'testing))))))))

(ert-deftest test-use-package-handler/:wal-ways ()
  (let ((toggle t))
    (with-mock ((package-installed-p . #'ignore)
                use-package-plist-maybe-put
                use-package-process-keywords
                (junk-pack-p . (lambda (_) toggle)))

      (should (equal '((when nil name nil (nil :wal-ways nil)))
                     (use-package-handler/:wal-ways 'name nil nil nil nil)))

      (setq toggle nil)
      (wal/clear-mocks)
      (defvar wal/minimal)
      (defvar wal/flag-mini)
      (defvar wal/minimal-exclude)
      (let ((wal/minimal nil)
            (wal/flag-mini t)
            (wal/minimal-exclude '(name)))

        (should (equal '((when t name nil (nil :wal-ways t)))
                       (use-package-handler/:wal-ways 'name nil nil nil nil)))))))

(ert-deftest test-wal/ignore-if-not-installed ()
  (let ((installed nil)
        (built-in nil))
    (with-mock ((package-installed-p . (lambda (_) installed))
                (package-built-in-p . (lambda (_) built-in)))

      (should-not (wal/ignore-if-not-installed 'some-package))

      (setq installed t)
      (should (wal/ignore-if-not-installed 'some-package))

      (setq installed nil
            built-in t)
      (should (wal/ignore-if-not-installed 'some-package)))))

;;; wal-external-test.el ends here
