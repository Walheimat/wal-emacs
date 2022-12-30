;;; wal-external-test.el --- Tests for external package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom `use-package' normalizer and handler.

;;; Code:

(require 'wal-external nil t)

(ert-deftest test-wal/use-package-normalize-binder ()
  (with-mock-all ((use-package-recognize-function . #'always)
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
  (with-mock-all ((package-installed-p . #'ignore)
                  (use-package-plist-maybe-put . #'plist-put)
                  (use-package-process-keywords . #'wal/ra))
    (with-mock wal/is-expansion-pack #'always
      (should (equal '((when nil name nil (:wal-ways nil))) (use-package-handler/:wal-ways 'name nil nil nil nil))))
    (with-mock wal/is-expansion-pack #'ignore
      (defvar wal/minimal)
      (defvar wal/flag-mini)
      (defvar wal/minimal-exclude)
      (let ((wal/minimal nil)
            (wal/flag-mini t)
            (wal/minimal-exclude '(name)))
        (should (equal '((when t name nil (:wal-ways t))) (use-package-handler/:wal-ways 'name nil nil nil nil)))))))

;;; wal-external-test.el ends here
