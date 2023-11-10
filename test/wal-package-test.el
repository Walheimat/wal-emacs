;;; wal-package-test.el --- Tests for external package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom `use-package' normalizer and handler.

;;; Code:

(require 'wal-package nil t)

(ert-deftest wal-use-package-normalize-binder ()

  (bydi ((:always use-package-recognize-function)
         (:mock wal-prefix-user-key :with (lambda (x) (format "C-t %s" x)))
         (:mock use-package-error :with (lambda (x) (error x))))

    (should (equal (list (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list (cons "w" 'testing)))))

    (should (equal (list (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list (list (cons "w" 'testing))))))

    (should-error (wal-use-package-normalize-binder ':wal-bind nil (list "testing")))

    (setq wal-up-test-sym t)
    (dolist (it (list :map :prefix-map :package))

      (should (equal (list it 'wal-up-test-sym (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list it 'wal-up-test-sym (cons "w" 'testing))))))

    (dolist (it (list :prefix-docstring :prefix :menu-name))

      (should (equal (list it "testing" (cons "C-t w" 'testing)) (wal-use-package-normalize-binder nil nil (list it "testing" (cons "w" 'testing))))))))

(ert-deftest use-package-handler/:wal-ways ()
  (bydi ((:sometimes wal-package-built-in-p)
         use-package-plist-maybe-put
         use-package-process-keywords)

    (defvar wal-minimal)
    (defvar wal-flag-mini)

    (let ((wal-minimal nil)
          (wal-flag-mini t))

      (should (equal '((when t name nil (nil :wal-ways t)))
                     (use-package-handler/:wal-ways 'name nil t nil nil)))

      (should (equal '((when t name nil (nil :wal-ways t)))
                     (use-package-handler/:wal-ways 'name nil nil nil nil)))

      (bydi-toggle-sometimes)

      (should (equal '((when nil name nil (nil :wal-ways nil)))
                     (use-package-handler/:wal-ways 'name nil nil nil nil)))

      (setq wal-flag-mini nil)

      (should (equal '((when t name nil (nil :wal-ways t)))
                     (use-package-handler/:wal-ways 'name nil nil nil nil))))))

(ert-deftest sinker-handler ()
  (defvar use-package-hook-name-suffix)
  (let ((use-package-hook-name-suffix "-test"))
    (bydi ((:mock use-package-concat :with append)
           (:mock use-package-process-keywords :return (list 'kw))
           (:mock use-package-hook-handler-normalize-mode-symbols :with (lambda (x) (list x)))
           (:mock use-package-normalize-commands :with bydi-rf))

      (should (equal (use-package-handler/:sinker
                      'test
                      nil
                      '((kill-emacs . test-fn) (emacs-startup . test-fn))
                      nil
                      nil)
                     '(kw
                       (add-hook 'kill-emacs-test #'test-fn t)
                       (add-hook 'emacs-startup-test #'test-fn t)))))))

(ert-deftest fhook-handler ()
  (bydi (use-package-handler/:hook)

    (use-package-handler/:fhook 'name 'keyword 'args 'rest 'state)
    (bydi-was-called use-package-handler/:hook)))

(ert-deftest wal-ignore-if-not-installed ()
  (let ((installed nil)
        (built-in nil)
        (user nil))

    (bydi ((:mock wal-package-installed-p :return installed)
           (:mock package-built-in-p :return built-in)
           (:mock package--user-selected-p :return user))

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

(ert-deftest wal-use-package-ensure-function ()

  (defvar package-archive-contents)
  (defvar package-pinned-packages)

  (let ((built-in nil)
        (installed nil)
        (package-archive-contents nil)
        (package-pinned-packages '((some-package . "test"))))

    (bydi (use-package-as-symbol
           use-package-pin-package
           (:mock package-installed-p :return installed)
           (:mock package-built-in-p .:return built-in)
           package-read-all-archive-contents
           package-refresh-contents
           package-install
           require)

      (wal-use-package-ensure-elpa
       'some-package
       (list t)
       nil)

      (bydi-was-called-with package-install (list 'some-package))

      (bydi-clear-mocks)

      (setq package-archive-contents '((some-package . 'content)))

      (wal-use-package-ensure-elpa
       'some-package
       (list t)
       nil)

      (bydi-was-called-with package-install (list 'some-package))

      (bydi-clear-mocks)

      (setq package-archive-contents nil)

      (wal-use-package-ensure-elpa
       'some-package
       (list 'truthy)
       nil)

      (bydi-was-called package-refresh-contents)

      (bydi-was-called-with package-install (list 'truthy))

      (bydi-clear-mocks))))

(ert-deftest wal-use-package-ensure-function--displays-warning-on-error ()
  (defvar debug-on-error)

  (let ((debug-on-error nil))

    (bydi (use-package-as-symbol
           use-package-pin-package
           (:ignore package-installed-p)
           (:ignore package-built-in-p)
           package-read-all-archive-contents
           package-install
           (:mock package-refresh-contents :with (lambda () (error "Testing")))
           require
           display-warning)

      (wal-use-package-ensure-elpa
       'some-package
       (list t)
       nil)

      (bydi-was-called display-warning))))

(ert-deftest wal-package-installed-p ()
  (let ((wal-package--missed-built-in '(test)))
    (bydi ((:sometimes package-installed-p))

      (should (wal-package-installed-p 'other))
      (bydi-toggle-sometimes)
      (should-not (wal-package-installed-p 'other))
      (should (wal-package-installed-p 'test)))))

(ert-deftest wal-package-built-in-p ()
  (let ((wal-package--missed-built-in '(test)))
    (bydi ((:sometimes package-built-in-p))

      (should (wal-package-built-in-p 'other))
      (bydi-toggle-sometimes)
      (should-not (wal-package-built-in-p 'other))
      (should (wal-package-built-in-p 'test)))))

;;; wal-package-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
