;;; wal-fix-test.el --- Tests for fixing package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-fix nil t)

(ert-deftest test-wal/flycheck-file--get-buffer ()
  (with-current-buffer (wal/flycheck-file--get-buffer)
    (should view-mode)
    (should (string-equal wal/flycheck-file--buffer (buffer-name))))
  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file--write ()
  (wal/flycheck-file--write "hello")
  (with-current-buffer (wal/flycheck-file--get-buffer)
    (should (string-equal "hello\n" (buffer-string))))
  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file--erase ()
  (wal/flycheck-file--write "testing" t)
  (with-current-buffer (wal/flycheck-file--get-buffer)
    (should (string-equal "testing" (buffer-string)))
    (wal/flycheck-file--erase)
    (should (string-equal "" (buffer-string)))))

(ert-deftest test-wal/flycheck-file--callback ()
  (wal/with-temp-file "check"
    (let* ((buf (find-file-noselect wal/tmp-file))
           (cb (wal/flycheck-file--callback wal/tmp-file buf nil t))
           (out nil))
      (with-mock display-buffer (lambda (b) (setq out (buffer-name b)))
        (apply cb '(nil nil)))
      (with-current-buffer (wal/flycheck-file--get-buffer)
        (should (string-equal "No errors in 'check'.\n" (buffer-string))))
      (should (equal wal/flycheck-file--buffer out))))
  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file--callback-on-error ()
  (wal/with-temp-file "check-error"
    (let* ((buf (find-file-noselect wal/tmp-file))
           (cb (wal/flycheck-file--callback wal/tmp-file buf t t)))
      (with-mock-all ((flycheck-error-message . #'wal/rf)
                      (flycheck-error-line . (lambda (_) 1)))
        (apply cb '(nil ("testing"))))
      (with-current-buffer (wal/flycheck-file--get-buffer)
        (should (string-equal "Errors in file 'check-error':\nline 1: testing\n\n" (buffer-string))))))
  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file ()
  (with-mock-all ((flycheck-get-checker-for-buffer . #'wal/rt)
                  (flycheck-syntax-check-new . #'wal/rt)
                  (flycheck-compute-working-directory . #'wal/rt)
                  (flycheck-syntax-check-start . #'wal/ra)
                  (wal/flycheck-file--callback . #'wal/rf))
    (wal/with-temp-file "flycheck"
      (should (equal (list 'testing wal/tmp-file) (wal/flycheck-file wal/tmp-file))))))

(ert-deftest test-wal/flycheck-file--no-checker ()
  (with-mock-all ((flycheck-get-checker-for-buffer . #'ignore)
                  (flycheck-syntax-check-new . #'wal/rt)
                  (flycheck-compute-working-directory . #'wal/rt)
                  (flycheck-syntax-check-start . #'wal/ra)
                  (wal/flycheck-file--callback . #'wal/rf))
    (wal/with-temp-file "flycheck"
      (should-error (wal/flycheck-file wal/tmp-file) :type 'user-error))))

(ert-deftest test-wal/flyspell ()
  (defvar flyspell-mode)
  (with-mock-all ((flyspell-mode . (lambda (&optional _) 'normal))
                  (flyspell-prog-mode . (lambda (&optional _) 'prog)))
    (let ((flyspell-mode t))
      (should (equal 'normal (wal/flyspell))))
    (let ((flyspell-mode nil))
      (should (equal 'normal (wal/flyspell)))
      (with-temp-buffer
        (prog-mode)
        (should (equal 'prog (wal/flyspell)))))))

;;; wal-fix-test.el ends here
