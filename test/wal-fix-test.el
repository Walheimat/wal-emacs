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

    (with-mock display-buffer

      (let* ((buf (find-file-noselect wal/tmp-file))
             (cb (wal/flycheck-file--callback wal/tmp-file buf nil t)))

        (apply cb '(nil nil))

        (with-current-buffer (wal/flycheck-file--get-buffer)

          (should (string-equal "No errors in 'check'.\n" (buffer-string))))

        (let ((expected (wal/flycheck-file--get-buffer)))

          (was-called-with display-buffer expected)))))

  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file--callback-on-error ()
  (wal/with-temp-file "check-error"

    (let* ((buf (find-file-noselect wal/tmp-file))
           (cb (wal/flycheck-file--callback wal/tmp-file buf t t)))

      (with-mock ((flycheck-error-message . #'wal/rf)
                  (flycheck-error-line . (lambda (_) 1)))

        (apply cb '(nil ("testing"))))

      (with-current-buffer (wal/flycheck-file--get-buffer)

        (should (string-equal "Errors in file 'check-error':\nline 1: testing\n\n" (buffer-string))))))

  (kill-buffer wal/flycheck-file--buffer))

(ert-deftest test-wal/flycheck-file ()
  (with-mock ((flycheck-get-checker-for-buffer . #'wal/rt)
              flycheck-syntax-check-new
              flycheck-compute-working-directory
              flycheck-syntax-check-start
              (wal/flycheck-file--callback . #'wal/rt))
    (wal/with-temp-file "flycheck"

      (wal/flycheck-file wal/tmp-file)

      (let ((buf (find-file-noselect wal/tmp-file)))

        (was-called-with flycheck-syntax-check-start
                         (list
                          (list :buffer buf :checker 'testing :context nil :working-directory (list 'testing))
                          'testing))))))

(ert-deftest test-wal/flycheck-file--no-checker ()
  (with-mock ((flycheck-get-checker-for-buffer . #'ignore)
              flycheck-syntax-check-new
              flycheck-compute-working-directory
              flycheck-syntax-check-start
              wal/flycheck-file--callback)

    (wal/with-temp-file "flycheck"

      (should-error (wal/flycheck-file wal/tmp-file) :type 'user-error))))

(ert-deftest test-wal/flyspell ()
  (defvar flyspell-mode)
  (with-mock (flyspell-mode flyspell-prog-mode)

    (let ((flyspell-mode t))

      (wal/flyspell)

      (was-called-with flyspell-mode -1))

    (let ((flyspell-mode nil))

      (wal/flyspell)

      (was-called flyspell-mode)
      (was-not-called flyspell-prog-mode)
      (wal/clear-mocks)

      (with-temp-buffer
        (prog-mode)

        (wal/flyspell)

        (was-not-called flyspell-mode)
        (was-called flyspell-prog-mode)))))

(ert-deftest test-wal/flyspell-goto-previous-error ()
  (with-mock (flyspell-goto-next-error)

    (wal/flyspell-goto-previous-error)

    (was-called-with flyspell-goto-next-error (list t))))

;;; wal-fix-test.el ends here
