;;; wal-fix-test.el --- Tests for fixing package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-fix nil t)

(ert-deftest wal-flycheck-file--get-buffer ()
  (bydi (view-mode)
    (with-current-buffer (wal-flycheck-file--get-buffer)

      (should view-mode)
      (should (string-equal wal-flycheck-file--buffer (buffer-name)))))

  (kill-buffer wal-flycheck-file--buffer))

(ert-deftest wal-flycheck-file--write ()
  (wal-flycheck-file--write "hello")

  (with-current-buffer (wal-flycheck-file--get-buffer)

    (should (string-equal "hello\n" (buffer-string))))

  (kill-buffer wal-flycheck-file--buffer))

(ert-deftest wal-flycheck-file--erase ()
  (wal-flycheck-file--write "testing" t)

  (with-current-buffer (wal-flycheck-file--get-buffer)

    (should (string-equal "testing" (buffer-string)))

    (wal-flycheck-file--erase)

    (should (string-equal "" (buffer-string)))))

(ert-deftest wal-flycheck-file--callback ()
  (ert-with-temp-file check

    (bydi display-buffer

      (let* ((buf (find-file-noselect check))
             (cb (wal-flycheck-file--callback check buf nil t)))

        (apply cb '(nil nil))

        (with-current-buffer (wal-flycheck-file--get-buffer)

          (should (string-match-p "No errors in" (buffer-string))))

        (let ((expected (wal-flycheck-file--get-buffer)))

          (bydi-was-called-with display-buffer expected)))))

  (kill-buffer wal-flycheck-file--buffer))

(ert-deftest wal-flycheck-file--callback-on-error ()
  (ert-with-temp-file check-error

    (let* ((buf (find-file-noselect check-error))
           (cb (wal-flycheck-file--callback check-error buf t t)))

      (bydi ((:mock flycheck-error-message :with bydi-rf)
             (:mock flycheck-error-line :return 1))

        (apply cb '(nil ("testing"))))

      (with-current-buffer (wal-flycheck-file--get-buffer)

        (should (string-match-p "Errors in file" (buffer-string)))
        (should (string-match-p "line 1: testing" (buffer-string))))))

  (kill-buffer wal-flycheck-file--buffer))

(ert-deftest wal-flycheck-file ()
  (bydi ((:mock flycheck-get-checker-for-buffer :with bydi-rt)
         flycheck-syntax-check-new
         flycheck-compute-working-directory
         flycheck-syntax-check-start
         (:mock wal-flycheck-file--callback :with bydi-rt))
    (ert-with-temp-file flycheck

      (wal-flycheck-file flycheck)

      (let ((buf (find-file-noselect flycheck)))

        (bydi-was-called-with flycheck-syntax-check-start
                              (list
                               (list :buffer buf :checker 'testing :context nil :working-directory (list 'testing))
                               'testing))))))

(ert-deftest wal-flycheck-file--no-checker ()
  (bydi ((:ignore flycheck-get-checker-for-buffer)
         flycheck-syntax-check-new
         flycheck-compute-working-directory
         flycheck-syntax-check-start
         wal-flycheck-file--callback)

    (ert-with-temp-file flycheck

      (should-error (wal-flycheck-file flycheck) :type 'user-error))))

(ert-deftest wal-flycheck-mode ()
  (bydi (flycheck-mode)
    (with-temp-buffer
      (setq major-mode 'test-mode)

      (let ((wal-flycheck-ignored-modes '(test-mode)))

        (wal-flycheck-mode)

        (bydi-was-not-called flycheck-mode)

        (setq wal-flycheck-ignored-modes nil)

        (wal-flycheck-mode)
        (bydi-was-called flycheck-mode)))))

(ert-deftest wal-flyspell ()
  (defvar flyspell-mode nil)
  (bydi (flyspell-mode flyspell-prog-mode)

    (let ((flyspell-mode t))

      (wal-flyspell)

      (bydi-was-called-with flyspell-mode -1))

    (let ((flyspell-mode nil))

      (wal-flyspell)

      (bydi-was-called flyspell-mode)
      (bydi-was-not-called flyspell-prog-mode)
      (bydi-clear-mocks)

      (with-temp-buffer

        (bydi ((:always derived-mode-p))
          (wal-flyspell)

          (bydi-was-not-called flyspell-mode)
          (bydi-was-called flyspell-prog-mode))))))

(ert-deftest wal-flyspell-goto-previous-error ()
  (bydi (flyspell-goto-next-error)

    (wal-flyspell-goto-previous-error)

    (bydi-was-called-with flyspell-goto-next-error (list t))))

;;; wal-fix-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
