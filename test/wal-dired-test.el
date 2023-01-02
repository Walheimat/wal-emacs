;;; wal-dired-test.el.el --- Tests for Dired package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-dired nil t)

(ert-deftest tes-wal/dired-from-home ()
  (with-mock-history ((dired-read-dir-and-switches . (lambda (&rest _) (list  "/tmp")))
                      (dired-noselect . (lambda (&rest _) "/tmp/test"))
                      pop-to-buffer-same-window)

    (call-interactively 'wal/dired-from-home)

    (was-called-with dired-noselect (list "/tmp" nil))
    (was-called-with pop-to-buffer-same-window (list "/tmp/test"))))

(ert-deftest test-wal/image-dired ()
  (with-mock image-dired (lambda (x) x)
             (should (string-equal (expand-file-name default-directory) (wal/image-dired)))))

(ert-deftest test-wal/dired-buffer-p ()
  (with-temp-buffer
    (dired-mode)
    (should (wal/dired-buffer-p (current-buffer)))))

(ert-deftest test-wal/consult-dired-buffer--query ()
  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))
  (should (equal (wal/consult-dired-buffer--query) '(visibility buffer-name wal/dired-buffer-p))))

;;; wal-dired-test.el ends here
