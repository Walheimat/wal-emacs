;;; wal-dired-test.el.el --- Tests for Dired package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-dired nil t)

(ert-deftest tes-wal-dired-from-home ()
  (bydi-with-mock ((dired-read-dir-and-switches . (lambda (&rest _) (list  "/tmp")))
                   (dired-noselect . (lambda (&rest _) "/tmp/test"))
                   pop-to-buffer-same-window)

    (call-interactively 'wal-dired-from-home)

    (bydi-was-called-with dired-noselect (list "/tmp" nil))
    (bydi-was-called-with pop-to-buffer-same-window "/tmp/test")))

(ert-deftest test-wal-image-dired ()
  (bydi-with-mock image-dired

    (wal-image-dired)

    (let ((expected (expand-file-name default-directory)))

      (bydi-was-called-with image-dired expected))))

(ert-deftest test-wal-dired-buffer-p ()
  (with-temp-buffer
    (dired-mode)

    (should (wal-dired-buffer-p (current-buffer)))))

(ert-deftest test-wal-consult-dired-buffer--query ()
  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-dired-buffer--query) '(visibility buffer-name wal-dired-buffer-p))))

;;; wal-dired-test.el ends here
