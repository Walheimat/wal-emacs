;;; wal-prelude-test.el --- Test prelude package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions

;;; Code:

(require 'wal-prelude nil t)

(ert-deftest test-wal-prelude-package-files ()
  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt")
         (clean (lambda () (delete-directory dir t))))

    (make-directory dir)
    (make-empty-file file)
    (make-empty-file other-file)

    (condition-case nil
        (with-mock ((directory-files . (lambda (&rest _) (list "." ".." file))))

          (should (equal (list file) (wal-prelude-package-files)))

          (funcall clean))
      (error
       (funcall clean)))))

;;; wal-prelude-test.el ends here
