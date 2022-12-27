;;; wal-movement-test.el --- Tests for movement package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-movement nil t)

(ert-deftest test-wal/avy-goto-word ()
  (defvar avy-goto-word-0 nil)
  (defvar avy-goto-word-0-regexp nil)
  (cl-defun wal/avy-jump (_a &key beg end)
    ""
    (list beg end))
  (let ((out nil))
    (with-mock-all ((avy-goto-word-0 . (lambda (&rest _) (add-to-list 'out 'goto-word-0)))
                    (avy-jump . #'wal/avy-jump)
                    (avy-with . (lambda (_ b) b)))
      (with-temp-buffer
        (insert "test")
        (should (equal '(goto-word-0) (wal/avy-goto-word)))
        (should (equal '(1 5) (wal/avy-goto-word t)))))))

(ert-deftest test-wal/avy-goto-line ()
  (with-mock-all ((avy-goto-line . #'beginning-of-line)
                  (avy-goto-end-of-line . #'end-of-line))
    (with-temp-buffer
      (insert "test")
      (goto-char (point-min))
      (wal/avy-goto-line)
      (should (equal (point) (point-max)))
      (wal/avy-goto-line t)
      (should (equal (point) (point-min))))))

(ert-deftest test-wal/avy-mark-region ()
  (defvar wal/avy-mark-region nil)
  (let ((positions '(0 5 5 0)))
    (with-mock-all ((avy-with . (lambda (_ b) b))
                    (avy--line . (lambda () (pop positions))))
      (with-temp-buffer
        (insert "test\ntesting")
        (should (equal 6 (wal/avy-mark-region)))
        (should (equal 6 (wal/avy-mark-region)))))))

(ert-deftest test-avy-action-zip-to-char ()
  (with-temp-buffer
    (insert "testing")
    (should (equal (point-max) (avy-action-zip-to-char (point-max))))))

(ert-deftest test-wal/then-goto-beginning-for-org-headings ()
  (with-mock-all ((wal/univ-p . #'ignore)
                  (org-at-heading-p . #'always))
    (with-temp-buffer
      (setq major-mode 'org-mode)
      (insert "testing")
      (goto-char (point-min))
      (wal/then-goto-beginning-for-org-headings)
      (should (equal 1 (point))))))

;;; wal-movement-test.el ends here
