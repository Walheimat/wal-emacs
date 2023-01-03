;;; wal-movement-test.el --- Tests for movement package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-movement nil t)

(ert-deftest test-wal/avy-goto-word ()
  (defvar avy-goto-word-0 nil)
  (defvar avy-goto-word-0-regexp nil)

  (with-mock (avy-goto-word-0
              avy-jump
              (avy-with . (lambda (_ b) b)))

    (with-temp-buffer
      (insert "test")

      (wal/avy-goto-word t)

      (was-called-with avy-jump (list nil :beg (line-beginning-position) :end (line-end-position)))

      (wal/avy-goto-word)

      (was-called-with avy-goto-word-0 (list t)))))

(ert-deftest test-wal/avy-goto-line ()
  (with-mock ((avy-goto-line . #'beginning-of-line)
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

    (with-mock ((avy-with . (lambda (_ b) b))
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
  (with-mock ((wal/univ-p . #'ignore)
              (org-at-heading-p . #'always))

    (with-temp-buffer
      (setq major-mode 'org-mode)
      (insert "testing")
      (goto-char (point-min))
      (wal/then-goto-beginning-for-org-headings)

      (should (equal 1 (point))))))

;;; wal-movement-test.el ends here
