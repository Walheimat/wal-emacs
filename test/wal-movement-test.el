;;; wal-movement-test.el --- Tests for movement package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-movement nil t)

(ert-deftest test-wal-avy-goto-word ()
  (defvar avy-goto-word-0 nil)
  (defvar avy-goto-word-0-regexp nil)

  (bydi (avy-goto-word-0
         avy-jump
         (:mock avy-with :with (lambda (_ b) b)))

    (with-temp-buffer
      (insert "test")

      (wal-avy-goto-word)

      (bydi-was-called-with avy-jump (list nil :beg (line-beginning-position) :end (line-end-position) :window-flip t))

      (wal-avy-goto-word '(4))

      (bydi-was-called-with avy-goto-word-0 t)

      (bydi-clear-mocks)

      (wal-avy-goto-word 1)

      (bydi-was-called-with avy-goto-word-0 t))))

(ert-deftest test-wal-avy-goto-line ()
  (bydi ((:mock avy-goto-line :with beginning-of-line)
         (:mock avy-goto-end-of-line :with end-of-line))

    (with-temp-buffer
      (insert "test")
      (goto-char (point-min))
      (wal-avy-goto-line)

      (should (equal (point) (point-max)))

      (wal-avy-goto-line t)
      (should (equal (point) (point-min))))))

(ert-deftest test-wal-avy-mark-region ()
  (defvar wal-avy-mark-region nil)
  (let ((positions '(0 5 5 0)))

    (bydi ((:mock avy-with :with (lambda (_ b) b))
           (:mock avy--line :with (lambda () (pop positions))))

      (with-temp-buffer
        (insert "test\ntesting")

        (should (equal 6 (wal-avy-mark-region)))
        (should (equal 6 (wal-avy-mark-region)))))))

(ert-deftest test-avy-action-zip-to-char ()
  (with-temp-buffer
    (insert "testing")
    (should (equal (point-max) (avy-action-zip-to-char (point-max))))))

(ert-deftest test-wal-then-goto-beginning-for-org-headings ()
  (bydi ((:ignore wal-univ-p)
         (:always org-at-heading-p))

    (with-temp-buffer
      (setq major-mode 'org-mode)
      (insert "testing")
      (goto-char (point-min))
      (wal-then-goto-beginning-for-org-headings)

      (should (equal 1 (point))))))

;;; wal-movement-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
