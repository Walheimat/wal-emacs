;;; wal-movement-test.el --- Tests for movement package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-movement nil t)

(ert-deftest wal-avy-goto-word ()
  :tags '(movement user-facing)

  (defvar avy-goto-word-0 nil)
  (defvar avy-goto-word-0-regexp nil)

  (bydi (avy-goto-word-0
         avy-jump
         require
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

(ert-deftest wal-avy-goto-line ()
  :tags '(movement user-facing)

  (bydi ((:mock avy-goto-line :with beginning-of-line)
         (:mock avy-goto-end-of-line :with end-of-line)
         require)

    (with-temp-buffer
      (insert "test")
      (goto-char (point-min))
      (wal-avy-goto-line)

      (should (equal (point) (point-max)))

      (wal-avy-goto-line t)
      (should (equal (point) (point-min))))))

(ert-deftest avy-action-zip-to-char ()
  :tags '(movement)

  (with-temp-buffer
    (insert "testing")
    (should (equal (point-max) (avy-action-zip-to-char (point-max))))))

(ert-deftest wal-then-goto-beginning-for-org-headings ()
  :tags '(movement)

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
