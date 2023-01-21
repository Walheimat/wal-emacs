;;; wal-look-test.el --- Tests for look functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-look nil t)

(ert-deftest test-wal/set-transparency ()
  (let ((entered-number nil))
    (with-mock ((read-number . (lambda (&rest _) entered-number)))

      (let ((emacs-major-version 29))

        (setq entered-number 90)
        (call-interactively 'wal/set-transparency)

        (should (eq 90 wal/transparency))
        (should (eq 90 (cdr (assoc 'alpha-background default-frame-alist)))))

      (let ((emacs-major-version 29))

        (setq wal/transparency 50)
        (wal/set-transparency)

        (should (eq 50 wal/transparency))
        (should (eq 50 (cdr (assoc 'alpha-background default-frame-alist)))))

      (let ((emacs-major-version 28))

        (setq entered-number 90)
        (call-interactively 'wal/set-transparency)

        (should (eq 90 wal/transparency))
        (should (eq 90 (cdr (assoc 'alpha default-frame-alist))))))))

(ert-deftest test-wal/load-active-theme ()
  (with-mock (load-theme run-hooks)

    (let ((wal/active-theme nil))

      (wal/load-active-theme)

      (was-not-called load-theme)
      (was-not-called run-hooks)

      (wal/clear-mocks))

    (let ((wal/active-theme 'test-theme))

      (wal/load-active-theme)
      (was-called-with load-theme (list 'test-theme t))
      (was-called-with run-hooks (list 'wal/theme-hook)))))

(ert-deftest test-wal/load-active-them--captures-error ()
  (let ((wal/active-theme 'non-existence))

    (ert-with-message-capture messages
      (wal/load-active-theme)

      (should (string= "Failed to load theme: Unable to find theme file for ‘non-existence’\n" messages)))))

;;; wal-look-test.el ends here
