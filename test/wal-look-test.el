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
  (let ((wal/active-theme nil))

    (should-not (wal/load-active-theme)))

  (with-mock ((custom-theme-p . #'always)
              enable-theme)

    (let ((wal/active-theme 'wombat))

      (should (wal/load-active-theme))
      (was-called-with enable-theme (list 'wombat)))))

;;; wal-look-test.el ends here
