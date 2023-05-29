;;; wal-visuals-test.el --- Tests for visuals package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test custom functionality.

;;; Code:

(require 'wal-visuals nil t)

(ert-deftest test-wal-font-update ()
  (with-mock (set-face-attribute (selected-frame . (lambda (&rest _) 'selected)))
    (wal-font-update :height 120 '(default) t)

    (was-called-with set-face-attribute (list 'default 'selected :height 120))))

(ert-deftest test-wal-read-sensible-font-height ()
  (let ((wal-fixed-font-height 121)
        (varying nil))

    (with-mock ((read-number . (lambda (&rest _) varying)))

      (setq varying 301)

      (should  (eq 300 (wal-read-sensible-font-height 'fixed)))
      (was-called-with read-number "Set fixed font (currently: 121): ")

      (wal-clear-mocks)

      (setq varying 79)
      (should (equal 80 (wal-read-sensible-font-height 'fixed)))

      (wal-clear-mocks)

      (setq varying 177)
      (should (equal 177 (wal-read-sensible-font-height 'fixed))))))

(ert-deftest test-wal-available-fonts ()
  (with-mock (font-spec
              (find-font . (lambda (specs) (string-equal "TestFont" (nth 1 specs)))))

    (should (equal (list "TestFont") (wal-available-fonts '("Mononoki" "TestFont" "Arial"))))
    (was-called-n-times find-font 3)
    (was-called-n-times font-spec 3)
    (was-called-nth-with font-spec (list :name "Arial") 2)))

(ert-deftest test-wal-read-font ()
  (let ((wal-fixed-fonts '("TestFont" "OtherFont")))

    (with-mock ((completing-read . (lambda (&rest _) "TestFont"))
                (face-attribute . (lambda (&rest _) "SomeFont"))
                (wal-available-fonts . (lambda (&rest _) wal-fixed-fonts)))

      (should (string= "TestFont" (wal-read-font 'fixed)))
      (was-called-with completing-read (list "Select fixed font (current: SomeFont) " wal-fixed-fonts)))))

(ert-deftest test-wal-select-fixed-or-variable-font ()
  (with-mock ((wal-read-font . (lambda (&rest _) "TestFont")) wal-font-update)

    (call-interactively 'wal-select-fixed-font)

    (was-called-with wal-font-update (list :font "TestFont" '(default fixed-pitch)))

    (call-interactively 'wal-select-variable-font)

    (was-called-with wal-font-update (list :font "TestFont" '(variable-pitch)))))

(ert-deftest test-wal-set-fixed-or-variable-font-height ()
  (with-mock ((wal-read-sensible-font-height . (lambda (&rest _) 101))
              wal-font-update)

    (call-interactively 'wal-set-fixed-font-height)

    (was-called-with wal-font-update (list :height 101 '(default fixed-pitch) nil))
    (wal-clear-mocks)

    (call-interactively 'wal-set-variable-font-height)

    (was-called-with wal-font-update (list :height 101 '(variable-pitch) nil))))

(ert-deftest test-wal-preferred-fonts ()
  (let ((wal-preferred-fonts '("PreferredFont" "NiceFont" "TestableFont")))

    (should (equal (list "TestableFont" "PreferredFont") (wal-preferred-fonts (list "CruelFont" "TestableFont" "WaningFont" "PreferredFont"))))))

(ert-deftest test-wal-fonts-candidate ()
  (with-mock ((wal-available-fonts . (lambda (_) (list "TestFont")))
              (wal-preferred-fonts . (lambda (_) (list "ZestFont"))))

    (should (equal "TestFont" (wal-fonts-candidate (list "TestFont" "ZestFont"))))
    (was-not-called wal-preferred-fonts)

    (wal-clear-mocks)

    (should (equal "ZestFont" (wal-fonts-candidate (list "TestFont" "ZestFont") t)))
    (was-called wal-preferred-fonts)))

(ert-deftest test-wal-font-lock ()
  (with-mock set-face-attribute

    (wal-font-lock)

    (was-called-nth-with set-face-attribute (list 'font-lock-comment-face nil :slant 'italic :weight 'normal) 0)
    (was-called-nth-with set-face-attribute (list 'font-lock-keyword-face nil :weight 'bold) 1)))

(ert-deftest test-wal-set-transparency ()
  (let ((entered-number nil))
    (with-mock ((read-number . (lambda (&rest _) entered-number)))

      (let ((emacs-major-version 29))

        (setq entered-number 90)
        (call-interactively 'wal-set-transparency)

        (should (eq 90 wal-transparency))
        (should (eq 90 (cdr (assoc 'alpha-background default-frame-alist)))))

      (let ((emacs-major-version 29))

        (setq wal-transparency 50)
        (wal-set-transparency)

        (should (eq 50 wal-transparency))
        (should (eq 50 (cdr (assoc 'alpha-background default-frame-alist)))))

      (let ((emacs-major-version 28))

        (setq entered-number 90)
        (call-interactively 'wal-set-transparency)

        (should (eq 90 wal-transparency))
        (should (eq 90 (cdr (assoc 'alpha default-frame-alist))))))))

(ert-deftest test-wal-load-active-theme ()
  (with-mock (load-theme run-hooks)

    (let ((wal-active-theme nil))

      (wal-load-active-theme)

      (was-not-called load-theme)
      (was-not-called run-hooks)

      (wal-clear-mocks))

    (let ((wal-active-theme 'test-theme))

      (wal-load-active-theme)
      (was-called-with load-theme (list 'test-theme t))
      (was-called-with run-hooks (list 'wal-theme-hook)))))

(ert-deftest test-wal-load-active-them--captures-error ()
  (let ((wal-active-theme 'non-existence))

    (ert-with-message-capture messages
      (wal-load-active-theme)

      (should (string= "Failed to load theme: Unable to find theme file for ‘non-existence’\n" messages)))))

(ert-deftest test-wal-with-recent-files-excluded ()
  (defvar recentf-exclude nil)

  (let ((fun (lambda (_) recentf-exclude)))

    (should (equal '("bookmarks\\'" "zettelkasten" "org/tasks") (wal-with-recent-files-excluded fun 'test)))))

(ert-deftest test-wal-instead-show-biased-random ()
  (defvar dashboard-footer-messages '("Testing"))
  (should (equal "Testing" (wal-instead-show-biased-random))))

(ert-deftest test-wal-in-case-of-daemonp-add-different-hook ()
  (with-mock (require (daemonp . #'always) add-hook)

    (wal-in-case-of-daemonp-add-different-hook)

    (should (string-equal "*dashboard*" (buffer-name (funcall initial-buffer-choice))))
    (was-called-with require (list 'all-the-icons nil t))
    (was-called-with add-hook (list 'server-after-make-frame-hook #'dashboard-insert-startupify-lists)))

  (with-mock ((daemonp . #'ignore) add-hook)

    (wal-in-case-of-daemonp-add-different-hook)

    (was-not-called add-hook)))

(ert-deftest test-wal-dashboard-get-buffer ()
  (defvar dashboard-force-refresh)
  (defvar dashboard-buffer-name)

  (let ((dashboard-buffer-name "dash"))

    (with-mock (dashboard-insert-startupify-lists get-buffer)

      (wal-dashboard-get-buffer)

      (was-called dashboard-insert-startupify-lists)

      (was-called-with get-buffer "dash"))))

;;; wal-visuals-test.el ends here
