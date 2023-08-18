;;; wal-visuals-test.el --- Tests for visuals package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test custom functionality.

;;; Code:

(require 'wal-visuals nil t)

(ert-deftest test-wal-font-update ()
  (bydi (set-face-attribute
         (:mock selected-frame :return 'selected))
    (wal-font-update :height 120 '(default) t)

    (bydi-was-called-with set-face-attribute (list 'default 'selected :height 120))))

(ert-deftest test-wal-read-sensible-font-height ()
  (let ((wal-fixed-font-height 121)
        (varying nil))

    (bydi ((:mock read-number :return varying))

      (setq varying 301)

      (should  (eq 300 (wal-read-sensible-font-height 'fixed)))
      (bydi-was-called-with read-number "Set fixed font (currently: 121): ")

      (bydi-clear-mocks)

      (setq varying 79)
      (should (equal 80 (wal-read-sensible-font-height 'fixed)))

      (bydi-clear-mocks)

      (setq varying 177)
      (should (equal 177 (wal-read-sensible-font-height 'fixed))))))

(ert-deftest test-wal-available-fonts ()
  (bydi (font-spec
         (:mock find-font :with (lambda (specs) (string-equal "TestFont" (nth 1 specs)))))

    (should (equal (list "TestFont") (wal-available-fonts '("Mononoki" "TestFont" "Arial"))))
    (bydi-was-called-n-times find-font 3)
    (bydi-was-called-n-times font-spec 3)
    (bydi-was-called-nth-with font-spec (list :name "Arial") 2)))

(ert-deftest test-wal-read-font ()
  (let ((wal-fixed-fonts '("TestFont" "OtherFont")))

    (bydi ((:mock completing-read :return "TestFont")
           (:mock face-attribute :return "SomeFont")
           (:mock wal-available-fonts :return wal-fixed-fonts))

      (should (string= "TestFont" (wal-read-font 'fixed)))
      (bydi-was-called-with completing-read (list "Select fixed font (current: SomeFont) " wal-fixed-fonts)))))

(ert-deftest test-wal-select-fixed-or-variable-font ()
  (bydi ((:mock wal-read-font :return "TestFont") wal-font-update)

    (call-interactively 'wal-select-fixed-font)

    (bydi-was-called-with wal-font-update (list :font "TestFont" '(default fixed-pitch)))

    (call-interactively 'wal-select-variable-font)

    (bydi-was-called-with wal-font-update (list :font "TestFont" '(variable-pitch)))))

(ert-deftest test-wal-set-fixed-or-variable-font-height ()
  (bydi ((:mock wal-read-sensible-font-height :return 101)
         wal-font-update)

    (call-interactively 'wal-set-fixed-font-height)

    (bydi-was-called-with wal-font-update (list :height 101 '(default fixed-pitch) nil))
    (bydi-clear-mocks)

    (call-interactively 'wal-set-variable-font-height)

    (bydi-was-called-with wal-font-update (list :height 101 '(variable-pitch) nil))))

(ert-deftest test-wal-preferred-fonts ()
  (let ((wal-preferred-fonts '("PreferredFont" "NiceFont" "TestableFont")))

    (should (equal (list "TestableFont" "PreferredFont") (wal-preferred-fonts (list "CruelFont" "TestableFont" "WaningFont" "PreferredFont"))))))

(ert-deftest test-wal-fonts-candidate ()
  (bydi ((:mock wal-available-fonts :return (list "TestFont"))
         (:mock wal-preferred-fonts :return (list "ZestFont")))

    (should (equal "TestFont" (wal-fonts-candidate (list "TestFont" "ZestFont"))))
    (bydi-was-not-called wal-preferred-fonts)

    (bydi-clear-mocks)

    (should (equal "ZestFont" (wal-fonts-candidate (list "TestFont" "ZestFont") t)))
    (bydi-was-called wal-preferred-fonts)))

(ert-deftest test-wal-font-lock ()
  (bydi set-face-attribute

    (wal-font-lock)

    (bydi-was-called-nth-with set-face-attribute (list 'font-lock-comment-face nil :slant 'italic :weight 'normal) 0)
    (bydi-was-called-nth-with set-face-attribute (list 'font-lock-keyword-face nil :weight 'bold) 1)))

(ert-deftest test-wal-set-transparency ()
  (let ((entered-number nil))
    (bydi ((:mock read-number :return entered-number))

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
  (bydi (load-theme run-hooks)

    (let ((wal-active-theme nil))

      (wal-load-active-theme)

      (bydi-was-not-called load-theme)
      (bydi-was-not-called run-hooks)

      (bydi-clear-mocks))

    (let ((wal-active-theme 'test-theme))

      (wal-load-active-theme)
      (bydi-was-called-with load-theme (list 'test-theme t))
      (bydi-was-called-with run-hooks (list 'wal-theme-hook)))))

(ert-deftest test-wal-load-active-them--captures-error ()
  (let ((wal-active-theme 'non-existence))

    (ert-with-message-capture messages
      (wal-load-active-theme)

      (should (string= "Failed to load theme: Unable to find theme file for ‘non-existence’\n" messages)))))

(ert-deftest wal-visual-fill-column-mode--toggles ()
  (bydi (visual-fill-column-mode)
    (with-temp-buffer
      (wal-visual-fill-column-mode)
      (bydi-was-called-with visual-fill-column-mode '(-1))

      (setq visual-line-mode t)
      (bydi-clear-mocks)

      (wal-visual-fill-column-mode)
      (bydi-was-called-with visual-fill-column-mode '(+1)))))

(ert-deftest test-wal-with-recent-files-excluded ()
  (defvar recentf-exclude nil)

  (let ((fun (lambda (_) recentf-exclude)))

    (should (equal '("bookmarks\\'" "zettelkasten" "org/tasks") (wal-with-recent-files-excluded fun 'test)))))

(ert-deftest test-wal-instead-show-biased-random ()
  (defvar dashboard-footer-messages '("Testing"))
  (should (equal "Testing" (wal-instead-show-biased-random))))

(ert-deftest test-wal-in-case-of-daemonp-add-different-hook ()
  (bydi (require (:always daemonp) add-hook)

    (wal-in-case-of-daemonp-add-different-hook)

    (should (string-equal "*dashboard*" (buffer-name (funcall initial-buffer-choice))))
    (bydi-was-called-with require (list 'all-the-icons nil t))
    (bydi-was-called-with add-hook (list 'server-after-make-frame-hook #'dashboard-insert-startupify-lists)))

  (bydi ((:ignore daemonp) add-hook)

    (wal-in-case-of-daemonp-add-different-hook)

    (bydi-was-not-called add-hook)))

(ert-deftest test-wal-dashboard-get-buffer ()
  (defvar dashboard-force-refresh)
  (defvar dashboard-buffer-name)

  (let ((dashboard-buffer-name "dash"))

    (bydi (dashboard-insert-startupify-lists get-buffer)

      (wal-dashboard-get-buffer)

      (bydi-was-called dashboard-insert-startupify-lists)

      (bydi-was-called-with get-buffer "dash"))))

(ert-deftest wal-instead-use-custom-banner ()
  (let ((wal-emacs-config-default-path "/home/test"))
    (bydi ((:sometimes dashboard--image-supported-p))

      (should (equal (list :image "/home/test/assets/logo.png"
                           :text "/home/test/assets/logo.txt")
                     (wal-instead-use-custom-banner)))

      (bydi-toggle-sometimes)

      (should (equal (list :text "/home/test/assets/logo.txt")
                     (wal-instead-use-custom-banner))))))

;;; wal-visuals-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
