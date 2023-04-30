;;; wal-fonts-test.el --- Tests for fonts functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-fonts nil t)

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

;;; wal-fonts-test.el ends here
