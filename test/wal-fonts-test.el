;;; wal-fonts-test.el --- Tests for fonts functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; These tests require a lot of mocking as there won't be an
;; initialized window system.

;;; Code:

(require 'wal-fonts nil t)

(ert-deftest test-wal/font-update ()
  (should (equal '(default) (wal/font-update :height 120 '(default)))))

(ert-deftest test-wal/select-fixed-font ()
  (with-mock completing-read (lambda (_m _) "JetBrains Mono")
    (let ((updates (call-interactively 'wal/select-fixed-font)))
      (should (equal updates '(default fixed-pitch))))))

(ert-deftest test-wal/set-fixed-font-height ()
  (let ((updates (funcall-interactively 'wal/set-fixed-font-height 110)))
    (should (equal updates '(default fixed-pitch)))
    (should (eq 110 (face-attribute 'default :height)))
    (should (eq wal/fixed-font-height 110))))

(ert-deftest test-wal/select-variable-font ()
  (with-mock completing-read (lambda (_m _) "Roboto")
    (let ((updates (call-interactively 'wal/select-variable-font)))
    (should (equal updates '(variable-pitch))))))

(ert-deftest test-wal/set-variable-font-height ()
  (let ((updates (funcall-interactively 'wal/set-variable-font-height 98)))
    (should (equal updates '(variable-pitch)))
    (should (eq 98 (face-attribute 'variable-pitch :height)))
    (should (eq wal/variable-font-height 98))))

(ert-deftest test-wal/preferred-fonts ()
  (let ((wal/preferred-fonts '("None" "This One")))
    (should (equal '("This One") (wal/preferred-fonts '("Not" "This One" "Here"))))))

(ert-deftest test-wal/fonts-candidate ()
  (with-mock-all ((wal/available-fonts . (lambda (_) '("Two" "One")))
                  (wal/preferred-fonts . (lambda (_) '("One" "Two"))))
    (should (string-equal "Two" (wal/fonts-candidate '())))
    (should (string-equal "One" (wal/fonts-candidate '() t)))))

(ert-deftest test-wal/font-lock ()
  (with-temp-buffer
    (wal/font-lock)
    (should (equal 'italic (face-attribute 'font-lock-comment-face :slant)))
    (should (equal 'bold (face-attribute 'font-lock-keyword-face :weight)))))

;; wal-fonts-test.el ends here
