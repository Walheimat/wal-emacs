;;; wal-emacs-test.el --- Tests for Emacs package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-emacs nil t)

(ert-deftest test-wal/kmacro ()
  (with-mock-all ((kmacro-end-macro . (lambda (_) 'end))
                  (kmacro-start-macro . (lambda (_) 'start)))
    (let ((defining-kbd-macro t))
      (should (equal (wal/kmacro nil) 'end)))
    (let ((defining-kbd-macro nil))
      (should (equal (wal/kmacro nil) 'start)))))

(ert-deftest test-wal/clear-registers ()
  (let ((register-alist '((a . b))))
    (wal/clear-registers)
    (should-not register-alist)))

(ert-deftest test-wal/lighthouse ()
  (with-mock pulse-momentary-highlight-one-line (lambda (p c) (list p c))
    (with-temp-buffer
      (insert "testing")
      (goto-char (point-max))
      (should (equal (list 8 'cursor) (wal/lighthouse))))))

;;; wal-emacs-test.el ends here
