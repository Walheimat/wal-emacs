;;; wal-terminal-test.el --- Test terminal package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-terminal nil t)

(ert-deftest test-wal/vterm-run--errors-for-unknown ()
  (should-error (wal/vterm-run "asdf") :type 'user-error))

(ert-deftest test-wal/vterm-run--displays-existing-buffer ()
  (with-mock-all ((display-buffer . #'wal/ra)
                  (buffer-list . (lambda (&optional _) '("*vterm-run-test")))
                  (buffer-name . #'wal/rf))
    (should (equal '("*vterm-run-test" nil) (wal/vterm-run "test")))))

(ert-deftest test-wal/vterm-run--sends-command ()
  (let ((out nil))
    (with-mock-all ((buffer-list . #'ignore)
                    (vterm-mode . #'ignore)
                    (vterm-send-string . (lambda (x) (add-to-list 'out x)))
                    (vterm-send-return . #'ignore)
                    (display-buffer . #'ignore))
      (wal/vterm-run "test")
      (should (equal '("test") out)))))

(ert-deftest test-wal/vterm--calls-vterm-outside-project ()
  (defvar wal/use-projectile)
  (with-mock-all ((projectile-project-p . #'ignore)
                  (project-current . #'ignore)
                  (vterm . (lambda (&optional _) 'vterm)))
    (let ((wal/use-projectile t))
      (should (equal 'vterm (wal/vterm))))
    (let ((wal/use-projectile nil))
      (should (equal 'vterm (wal/vterm))))))

(ert-deftest test-wal/vterm--checks-project-buffers ()
  (defvar wal/use-projectile)
  (let ((wal/use-projectile t))
    (with-temp-buffer
      (setq-local major-mode 'vterm-mode)
      (rename-buffer "VTerm: test")
      (with-mock-all ((projectile-project-p . #'always)
                      (projectile-project-buffers . (lambda (&optional _) (list (current-buffer))))
                      (switch-to-buffer . #'wal/ra))
        (should (equal (list (current-buffer)) (wal/vterm))))))
  (let ((wal/use-projectile nil))
    (with-temp-buffer
      (setq-local major-mode 'vterm-mode)
      (rename-buffer "VTerm: test")
      (with-mock-all ((project-current . #'always)
                      (project-buffers . (lambda (_) (list (current-buffer))))
                      (switch-to-buffer . #'wal/ra))
        (should (equal (list (current-buffer)) (wal/vterm)))))))

(ert-deftest test-wal/instead-truncate-buffer ()
  (with-mock eshell-truncate-buffer (lambda () 'truncate)
    (should (equal 'truncate (wal/instead-truncate-buffer)))))

;;; wal-terminal-test.el ends here
