;;; wal-terminal-test.el --- Test terminal package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-terminal nil t)

(ert-deftest test-wal/vterm-run--errors-for-unknown ()
  (should-error (wal/vterm-run "asdf") :type 'user-error))

(ert-deftest test-wal/vterm-run--displays-existing-buffer ()
  (let ((buf (get-buffer-create "*vterm-run-test")))
    (with-mock (display-buffer
                (buffer-list . (lambda (&optional _) (list buf))))

      (wal/vterm-run "test")

      (was-called-with display-buffer (list buf nil)))))

(ert-deftest test-wal/vterm-run--sends-command ()
  (with-mock ((buffer-list . #'ignore)
              vterm-mode
              vterm-send-string
              vterm-send-return
              display-buffer)
    (wal/vterm-run "test")

    (let ((buf (get-buffer "*vterm-run-test*")))

      (was-called-with display-buffer (list buf nil))
      (was-called vterm-mode)
      (was-called-with vterm-send-string "test")
      (was-called vterm-send-return))))

(ert-deftest test-wal/vterm--calls-vterm-outside-project ()
  (with-mock ((project-current . #'ignore) vterm)

    (wal/vterm)

    (was-called vterm)))

(ert-deftest test-wal/vterm--checks-project-buffers ()
  (with-temp-buffer
    (setq-local major-mode 'vterm-mode)
    (rename-buffer "VTerm: test")
    (with-mock ((project-current . #'always)
                (project-buffers . (lambda (_) (list (current-buffer))))
                switch-to-buffer)

      (wal/vterm)

      (was-called-with switch-to-buffer (list (current-buffer))))))

(ert-deftest test-wal/instead-truncate-buffer ()
  (with-mock eshell-truncate-buffer
    (wal/instead-truncate-buffer)

    (was-called eshell-truncate-buffer)))

;;; wal-terminal-test.el ends here
