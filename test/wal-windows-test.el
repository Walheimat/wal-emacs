;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest test-wal/aw-delete-window-kill-buffer ()
  (with-default-mock aw-delete-window
    (should (equal '(window t) (wal/aw-delete-window-kill-buffer 'window)))))

(ert-deftest test-wal/instead-call-consult-buffer ()
  (with-default-mock call-interactively
    (should (equal '(consult-buffer) (wal/instead-call-consult-buffer)))))

(ert-deftest test-wal/aw-delete-other-windows ()
  (with-rf-mock delete-other-windows
    (should (equal 'window (wal/aw-delete-other-windows 'window)))))

(ert-deftest test-wal/popper-echo-transform ()
  (should (string-match "testing/help" (wal/popper-echo-transform "*helpful variable: testing*")))
  (should (string-match "testing/dc-up" (wal/popper-echo-transform "* docker-compose up --rm testing*")))
  (should (string-match "is_testing_2/logs" (wal/popper-echo-transform "* docker container logs --tail 10 is_testing_2 *")))
  (should (string-match "*unchanged*" (wal/popper-echo-transform "*unchanged*"))))

(ert-deftest test-wal/popper--spared-p ()
  (let ((wal/spared-popups '("testing")))
    (with-rf-mock buffer-name
      (should (wal/popper--spared-p "testing"))
      (should-not (wal/popper--spared-p "dying")))))

(ert-deftest test-wal/kill-some-popups ()
  (defvar popper-group-function)
  (defvar popper-buried-popup-alist '((nil . ((nil . "testing") (nil . "dying")))))
  (let ((out nil)
        (popper-group-function #'ignore)
        (wal/spared-popups '("testing")))
    (with-mock-all ((buffer-live-p . #'always)
                    (kill-some-buffers . (lambda (x) (add-to-list 'out x)))
                    (buffer-name . #'wal/rf))
      (wal/kill-some-popups)
      (should (equal '(("dying")) out)))))

;;; wal-windows-test.el ends here
