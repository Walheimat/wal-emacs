;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest test-wal/aw-delete-window-kill-buffer ()
  (with-mock aw-delete-window
    (wal/aw-delete-window-kill-buffer 'window)

    (was-called-with aw-delete-window (list 'window t))))

(ert-deftest test-wal/instead-call-consult-buffer ()
  (with-mock call-interactively

    (wal/instead-call-consult-buffer)

    (was-called-with call-interactively (list 'consult-buffer))))

(ert-deftest test-wal/aw-delete-other-windows ()
  (with-mock delete-other-windows

    (wal/aw-delete-other-windows 'window)

    (was-called-with delete-other-windows (list 'window))))

(ert-deftest test-wal/popper-echo-transform ()
  (should (string-match "testing/help" (wal/popper-echo-transform "*helpful variable: testing*")))
  (should (string-match "testing/dc-up" (wal/popper-echo-transform "* docker-compose up --rm testing*")))
  (should (string-match "is_testing_2/logs" (wal/popper-echo-transform "* docker container logs --tail 10 is_testing_2 *")))
  (should (string-match "*unchanged*" (wal/popper-echo-transform "*unchanged*"))))

(ert-deftest test-wal/popper--spared-p ()
  (let ((wal/spared-popups '("testing")))

    (with-mock ((buffer-name . #'wal/rf))

      (should (wal/popper--spared-p "testing"))
      (should-not (wal/popper--spared-p "dying")))))

(ert-deftest test-wal/kill-some-popups ()
  (defvar popper-group-function)
  (defvar popper-buried-popup-alist '((nil . ((nil . "testing") (nil . "dying")))))
  (let ((popper-group-function #'ignore)
        (wal/spared-popups '("testing")))

    (with-mock ((buffer-live-p . #'always)
                kill-some-buffers
                (buffer-name . #'wal/rf))

      (wal/kill-some-popups)

      (was-called-with kill-some-buffers (list (list "dying"))))))

;;; wal-windows-test.el ends here
