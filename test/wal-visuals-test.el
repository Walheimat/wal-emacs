;;; wal-visuals-test.el --- Tests for visuals package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test custom functionality.

;;; Code:

(require 'wal-visuals nil t)

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

(ert-deftest test-wal-with-common-ligatures ()
  (defvar wal-common-ligatures)

  (let ((wal-common-ligatures '("?")))

    (with-mock ((require . #'always) ligature-set-ligatures)

      (wal-set-ligatures 'test-mode '("!"))

      (was-called-with ligature-set-ligatures (list 'test-mode '("!" "?"))))))

;;; wal-visuals-test.el ends here
