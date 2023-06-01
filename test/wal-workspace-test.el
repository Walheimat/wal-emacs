;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest test-wal-project-switch-to-parent-project ()
  (bydi-with-mock (project-switch-project (wal-project-local-value . #'symbol-value))
    (let ((wal-project-parent-project "/tmp/parent"))

      (wal-project-switch-to-parent-project)

      (bydi-was-called-with project-switch-project (list "/tmp/parent"))

      (setq wal-project-parent-project nil)
      (bydi-clear-mocks)

      (should-error (wal-project-switch-to-parent-project) :type 'user-error))))

(ert-deftest test-wal-with-project-bounded-compilation ()
  (bydi-with-mock ((project-current . #'ignore)
                   (project-buffers . #'buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (wal-with-project-bounded-compilation fun)))))

(ert-deftest test-wal-project-command--buffer-name ()
  (let ((wal-project-current-command "test"))
    (should (string-equal (wal-project-command--buffer-name nil) "*project-test*")))

  (should (string-equal (wal-project-command--buffer-name nil) "*project-compile*")))

(ert-deftest test-wal-project-command ()
  (defvar wal-project-test-default-cmd nil)

  (let ((wal-project-commands (list 'test (make-hash-table :test 'equal)))
        (wal-project-test-default-cmd "untest")
        (wal-project-command-history nil)
        (entered-command nil))

    (bydi-with-mock ((project-current . #'always)
                     (project-root . (lambda (_) "/tmp/cmd"))
                     (project-name . (lambda (_) "Test Project"))
                     (project--value-in-dir . (lambda (&rest _) wal-project-test-default-cmd))
                     compile
                     (read-shell-command . (lambda (&rest _) entered-command)))

      (setq entered-command "test")

      (wal-project-command 'test)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'wal-project-command-history))
      (bydi-was-called-with compile "test")

      (setq entered-command "best")
      (wal-project-command 'test)
      (bydi-was-called-with compile "best")

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 0))))))

(ert-deftest test-wal-project-create-command ()
  (bydi-with-mock ((make-hash-table . (lambda (&rest _) 'hash-table)))
    (bydi-match-expansion
     (wal-project-create-command test)
     `(progn
        (defvar-local wal-project-test-default-cmd nil)
        (defun wal-project-test nil "Test the current project."
               (interactive)
               (wal-project-command 'test))
        (setq wal-project-commands (plist-put wal-project-commands 'test hash-table))
        (bind-key "t" 'wal-project-test wal-project-prefix-map)))))

(ert-deftest test-wal-project-select-command ()
  (bydi-with-mock ((completing-read . (lambda (&rest _) "test"))
                   wal-project-command)
    (call-interactively 'wal-project-select-command)

    (bydi-was-called-with wal-project-command (list 'test))))

(ert-deftest test-wal-project-consult-buffer ()
  (defvar consult-project-buffer-sources)
  (bydi-with-mock (consult-buffer)
    (let ((consult-project-buffer-sources 'testing))

      (wal-project-consult-buffer)

      (bydi-was-called-with consult-buffer (list 'testing)))))

(ert-deftest test-wal-project-magit-status ()
  (bydi-with-mock (magit-status
                   (project-root . (lambda (_) "/tmp/test"))
                   (project-current . (lambda (&rest _) (list 'vc 'Git "/tmp/test"))))

    (wal-project-magit-status)

    (bydi-was-called-with magit-status (list "/tmp/test"))))

(ert-deftest test-wal-project-magit-status--ignores-if-no-vc ()
  (ert-with-message-capture messages
    (bydi-with-mock ((project-current . (lambda (&rest _) (list 'vc nil "/tmp/test")))
                     (project-root . (lambda (&rest _) "/tmp/test"))
                     magit-status)

      (wal-project-magit-status)

      (bydi-was-not-called magit-status)

      (should (string= "Project at ’/tmp/test’ is not version-controlled\n" messages)))))

(ert-deftest test-wal-project-dired-root ()
  (bydi-with-mock (project-current (project-root . (lambda (&rest _) "/tmp/test")) dired)

    (wal-project-dired-root)

    (bydi-was-called-with dired (list "/tmp/test"))))

(ert-deftest test-wal-project--buffer-root ()
  (bydi-with-mock ((project-current . (lambda (&rest _r) '(vc Git "/tmp")))
                   project-root)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/test-buffer/file.test")

      (wal-project--buffer-root (current-buffer))

      (bydi-was-called-with project-current '(nil "/tmp/test-buffer/"))
      (bydi-was-called-with project-root (list '(vc Git "/tmp"))))

    (with-temp-buffer
      (setq dired-directory "/tmp/test-buffer/")

      (wal-project--buffer-root (current-buffer))

      (bydi-was-called-with project-current '(nil "/tmp/test-buffer/"))
      (bydi-was-called-with project-root (list '(vc Git "/tmp"))))))

(ert-deftest wal-project-local-value ()
  (bydi-with-temp-file "project"

    (bydi-with-mock ((project-current . #'always) (project-root . (lambda (_) bydi-tmp-file)))

      (with-current-buffer (find-file-noselect bydi-tmp-file)
        (setq-local major-mode 'text-mode))

      (should (equal (wal-project-local-value 'major-mode) 'text-mode)))))

;;; wal-workspace-test.el ends here
