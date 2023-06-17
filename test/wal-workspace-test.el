;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest test-wal-project-switch-to-parent-project ()
  (bydi (project-switch-project (:mock wal-project-local-value :with symbol-value))
    (let ((wal-project-parent-project "/tmp/parent"))

      (wal-project-switch-to-parent-project)

      (bydi-was-called-with project-switch-project (list "/tmp/parent"))

      (setq wal-project-parent-project nil)
      (bydi-clear-mocks)

      (should-error (wal-project-switch-to-parent-project) :type 'user-error))))

(ert-deftest test-wal-with-project-bounded-compilation ()
  (bydi ((:ignore project-current )
         (:mock project-buffers :with buffer-list))

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

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return wal-project-test-default-cmd)
           compile
           (:mock read-shell-command :return entered-command))

      (setq entered-command "test")

      (wal-project-command 'test)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'wal-project-command-history))
      (bydi-was-called-with compile "test")

      (setq entered-command "best")
      (wal-project-command 'test)
      (bydi-was-called-with compile "best")

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 0))))))

(ert-deftest wal-project-command--only-inserted-once ()
  (let ((wal-project-commands (list 'test (make-hash-table :test 'equal)))
        (wal-project-command-history nil)
        (entered-command "test"))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return wal-project-test-default-cmd)
           compile
           (:mock read-shell-command :return entered-command))

      (wal-project-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)))))
      (wal-project-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get wal-project-commands 'test))))))))

(ert-deftest test-wal-project-create-command ()
  (bydi ((:mock make-hash-table :return 'hash-table))
    (bydi-match-expansion
     (wal-project-create-command test)
     '(progn
        (defvar-local wal-project-test-default-cmd nil)
        (defun wal-project-test nil "Test the current project."
               (interactive)
               (wal-project-command 'test))
        (setq wal-project-commands (plist-put wal-project-commands 'test hash-table))
        (bind-key "t" 'wal-project-test wal-project-prefix-map)
        (put 'wal-project-test-default-cmd 'safe-local-variable #'stringp)))))

(ert-deftest test-wal-project-select-command ()
  (bydi ((:mock completing-read :return "test")
         wal-project-command)
    (call-interactively 'wal-project-select-command)

    (bydi-was-called-with wal-project-command (list 'test))))

(ert-deftest test-wal-project-consult-buffer ()
  (defvar consult-project-buffer-sources)
  (bydi (consult-buffer)
    (let ((consult-project-buffer-sources 'testing))

      (wal-project-consult-buffer)

      (bydi-was-called-with consult-buffer (list 'testing)))))

(ert-deftest test-wal-project-magit-status ()
  (bydi (magit-status
         (:mock project-root :return "/tmp/test")
         (:mock project-current :return (list 'vc 'Git "/tmp/test")))

    (wal-project-magit-status)

    (bydi-was-called-with magit-status (list "/tmp/test"))))

(ert-deftest test-wal-project-magit-status--ignores-if-no-vc ()
  (ert-with-message-capture messages
    (bydi ((:mock project-current :return (list 'vc nil "/tmp/test"))
           (:mock project-root :return "/tmp/test")
           magit-status)

      (wal-project-magit-status)

      (bydi-was-not-called magit-status)

      (should (string= "Project at ’/tmp/test’ is not version-controlled\n" messages)))))

(ert-deftest test-wal-project-dired-root ()
  (bydi (project-current (:mock project-root :return "/tmp/test") dired)

    (wal-project-dired-root)

    (bydi-was-called-with dired (list "/tmp/test"))))

(ert-deftest test-wal-project--buffer-root ()
  (bydi ((:mock project-current :return '(vc Git "/tmp"))
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

    (bydi ((:always project-current) (:mock project-root :return bydi-tmp-file))

      (with-current-buffer (find-file-noselect bydi-tmp-file)
        (setq-local major-mode 'text-mode))

      (should (equal (wal-project-local-value 'major-mode) 'text-mode)))))

;;; wal-workspace-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
