;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest wal-project-switch-to-parent-project ()
  :tags '(workspace)

  (bydi (project-switch-project (:mock wal-project-local-value :with symbol-value))
    (let ((wal-project-parent-project "/tmp/parent"))

      (wal-project-switch-to-parent-project)

      (bydi-was-called-with project-switch-project (list "/tmp/parent"))

      (setq wal-project-parent-project nil)
      (bydi-clear-mocks)

      (should-error (wal-project-switch-to-parent-project) :type 'user-error))))

(ert-deftest wal-project-consult-buffer ()
  :tags '(workspace)

  (defvar consult-project-buffer-sources)
  (bydi (consult-buffer)
    (let ((consult-project-buffer-sources 'testing))

      (wal-project-consult-buffer)

      (bydi-was-called-with consult-buffer (list 'testing)))))

(ert-deftest wal-project-magit-status ()
  :tags '(workspace)

  (bydi (magit-status
         (:mock project-root :return "/tmp/test")
         (:mock project-current :return (list 'vc 'Git "/tmp/test")))

    (wal-project-magit-status)

    (bydi-was-called-with magit-status (list "/tmp/test"))))

(ert-deftest wal-project-magit-status--ignores-if-no-vc ()
  :tags '(workspace)

  (shut-up
    (ert-with-message-capture messages
      (bydi ((:mock project-current :return (list 'vc nil "/tmp/test"))
             (:mock project-root :return "/tmp/test")
             magit-status)

        (wal-project-magit-status)

        (bydi-was-not-called magit-status)

        (should (string= "Project at ’/tmp/test’ is not version-controlled\n" messages))))))

(ert-deftest wal-project-dired-root ()
  :tags '(workspace)

  (bydi (project-current (:mock project-root :return "/tmp/test") dired)

    (wal-project-dired-root)

    (bydi-was-called-with dired (list "/tmp/test"))))

(ert-deftest wal-project--buffer-root ()
  :tags '(workspace)

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
  :tags '(workspace)

  (ert-with-temp-file project

    (bydi ((:always project-current)
           (:mock project-root :return project)
           (:mock project--value-in-dir :return 'text-mode))

      (should (equal (wal-project-local-value 'major-mode) 'text-mode)))))

(ert-deftest wal-project-find-in-here ()
  :tags '(workspace)

  (let ((default-directory "/tmp/test"))

    (bydi ((:mock project-current :return 'project)
           project-find-file-in)

      (funcall-interactively #'wal-project-find-in-here t)

      (bydi-was-called-with project-find-file-in '(nil ("/tmp/test") project t)))))

(ert-deftest wal-project-switch-to-tasks ()
  :tags '(workspace)

  (let ((marker nil))

    (bydi ((:mock wal-org-capture--find-project-tasks-heading :return marker)
           switch-to-buffer)

      (with-temp-buffer
        (setq marker (point-marker))
        (wal-project-switch-to-tasks)
        (bydi-was-called-with switch-to-buffer (list (current-buffer)))))))

;;; wal-workspace-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
