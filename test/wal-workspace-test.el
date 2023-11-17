;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest wal-project-switch-to-parent-project ()
  (bydi (project-switch-project (:mock wal-project-local-value :with symbol-value))
    (let ((wal-project-parent-project "/tmp/parent"))

      (wal-project-switch-to-parent-project)

      (bydi-was-called-with project-switch-project (list "/tmp/parent"))

      (setq wal-project-parent-project nil)
      (bydi-clear-mocks)

      (should-error (wal-project-switch-to-parent-project) :type 'user-error))))

(ert-deftest wal-project--with-bounded-compilation ()
  (bydi ((:always project-current)
         (:mock project-buffers :with buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (wal-project--with-bounded-compilation fun)))))

(ert-deftest wal-project--with-bounded-compilation--ignored-outside-project ()
  (bydi ((:ignore project-current))

    (let ((fun #'always))

      (should (wal-project--with-bounded-compilation fun)))))

(ert-deftest wal-project-command--buffer-name ()
  (let ((wal-project-current-command "test"))
    (should (string-equal (wal-project-command--buffer-name nil) "*project-test*")))

  (should (string-equal (wal-project-command--buffer-name nil) "*project-compile*")))

(ert-deftest wal-project-command ()
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

      (wal-project-command 'test t)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'wal-project-command-history))
      (bydi-was-called-with compile '("test" nil))

      (setq entered-command "best")
      (wal-project-command 'test t)
      (bydi-was-called-with compile '("best" nil))

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)) 0))))))

(ert-deftest wal-project-command--only-inserted-once ()
  (let ((wal-project-commands (list 'test (make-hash-table :test 'equal)))
        (wal-project-command-history nil)
        (entered-command "test"))

    (bydi ((:always project-current)
p           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return wal-project-test-default-cmd)
           compile
           (:mock read-shell-command :return entered-command))

      (wal-project-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get wal-project-commands 'test)))))
      (wal-project-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get wal-project-commands 'test))))))))

(ert-deftest project-command--history--inserts-multiple ()
  (let ((wal-project-commands (list 'test (make-hash-table :test 'equal)))
        (wal-project-command-history nil)
        (wal-project-test-default-cmd '("make test" "test make")))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return wal-project-test-default-cmd))

      (let ((history (wal-project-command--history 'test)))

        (should (string= "make test" (ring-ref history 0)))
        (should (string= "test make" (ring-ref history 1)))))))

(ert-deftest project-command--valid-default-p ()
  (should (wal-project-command--valid-default-p "test"))
  (should (wal-project-command--valid-default-p '("test" "make")))
  (should-not (wal-project-command--valid-default-p '("test" make))))

(ert-deftest wal-project-command--update-history ()
  (let ((fake-history (make-ring 3))
        (compile-command "make help"))

    (ring-insert fake-history "make test")

    (bydi ((:mock wal-project-command--history :return fake-history))

      (wal-project-command--update-history "make new")

      (should (equal (ring-elements fake-history)
                     '("make test")))

      (setq compile-command "make test")

      (wal-project-command--update-history "make new")

      (should (equal (ring-elements fake-history)
                     '("make new"))))))

(ert-deftest wal-project-create-command ()
  (bydi ((:mock make-hash-table :return 'hash-table))
    (bydi-match-expansion
     (wal-project-create-command test)
     '(progn
        (defvar-local wal-project-test-default-cmd nil "Default for `wal-project-test'.")
        (defun wal-project-test (&optional arg) "Test the current project.\n\nSee `wal-project-command' for behavior of ARG."
               (interactive "P")
               (wal-project-command 'test arg))
        (setq wal-project-commands (plist-put wal-project-commands 'test hash-table))
        (bind-key "t" 'wal-project-test wal-project-prefix-map)
        (put 'wal-project-test-default-cmd 'safe-local-variable #'wal-project-command--valid-default-p)))
    (bydi-match-expansion
     (wal-project-create-command test :key "o" :default "make all")
     '(progn
        (defvar-local wal-project-test-default-cmd "make all" "Default for `wal-project-test'.")
        (defun wal-project-test (&optional arg) "Test the current project.\n\nSee `wal-project-command' for behavior of ARG."
               (interactive "P")
               (wal-project-command 'test arg))
        (setq wal-project-commands (plist-put wal-project-commands 'test hash-table))
        (bind-key "o" 'wal-project-test wal-project-prefix-map)
        (put 'wal-project-test-default-cmd 'safe-local-variable #'wal-project-command--valid-default-p)))))

(ert-deftest wal-project-select-command ()
  (bydi ((:mock completing-read :return "test")
         wal-project-command)
    (call-interactively 'wal-project-select-command)

    (bydi-was-called-with wal-project-command (list 'test))))

(ert-deftest wal-project-consult-buffer ()
  (defvar consult-project-buffer-sources)
  (bydi (consult-buffer)
    (let ((consult-project-buffer-sources 'testing))

      (wal-project-consult-buffer)

      (bydi-was-called-with consult-buffer (list 'testing)))))

(ert-deftest wal-project-magit-status ()
  (bydi (magit-status
         (:mock project-root :return "/tmp/test")
         (:mock project-current :return (list 'vc 'Git "/tmp/test")))

    (wal-project-magit-status)

    (bydi-was-called-with magit-status (list "/tmp/test"))))

(ert-deftest wal-project-magit-status--ignores-if-no-vc ()
  (ert-with-message-capture messages
    (bydi ((:mock project-current :return (list 'vc nil "/tmp/test"))
           (:mock project-root :return "/tmp/test")
           magit-status)

      (wal-project-magit-status)

      (bydi-was-not-called magit-status)

      (should (string= "Project at ’/tmp/test’ is not version-controlled\n" messages)))))

(ert-deftest wal-project-dired-root ()
  (bydi (project-current (:mock project-root :return "/tmp/test") dired)

    (wal-project-dired-root)

    (bydi-was-called-with dired (list "/tmp/test"))))

(ert-deftest wal-project--buffer-root ()
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
  (ert-with-temp-file project

    (bydi ((:always project-current)
           (:mock project-root :return project)
           (:mock project--value-in-dir :return 'text-mode))

      (should (equal (wal-project-local-value 'major-mode) 'text-mode)))))

(ert-deftest wal-project-find-in-here ()
  (let ((default-directory "/tmp/test"))

    (bydi ((:mock project-current :return 'project)
           project-find-file-in)

      (funcall-interactively #'wal-project-find-in-here t)

      (bydi-was-called-with project-find-file-in '(nil ("/tmp/test") project t)))))

(ert-deftest wal-project-switch-to-tasks ()
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
