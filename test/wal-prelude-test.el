;;; wal-prelude-test.el --- Test prelude package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions

;;; Code:

(require 'wal-prelude nil t)

(ert-deftest package-files ()
  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt")
         (clean (lambda () (delete-directory dir t))))

    (make-directory dir)
    (make-empty-file file)
    (make-empty-file other-file)

    (condition-case nil
        (with-mock ((directory-files . (lambda (&rest _) (list "." ".." file))))

          (should (equal (list file) (wal-prelude-package-files)))

          (funcall clean))
      (error
       (funcall clean)))))

(ert-deftest init-fails-if-no-init ()
  (should-error (wal-prelude-init "/tmp/no-exist.el" "/tmp") :type 'user-error))

(ert-deftest init-sets-up-bootstrap ()
  (with-mock ((shell-command-to-string . (lambda (_cmd) "test")))
    (wal-with-temp-file "bootstrap.el"
      (wal-prelude-init wal-tmp-file wal-emacs-config-default-path)

      (with-current-buffer (find-file-noselect wal-tmp-file)
        (should (string-match "wal-prelude-bootstrap" (buffer-string)))))))

(ert-deftest init-does-not-set-up-for-valid-bootstrap ()
  (defvar wal-prelude--init-marker)

  (with-mock ((shell-command-to-string . (lambda (_cmd) "test")))
    (wal-with-temp-file "bootstrap.el"
      (let* ((hashed (base64-encode-string "test"))
             (marker (concat "\n" wal-prelude--init-marker hashed "\n")))

        (append-to-file marker nil wal-tmp-file))

      (with-mock (append-to-file)
        (wal-prelude-init wal-tmp-file wal-emacs-config-default-path)

        (was-not-called append-to-file)))))

(ert-deftest init-deletes-outdated-bootstrap ()
  (wal-with-temp-file "bootstrap.el"
    (let* ((hashed (base64-encode-string "best"))
           (marker (concat "\n" wal-prelude--init-marker hashed "\n")))

      (append-to-file marker nil wal-tmp-file)

      (wal-prelude-init wal-tmp-file wal-emacs-config-default-path)

      (with-current-buffer (find-file-noselect wal-tmp-file)
        (should-not (string-match hashed (buffer-string)))))))

(ert-deftest init--clears ()
  (wal-with-temp-file "bootstrap.el"
    (let* ((hashed (base64-encode-string "best"))
           (marker (concat "\n" wal-prelude--init-marker hashed "\n")))

      (append-to-file marker nil wal-tmp-file)

      (wal-prelude-init wal-tmp-file wal-emacs-config-default-path t)

      (with-current-buffer (find-file-noselect wal-tmp-file)
        (should-not (string-match hashed (buffer-string)))))))

(ert-deftest load-config--requires-packages ()
  (defvar wal-packages)

  (let ((wal-packages '(one two)))

    (with-mock (require add-to-list)
      (wal-prelude--load-config)

      (was-called-nth-with require (list 'one) 0)
      (was-called-nth-with require (list 'two) 1))))

(ert-deftest load-config--defaults-directory ()
  (defvar wal-packages)
  (defvar wal-emacs-config-build-path)

  (let ((wal-packages '(test))
        (default-directory "/tmp")
        (wal-emacs-config-build-path nil))

    (with-mock (require add-to-list)

      (wal-prelude--load-config)

      (was-called-with add-to-list (list 'load-path "/tmp")))))

(ert-deftest load-config--sets-init-error-on-failure ()
  (defvar wal-packages)
  (defvar wal-prelude-init-error)

  (let ((wal-packages '(fails))
        (wal-prelude-init-error nil))

    (with-mock (add-to-list (require . (lambda (&rest _) (error "Oops"))))

      (wal-prelude--load-config)

      (should (string= (error-message-string wal-prelude-init-error) "Oops"))

      (setq wal-prelude-init-error nil))))

(ert-deftest set-paths--does-that ()
  (defvar wal-emacs-config-default-path)
  (defvar wal-emacs-config-build-path)
  (defvar wal-emacs-config-lib-path)

  (let ((wal-emacs-config-default-path nil)
        (wal-emacs-config-build-path nil)
        (wal-emacs-config-lib-path nil))

    (wal-prelude--set-paths "/tmp")

    (should (string= wal-emacs-config-lib-path "/tmp/lib"))))

(ert-deftest configure-cold-boot--sets-package-user-dir ()
  (defvar package-user-dir)

  (let ((package-user-dir nil))

    (with-mock (require
                (make-temp-file . (lambda (&rest _) "/tmp/package")))

      (wal-prelude--configure-cold-boot)

      (should (string= "/tmp/package" package-user-dir)))))

(ert-deftest touch--does-not-for-non-existing ()
  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-prelude--phony-build-dependencies '("test" "testing")))

    (with-mock (shell-command)
      (wal-prelude--touch)

      (was-not-called shell-command))))

(ert-deftest touch--touches-existing ()
  (wal-with-temp-file "touchable"
    (let ((wal-emacs-config-default-path "/tmp")
          (wal-prelude--phony-build-dependencies (list wal-tmp-file "testing")))

      (with-mock (shell-command)
        (wal-prelude--touch)

        (was-called-n-times shell-command 1)))))

(ert-deftest tangle-config--tangles-all-sources ()
  (with-mock (require org-babel-tangle-file wal-prelude--touch)

    (message "current error %s" wal-prelude-init-error)

    (wal-prelude-tangle-config)

    (was-called-n-times org-babel-tangle-file (length wal-packages))

    (was-called wal-prelude--touch)))

(ert-deftest bootstrap--configures-cold-boot ()
(defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config-build-path "/tmp"))

    (with-mock (wal-prelude--configure-cold-boot
                wal-prelude--set-paths)

      (wal-prelude-bootstrap "/tmp" t t)

      (was-called wal-prelude--configure-cold-boot))))

(ert-deftest bootstrap--would-load-config ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config-build-path "/tmp"))

    (with-mock (wal-prelude--set-paths
                wal-prelude--load-config)

      (wal-prelude-bootstrap "/tmp")

      (was-called wal-prelude--load-config))))

(ert-deftest bootstrap--would-tangle ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config "/tmp"))

    (with-mock ((file-directory-p . #'always)
                (directory-empty-p . #'always)
                make-directory
                wal-prelude-tangle-config
                wal-prelude--set-paths)

      (wal-prelude-bootstrap "/tmp" t)

      (was-called wal-prelude-tangle-config))))

(ert-deftest bootstrap--would-ensure ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config "/tmp"))

    (with-mock ((file-directory-p . #'always)
                (directory-empty-p . #'always)
                make-directory
                wal-prelude-tangle-config
                wal-prelude--set-paths
                wal-prelude--load-config
                package-initialize)

      (wal-prelude-bootstrap "/tmp" nil nil t)

      (was-called package-initialize))))

(ert-deftest bootstrap--handles-errors ()
  (defvar wal-prelude-init-error)

  (let ((wal-prelude-init-error nil))

    (with-mock ((file-directory-p . #'always)
                (directory-empty-p . #'ignore)
                wal-prelude--set-paths
                wal-prelude--configure-cold-boot
                wal-prelude--load-config
                kill-emacs
                delay-warning)

      (setq wal-prelude-init-error 'some-error)

      (wal-prelude-bootstrap "/tmp" nil t)

      (was-called kill-emacs)

      (wal-prelude-bootstrap "/tmp")

      (was-called delay-warning))))

;;; wal-prelude-test.el ends here
