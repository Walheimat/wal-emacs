;;; wal-prelude-test.el --- Test prelude package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions

;;; Code:

(require 'wal-prelude nil t)

(ert-deftest package-files ()
  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt"))

    (bydi ((:mock directory-files :return (list "." ".." file other-file)))

      (should (equal (list file) (wal-prelude-package-files))))))

(ert-deftest init-fails-if-no-init ()
  (should-error (wal-prelude-init "/tmp/no-exist.el" "/tmp") :type 'user-error))

(defvar valid-init (ert-resource-file "valid-init.txt"))

(defmacro with-bootstrap (shell-result with-contents &rest body)
  "Evaluate BODY with SHELL-RESULT in a temporary file.

If WITH-CONTENTS is t, the valid init file will be inserted in
the temporary file."
  (declare (indent 1))
  `(bydi ((:mock shell-command-to-string :with (lambda (&rest _) ,shell-result))
          append-to-file
          delete-region)
     (ert-with-temp-file bootstrap
       :text ,(when with-contents
                (with-temp-buffer
                  (insert-file-contents valid-init)
                  (buffer-string)))
       ,@body)))

(ert-deftest init-sets-up-bootstrap ()
  (with-bootstrap "test" nil
    (wal-prelude-init bootstrap wal-emacs-config-default-path)

    (bydi-was-called append-to-file)))

(ert-deftest init-does-not-set-up-for-valid-bootstrap ()
  (defvar wal-prelude--init-marker)
  (with-bootstrap "test" t

    (wal-prelude-init bootstrap wal-emacs-config-default-path)
    (bydi-was-not-called append-to-file)))

(ert-deftest init-deletes-outdated-bootstrap ()
  (with-bootstrap "best" t

    (wal-prelude-init bootstrap wal-emacs-config-default-path)
    (bydi-was-called delete-region)))

(ert-deftest init--clears ()
  (with-bootstrap "test" t

    (wal-prelude-init bootstrap wal-emacs-config-default-path t)
    (bydi-was-called delete-region)))

(ert-deftest configure-customization ()
  (let ((custom-file nil)
        (user-emacs-directory "/tmp")
        (exists nil))

    (bydi ((:mock file-exists-p :with (lambda (&rest _)
                                        (let ((before exists))
                                          (setq exists (not exists))
                                          before)))
           make-empty-file
           load)

      (wal-prelude--configure-customization)

      (should (string= custom-file "/tmp/custom.el"))

      (bydi-was-called-with make-empty-file (list "/tmp/custom.el" t))
      (bydi-was-called-with load "/tmp/custom.el"))))

(ert-deftest load-config--requires-packages ()
  (defvar wal-packages)

  (let ((wal-packages '(one two)))

    (bydi (require add-to-list wal-prelude--configure-customization)
      (wal-prelude--load-config)

      (bydi-was-called wal-prelude--configure-customization)
      (bydi-was-called-nth-with require (list 'one) 0)
      (bydi-was-called-nth-with require (list 'two) 1))))

(ert-deftest load-config--defaults-directory ()
  (defvar wal-packages)
  (defvar wal-emacs-config-build-path)

  (let ((wal-packages '(test))
        (default-directory "/tmp")
        (wal-emacs-config-build-path nil))

    (bydi (require add-to-list wal-prelude--configure-customization)

      (wal-prelude--load-config)

      (bydi-was-called-with add-to-list (list 'load-path "/tmp")))))

(ert-deftest load-config--sets-init-error-on-failure ()
  (defvar wal-packages)
  (defvar wal-prelude-init-error)

  (let ((wal-packages '(fails))
        (wal-prelude-init-error nil))

    (bydi (add-to-list
           (:mock require :with (lambda (&rest _) (error "Oops")))
           wal-prelude--configure-customization)

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

    (bydi (require
           (:mock make-temp-file :return "/tmp/package"))

      (wal-prelude--configure-cold-boot)

      (should (string= "/tmp/package" package-user-dir)))))

(ert-deftest touch--does-not-for-non-existing ()
  (let ((wal-emacs-config-default-path "/tmp/default")
        (wal-prelude--phony-build-dependencies '("test" "testing")))

    (bydi (shell-command)
      (wal-prelude--touch)

      (bydi-was-not-called shell-command))))

(ert-deftest touch--touches-existing ()
  (ert-with-temp-file touchable
    (let ((wal-emacs-config-default-path "/tmp")
          (wal-prelude--phony-build-dependencies (list touchable "testing")))

      (bydi (shell-command)
        (wal-prelude--touch)

        (bydi-was-called-n-times shell-command 1)))))

(ert-deftest maybe-tangle--tangles-for-empty-directory ()
  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:always directory-empty-p)
           make-directory
           wal-prelude-tangle-config)

      (wal-prelude--maybe-tangle)

      (bydi-was-called wal-prelude-tangle-config))))

(ert-deftest maybe-tangle--does-not-tangle ()
  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:ignore directory-empty-p)
           make-directory
           wal-prelude-tangle-config)

      (wal-prelude--maybe-tangle)

      (bydi-was-not-called wal-prelude-tangle-config)
      (bydi-was-not-called make-directory))))

(ert-deftest tangle-config--tangles-all-sources ()
  (bydi (require org-babel-tangle-file wal-prelude--touch)
    (wal-prelude-tangle-config)

    (bydi-was-called-n-times org-babel-tangle-file (length wal-packages))

    (bydi-was-called wal-prelude--touch)))

(ert-deftest bootstrap--configures-cold-boot ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config-build-path "/tmp"))

    (bydi (wal-prelude--configure-cold-boot
           wal-prelude--set-paths
           wal-prelude--maybe-tangle
           wal-prelude--load-config)

      (wal-prelude-bootstrap "/tmp" 'cold)

      (bydi-was-called wal-prelude--configure-cold-boot))))

(ert-deftest bootstrap--would-load-config ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config-build-path "/tmp"))

    (bydi (wal-prelude--set-paths
           wal-prelude--maybe-tangle
           wal-prelude--load-config)

      (wal-prelude-bootstrap "/tmp")

      (bydi-was-called wal-prelude--load-config))))

(ert-deftest bootstrap--would-tangle ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:always directory-empty-p)
           make-directory
           wal-prelude-tangle-config
           wal-prelude--set-paths)

      (wal-prelude-bootstrap "/tmp" 'plain)

      (bydi-was-called wal-prelude-tangle-config))))

(ert-deftest bootstrap--would-ensure ()
  (defvar wal-emacs-config-build-path)

  (let ((wal-emacs-config "/tmp"))

    (bydi (wal-prelude-tangle-config
           wal-prelude--set-paths
           wal-prelude--maybe-tangle
           wal-prelude--load-config
           package-initialize)

      (wal-prelude-bootstrap "/tmp" 'ensure)

      (bydi-was-called package-initialize))))

(ert-deftest bootstrap--handles-errors ()
  (defvar wal-prelude-init-error)

  (let ((wal-prelude-init-error nil))

    (bydi (wal-prelude--set-paths
           wal-prelude--configure-cold-boot
           wal-prelude--load-config
           wal-prelude--maybe-tangle
           kill-emacs
           delay-warning)

      (setq wal-prelude-init-error 'some-error)

      (wal-prelude-bootstrap "/tmp" 'cold)

      (bydi-was-called kill-emacs)

      (wal-prelude-bootstrap "/tmp")

      (bydi-was-called delay-warning))))

;;; wal-prelude-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
