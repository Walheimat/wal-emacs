;;; wal-test.el --- Test prelude package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions

;;; Code:

(require 'wal nil t)

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
       (shut-up ,@body))))

(ert-deftest package-files ()
  :tags '(prelude)

  (let* ((dir "/tmp/package")
         (file "/tmp/package/test.el")
         (other-file "/tmp/package/test.txt"))

    (bydi ((:mock directory-files :return (list "." ".." file other-file)))

      (should (equal (list file) (wal-package-files))))))

(ert-deftest init-fails-if-no-init ()
  :tags '(prelude)

  (should-error (wal-init "/tmp/no-exist.el" "/tmp") :type 'user-error))

(ert-deftest init-sets-up-bootstrap ()
  :tags '(prelude)

  (with-bootstrap "test" nil
    (wal-init bootstrap wal--default-path)

    (bydi-was-called append-to-file)))

(ert-deftest init-does-not-set-up-for-valid-bootstrap ()
  :tags '(prelude)

  (defvar wal--init-marker)
  (with-bootstrap "test" t

    (wal-init bootstrap wal--default-path)
    (bydi-was-not-called append-to-file)))

(ert-deftest init-deletes-outdated-bootstrap ()
  :tags '(prelude)

  (with-bootstrap "best" t

    (wal-init bootstrap wal--default-path)
    (bydi-was-called delete-region)))

(ert-deftest init--clears ()
  :tags '(prelude)

  (with-bootstrap "test" t

    (wal-init bootstrap wal--default-path t)
    (bydi-was-called delete-region)))

(ert-deftest configure-customization ()
  :tags '(prelude)

  (ert-with-temp-file file
    :text "(message \"from within\")"

    (let ((user-emacs-directory "/tmp")
          (wal--custom-file (file-name-nondirectory file)))

      (shut-up
        (ert-with-message-capture messages
          (wal-load--configure-customization)

          (should (string-match-p "from within" messages)))))))

(ert-deftest configure-customization--creates-file ()
  :tags '(prelude)

  (let ((user-emacs-directory "/tmp")
        (wal--custom-file "some-custom-file.el"))

      (shut-up
        (ert-with-message-capture messages
          (wal-load--configure-customization)))

      (should (file-exists-p "/tmp/some-custom-file.el"))

      (delete-file "/tmp/some-custom-file.el")))

(ert-deftest load-config--requires-packages ()
  :tags '(prelude)

  (defvar wal-packages)

  (let ((wal-packages '(one two)))

    (bydi (require add-to-list wal-load--configure-customization)
      (shut-up (wal-load))

      (bydi-was-called wal-load--configure-customization)
      (bydi-was-called-nth-with require (list 'one) 0)
      (bydi-was-called-nth-with require (list 'two) 1))))

(ert-deftest load-config--defaults-directory ()
  :tags '(prelude)

  (defvar wal-packages)
  (defvar wal--build-path)

  (let ((wal-packages '(test))
        (default-directory "/tmp")
        (wal--build-path nil))

    (bydi (require add-to-list wal-load--configure-customization)

      (shut-up (wal-load))

      (bydi-was-called-with add-to-list (list 'load-path "/tmp")))))

(ert-deftest load-config--sets-init-error-on-failure ()
  :tags '(prelude)

  (defvar wal-packages)
  (defvar wal-init-error)

  (let ((wal-packages '(fails))
        (wal-init-error nil))

    (bydi (add-to-list
           (:mock require :with (lambda (&rest _) (error "Oops")))
           wal-load--configure-customization)

      (shut-up (wal-load))

      (should (string= (error-message-string wal-init-error) "Oops"))

      (setq wal-init-error nil))))

(ert-deftest set-paths--does-that ()
  :tags '(prelude)

  (defvar wal--default-path)
  (defvar wal--build-path)
  (defvar wal--lib-path)

  (let ((wal--default-path nil)
        (wal--build-path nil)
        (wal--lib-path nil))

    (wal-bootstrap--set-paths "/tmp")

    (should (string= wal--lib-path "/tmp/lib"))))

(ert-deftest configure-cold-boot--sets-package-user-dir ()
  :tags '(prelude)

  (defvar package-user-dir)

  (let ((package-user-dir nil))

    (bydi (require
           (:mock make-temp-file :return "/tmp/package"))

      (shut-up (wal-bootstrap--configure-cold-boot))

      (should (string= "/tmp/package" package-user-dir)))))

(ert-deftest touch--does-not-for-non-existing ()
  :tags '(prelude)

  (let ((wal--default-path "/tmp/default")
        (wal--phony-build-dependencies '("test" "testing")))

    (bydi ((:mock shell-command-to-string :return ""))

      (shut-up (wal--touch))

      (bydi-was-not-called shell-command-to-string))))

(ert-deftest touch--touches-existing ()
  :tags '(prelude)

  (ert-with-temp-file touchable
    (let ((wal--default-path "/tmp")
          (wal--phony-build-dependencies (list touchable "testing"))
          (output ""))

      (bydi ((:mock shell-command-to-string :return output))
        (shut-up (wal--touch))

        (bydi-was-called-n-times shell-command-to-string 1)

        (setq output "some output")

        (shut-up
          (ert-with-message-capture messages
            (wal--touch)

            (should (string-match-p "Output from touching: some output\n" messages))))))))

(ert-deftest maybe-tangle--tangles-for-empty-directory ()
  :tags '(prelude)

  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:always directory-empty-p)
           make-directory
           wal-tangle-library)

      (shut-up (wal-bootstrap--maybe-tangle))

      (bydi-was-called wal-tangle-library))))

(ert-deftest maybe-tangle--does-not-tangle ()
  :tags '(prelude)

  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:ignore directory-empty-p)
           make-directory
           wal-tangle-library)

      (shut-up (wal-bootstrap--maybe-tangle))

      (bydi-was-not-called wal-tangle-library)
      (bydi-was-not-called make-directory))))

(ert-deftest tangle-config--tangles-all-sources ()
  :tags '(prelude)

  (bydi (require org-babel-tangle-file wal--touch)

    (shut-up (wal-tangle-library))

    (let ((all (+ (length wal-packages)
                  (length wal-additional-packages))))

      (bydi-was-called-n-times org-babel-tangle-file all))

    (bydi-was-called wal--touch)))

(ert-deftest bootstrap--configures-cold-boot ()
  :tags '(prelude)

  (defvar wal--build-path)

  (let ((wal--build-path "/tmp"))

    (bydi (wal-bootstrap--configure-cold-boot
           wal-bootstrap--set-paths
           wal-bootstrap--maybe-tangle
           wal-load)

      (shut-up (wal-bootstrap "/tmp" 'cold))

      (bydi-was-called wal-bootstrap--configure-cold-boot))))

(ert-deftest bootstrap--would-load-config ()
  :tags '(prelude)

  (defvar wal--build-path)

  (let ((wal--build-path "/tmp"))

    (bydi (wal-bootstrap--set-paths
           wal-bootstrap--maybe-tangle
           wal-load)

      (shut-up (wal-bootstrap "/tmp"))

      (bydi-was-called wal-load))))

(ert-deftest bootstrap--would-tangle ()
  :tags '(prelude)

  (defvar wal--build-path)

  (let ((wal-emacs-config "/tmp"))

    (bydi ((:always file-directory-p)
           (:always directory-empty-p)
           make-directory
           wal-tangle-library
           wal-bootstrap--set-paths)

      (shut-up (wal-bootstrap "/tmp" 'plain))

      (bydi-was-called wal-tangle-library))))

(ert-deftest bootstrap--would-ensure ()
  :tags '(prelude)

  (defvar wal--build-path)

  (let ((wal-emacs-config "/tmp"))

    (bydi (wal-tangle-library
           wal-bootstrap--set-paths
           wal-bootstrap--maybe-tangle
           wal-load
           wal-bootstrap--ensure-dinghy
           package-initialize)

      (shut-up (wal-bootstrap "/tmp" 'ensure))

      (bydi-was-called package-initialize)
      (bydi-was-called wal-bootstrap--ensure-dinghy))))

(ert-deftest bootstrap--handles-errors ()
  :tags '(prelude)

  (defvar wal-init-error)

  (let ((wal-init-error nil))

    (bydi (wal-bootstrap--set-paths
           wal-bootstrap--configure-cold-boot
           wal-load
           wal-bootstrap--maybe-tangle
           kill-emacs
           delay-warning)

      (setq wal-init-error 'some-error)

      (shut-up (wal-bootstrap "/tmp" 'cold))

      (bydi-was-called kill-emacs)

      (shut-up (wal-bootstrap "/tmp"))

      (bydi-was-called delay-warning))))

(ert-deftest bootstrap--would-initialize ()
  :tags '(prelude)

  (let ((wal-emacs-config "/tmp"))

    (bydi (wal-tangle-library
           wal-bootstrap--set-paths
           wal-bootstrap--maybe-tangle
           wal-load
           package-initialize)

      (shut-up (wal-bootstrap "/tmp" 'upgrade))

      (bydi-was-called package-initialize))))

(ert-deftest wal-tangle-target ()
  :tags '(prelude)

  (let ((wal--build-path "/tmp/build")
        (buffer-file-name "/tmp/test.org"))

    (should (string= (wal-tangle-target) "/tmp/build/test.el"))))

(ert-deftest wal-update ()
  :tags '(prelude user-facing)

  (let ((wal--default-path "/tmp"))

    (bydi (compile
           (:watch default-directory))

      (shut-up (wal-update))

      (bydi-was-set-to default-directory "/tmp")
      (bydi-was-called-with compile '("make update && make upgrade" nil)))))

(ert-deftest wal-upgrade ()
  :tags '(prelude)

  (let ((wal-upgrade--wait-time 0)
        (package-alist nil))

    (push (list 'a (package-desc-create :name "Test 1" :kind 'vc)) package-alist)
    (push (list 'b (package-desc-create :name "Test 2" :kind 'vc)) package-alist)

    (shut-up
      (ert-with-message-capture messages

        (bydi package-vc-upgrade-all
          (wal-upgrade)

          (bydi-was-called package-vc-upgrade-all))

        (should (string= "Upgrades did not finish, waited for a maximum of 5 seconds\n" messages)))

      (ert-with-message-capture messages
        (bydi ((:mock package-vc-upgrade-all :with (lambda ()
                                                     (run-hooks 'vc-post-command-functions)
                                                     (run-hooks 'vc-post-command-functions))))

          (wal-upgrade)

          (should (string-prefix-p "Upgraded packages in" messages)))))))

(ert-deftest wal-tangle ()
  :tags '(prelude user-facing)

  (bydi wal--compile
    (shut-up (wal-tangle))

    (bydi-was-called-with wal--compile "make tangle")))

(ert-deftest wal-show-compilation-result ()
  :tags '(prelude user-facing)

  (let ((wal--compile-buffer nil))

    (should-error (wal-show-compilation-result))

    (setq wal--compile-buffer (current-buffer))

    (bydi pop-to-buffer
      (wal-show-compilation-result)

      (bydi-was-called-with pop-to-buffer (current-buffer)))))

(ert-deftest wal-bootstrap--ensure-dinghy ()
  :tags '(prelude)

  (ert-with-temp-directory dinghy-dir
    (let ((wal--dinghy-path dinghy-dir)
          (src-dir (expand-file-name "src" dinghy-dir)))

      (make-directory src-dir)

      (bydi (package-install-file)
        (wal-bootstrap--ensure-dinghy)

        (bydi-was-called-with package-install-file (expand-file-name "src/dinghy-rope.el" dinghy-dir))))))

;;; wal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
