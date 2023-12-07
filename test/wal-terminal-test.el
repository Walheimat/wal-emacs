;;; wal-terminal-test.el --- Test terminal package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-terminal nil t)

(ert-deftest wal-instead-truncate-buffer ()
  :tags '(terminal)

  (bydi eshell-truncate-buffer
    (wal-instead-truncate-buffer)

    (bydi-was-called eshell-truncate-buffer)))

(ert-deftest wal-eshell ()
  :tags '(terminal)

  (defvar eshell-buffer-name)
  (let ((eshell-buffer-name "*eshell*")
        (buffers (list (get-buffer-create "*eshell*")
                       (get-buffer-create "*eshell<1>*")
                       (get-buffer-create "*eshell<2>*")
                       (get-buffer-create "*eshell<3>*")))
        (values '("/home/test/somewhere"
                  "/home/test/projects/test/deep"
                  "/home/somewhere"
                  "/home/test/projects/other")))

    (dolist (b buffers)
      (with-current-buffer b
        (setq default-directory (pop values))))

    (bydi ((:sometimes project-current)
           (:mock project-root :return "/home/test/projects/test")
           require
           eshell
           switch-to-buffer)

      (wal-eshell)
      (bydi-was-called-with switch-to-buffer (list (nth 1 buffers)))

      (kill-buffer (nth 1 buffers))
      (bydi-clear-mocks)
      (wal-eshell)
      (bydi-was-called-with eshell (list '(4)))

      (bydi-toggle-sometimes)
      (funcall-interactively 'wal-eshell 5)
      (bydi-was-called-with eshell (list 5)))

    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest wal-vterm--calls-vterm-outside-project ()
  :tags '(terminal)

  (bydi ((:ignore project-current)
         vterm)

    (wal-vterm)

    (bydi-was-called vterm)))

(ert-deftest wal-vterm--checks-project-buffers ()
  :tags '(terminal)

  (let ((buffers nil))

    (bydi ((:always project-current)
           (:mock project-root :return "/test/project")
           (:mock project-buffers :return buffers)
           switch-to-buffer
           vterm)

      (with-temp-buffer
        (setq-local buffers (list (current-buffer))
                    major-mode 'vterm-mode
                    default-directory "/test/project/deep")

        (rename-buffer "VTerm: test")
        (wal-vterm)
        (bydi-was-called-with switch-to-buffer (list (current-buffer))))

      (bydi-clear-mocks)

      (with-temp-buffer
        (setq buffers (list (current-buffer)))

        (wal-vterm)

        (bydi-was-called-with vterm (list '(4)))))))

(ert-deftest wal-vterm--passes-on-prefix-arg ()
  :tags '(terminal)

  (bydi (vterm)

    (funcall-interactively 'wal-vterm '(4))

    (bydi-was-called-with vterm (list '(4)))))

;;; wal-terminal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
