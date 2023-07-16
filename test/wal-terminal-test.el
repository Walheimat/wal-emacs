;;; wal-terminal-test.el --- Test terminal package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-terminal nil t)

(ert-deftest test-wal-instead-truncate-buffer ()
  (bydi eshell-truncate-buffer
    (wal-instead-truncate-buffer)

    (bydi-was-called eshell-truncate-buffer)))

(ert-deftest wal-eshell ()
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
           eshell
           pop-to-buffer)

      (wal-eshell)
      (bydi-was-called-with pop-to-buffer (list (nth 1 buffers)))

      (kill-buffer (nth 1 buffers))
      (bydi-clear-mocks)
      (wal-eshell)
      (bydi-was-called-with eshell 3)

      (bydi-toggle-sometimes)
      (wal-eshell '(4))
      (bydi-was-called-with eshell (list '(4))))

    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; wal-terminal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
