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

(ert-deftest wal-vterm--prefer-project ()
  :tags '(terminal)

  (defun faketerm (&rest args)
    t)

  (bydi ((:sometimes project-current)
         (:mock project-root :return "/tmp")
         (:mock project-prefixed-buffer-name :return "*test-vterm*")
         (:spy pop-to-buffer)
         (:spy faketerm)
         (:watch default-directory))

    (wal-vterm--prefer-project 'faketerm t)

    (bydi-was-called faketerm)
    (bydi-was-set-to default-directory "/tmp")

    (ert-with-test-buffer (:name "vterm")
      (rename-buffer "*test-vterm*")

      (funcall-interactively 'wal-vterm--prefer-project 'faketerm t)

      (bydi-was-called faketerm)
      (bydi-was-not-called pop-to-buffer)

      (wal-vterm--prefer-project 'faketerm)

      (bydi-was-called pop-to-buffer))

    (bydi-toggle-sometimes)

    (funcall-interactively 'wal-vterm--prefer-project 'faketerm 'argument)

    (bydi-was-called-last-with faketerm 'argument)))

;;; wal-terminal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
