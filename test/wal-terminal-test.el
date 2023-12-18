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

(ert-deftest wal-project-vterm ()
  (bydi ((:always project-current)
         (:mock project-root :return "/tmp")
         (:mock project-prefixed-buffer-name :return "*test-vterm*")
         (:spy pop-to-buffer)
         (:watch default-directory)
         vterm)

    (wal-project-vterm)

    (bydi-was-called vterm)
    (bydi-was-set-to default-directory "/tmp")

    (ert-with-test-buffer (:name "vterm")
      (rename-buffer "*test-vterm*")

      (funcall-interactively 'wal-project-vterm t)

      (bydi-was-called vterm)
      (bydi-was-not-called pop-to-buffer)

      (wal-project-vterm)

      (bydi-was-called pop-to-buffer))))

;;; wal-terminal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
