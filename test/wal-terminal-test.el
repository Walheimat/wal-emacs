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
  :tags '(terminal)

  (bydi ((:sometimes project-current)
         (:mock project-root :return "/tmp")
         (:mock project-prefixed-buffer-name :return "*test-vterm*")
         (:spy pop-to-buffer)
         (:mock vterm :with bydi-return-first)
         (:watch default-directory))

    (wal-project-vterm)

    (bydi-was-called vterm :clear t)
    (bydi-was-set-to default-directory "/tmp")

    (ert-with-test-buffer (:name "vterm")
      (rename-buffer "*test-vterm*")

      (funcall-interactively 'wal-project-vterm)

      (bydi-was-not-called vterm)

      (bydi-was-called pop-to-buffer :clear t)

      (funcall-interactively 'wal-project-vterm '(4))

      (bydi-was-not-called pop-to-buffer)
      (bydi-was-called vterm :clear t))

    (bydi-toggle-sometimes)

    (funcall-interactively 'wal-project-vterm)

    (bydi-was-not-called vterm)))

(ert-deftest wal-vterm-adjust-by-disabling-query-on-exit ()
  :tags '(terminal)

  (bydi ((:always get-buffer-process)
         set-process-query-on-exit-flag)

    (wal-vterm-adjust-by-disabling-query-on-exit (current-buffer))

    (bydi-was-called-with set-process-query-on-exit-flag '(... nil))))

;;; wal-terminal-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
