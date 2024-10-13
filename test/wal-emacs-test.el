;;; wal-emacs-test.el --- Tests for Emacs package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-emacs nil t)

(ert-deftest wal-with-page-offset ()
  :tags '(emacs)

  (let ((this-command 'doc-view-goto-page)
        (wal-doc-view-page-offset 4))

    (bydi-with-mock doc-view-goto-page
      (wal-with-page-offset #'doc-view-goto-page 3)
      (bydi-was-called-with doc-view-goto-page 7)))

  (let ((this-command 'doc-view-next-page)
        (wal-doc-view-page-offset 4))

    (bydi-with-mock doc-view-goto-page
      (wal-with-page-offset #'doc-view-goto-page 3)
      (bydi-was-called-with doc-view-goto-page 3))))

;;; wal-emacs-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
