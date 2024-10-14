;;; wal-fix-test.el --- Tests for fixing package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests custom functions.

;;; Code:

(require 'wal-fix nil t)

(defvar js-imports (ert-resource-file "js.txt"))

(ert-deftest wal-jinx--js-import-p ()
  :tags '(fix)

  (ert-with-test-buffer (:name "jinx-js")
    (insert-file-contents js-imports)
    (goto-char 0)
    (should (wal-jinx--js-import-p (point)))
    (next-line)
    (should-not (wal-jinx--js-import-p (point)))
    (next-line)
    (should-not (wal-jinx--js-import-p (point)))
    (next-line 3)
    (should (wal-jinx--js-import-p (point)))))

;;; wal-fix-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
