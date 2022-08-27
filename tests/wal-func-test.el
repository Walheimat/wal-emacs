;;; wal-func-test.el --- Tests for utilities. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the various utility functions used in the config.

;;; Code:

;; HACK: Since we don't install packages yet, require from path.
(require 'wal-func
         (expand-file-name
          "../wal/wal-func.el"
          (if load-file-name
              (file-name-directory load-file-name)
            default-directory)))

;;;; List tests:

(defvar test-target nil)

(ert-deftest test-wal/append--appends ()
  (setq test-target '(a b c))
  (let ((sequence '(d e f)))
    (wal/append 'test-target sequence)
    (should (equal test-target '(a b c d e f)))))

(ert-deftest test-wal/append--removes-duplicates ()
  (setq test-target '(a b c))
  (let ((sequence '(c d a)))
    (wal/append 'test-target sequence)
    (should (equal test-target '(a b c d)))))

(ert-deftest test-wal/replace-in-alist--replaces ()
  (setq test-target '((a . "whale") (b . "home")))
  (let ((values '((b . "heimat"))))
    (wal/replace-in-alist 'test-target values)
    (should (equal test-target '((a . "whale") (b . "heimat"))))))

(ert-deftest test-wal/replace-in-alist--refuses-new-keys ()
  (setq test-target '((a . "whale") (b . "home")))
  (let ((values '((b . "heimat") (c . "dolphin"))))
    (should-error (wal/replace-in-alist 'test-target values) :type 'user-error)
    (should (equal test-target '((a . "whale") (b . "home"))))))

(ert-deftest test-wal/insert-after--inserts ()
  (setq test-target '(hello my old friend))
  (let ((preceding 'hello)
        (item 'darkness))
    (wal/insert-after 'test-target preceding item)
    (should (equal test-target '(hello darkness my old friend)))))


;;; wal-func-test.el ends here
