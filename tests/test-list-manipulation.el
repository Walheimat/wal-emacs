;;; test-list-maniplation.el --- Tests list manipulations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the various list manipulation functions.

;;; Code:

;; HACK: Since we don't install packages yet, require from path.
(require 'wal-func
         (expand-file-name
          "../wal/wal-func.el"
          (if load-file-name
              (file-name-directory load-file-name)
            default-directory)))

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

;;; test-list-manipulation.el ends here
