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

;;;; Garbage Collction:

(ert-deftest test-wal/bytes-per-mb--floors ()
  (should (equal 314572 (wal/bytes-per-mb 0.3))))

;;;; Helpers:

(ert-deftest test-wal/truncate--truncates ()
  (should (string-equal (wal/truncate "This is it" 7) "This...")))

(ert-deftest test-wal/truncate--truncates-without-len ()
  (should (string-equal (wal/truncate "This is it") "This ...")))

(ert-deftest test-wal/truncate--leaves-as-is-if-below ()
  (should (string-equal (wal/truncate "This is it" 24) "This is it")))

(defvar test-standard 'standard)

(ert-deftest test-wal/reset-to-standard--resets ()
  (setq test-standard 'global)
  (should (equal 'global test-standard))
  (wal/reset-to-standard 'test-standard)
  (should (equal nil test-standard)))

(require 'shell)

(ert-deftest test-wal/dead-shell-p ()
  (with-temp-buffer
    (shell-mode)
    (should (wal/dead-shell-p))))

;;; wal-func-test.el ends here
