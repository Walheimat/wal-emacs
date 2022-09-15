;;; test-helpers.el --- Tests for helper functions. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the various utility helpers.

;;; Code:

;; HACK: Since we don't install packages yet, require from path.
(require 'wal-func
         (expand-file-name
          "../wal/wal-func.el"
          (if load-file-name
              (file-name-directory load-file-name)
            default-directory)))


(ert-deftest test-wal/bytes-per-mb--floors ()
  (should (equal 314572 (wal/bytes-per-mb 0.3))))


(ert-deftest test-wal/maybe-intern--interns-non-symbol ()
  (should (eq 'test (wal/maybe-intern "test"))))

(ert-deftest test-wal/maybe-intern--leaves-symbols ()
  (should (eq 'test (wal/maybe-intern 'test))))


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

;;; test-helpers.el ends here
