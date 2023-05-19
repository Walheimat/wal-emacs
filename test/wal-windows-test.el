;;; wal-windows-test.el --- Tests for windows package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for window utilities.

;;; Code:

(require 'wal-windows nil t)

(ert-deftest test-wal-tab-bar-switch-to-buffer-tab ()
  (let ((found nil))

    (with-mock ((tab-bar-get-buffer-tab . (lambda (_) found))
                tab-bar-switch-to-tab
                switch-to-buffer
                select-window
                get-buffer-window)

      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (was-called-with switch-to-buffer (list 'buffer))
      (wal-clear-mocks)
      (setq found '((name . "test-tab")))

      (wal-tab-bar-switch-to-buffer-tab 'buffer)

      (was-called-with tab-bar-switch-to-tab "test-tab")
      (was-called-with get-buffer-window (list 'buffer))
      (was-called select-window))))

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (wtb . "test-hash")))

(ert-deftest wtb--create-hash-key ()
  (with-mock ((random . (lambda () 42))
              (emacs-pid . (lambda () 1))
              (recent-keys . (lambda () 'keys))
              md5)

    (wal-tab-buffers--create-hash-key "test")
    (was-called-with md5 (list "test421keys"))))

(ert-deftest wtb--on-create--sets-cdr ()
  (with-mock ((wal-tab-buffers--create-hash-key . (lambda (_) "test")))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (wal-tab-buffers--on-create tab)

      (should (string= "test" (alist-get 'wtb tab))))))

(ert-deftest wtb--key--returns-if-set ()
  (with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
    (should (string= "test-hash" (wal-tab-buffers--key)))))

(defmacro with-tab-history (&rest body)
  "Run BODY with a clear tab history and a temp buffer."
  (declare (indent 0))
  `(with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
     (let ((wal-tab-buffers--table (make-hash-table)))
       (with-temp-buffer
         ,@body))))

(ert-deftest wtb--remember--remembers ()
  (with-tab-history

   (wal-tab-buffers--remember)

   (should-not (null (gethash (alist-get 'wtb test-tab) wal-tab-buffers--table)))))

(ert-deftest wtb--remember--inserts-once ()
  (defvar wal-tab-buffers--table)

  (with-tab-history

    (wal-tab-buffers--remember)
    (wal-tab-buffers--remember)

    (should (eq 1 (ring-length (gethash (alist-get 'wtb test-tab) wal-tab-buffers--table))))))

(ert-deftest wtb--forget--forgets ()
    (with-tab-history
      (wal-tab-buffers--remember)

      (wal-tab-buffers--forget)

      (should (eq 0 (ring-length (gethash (alist-get 'wtb test-tab) wal-tab-buffers--table))))))

(ert-deftest wtb--on-close ()
  (with-tab-history
    (wal-tab-buffers--remember)

    (should (eq 1 (length (hash-table-keys wal-tab-buffers--table))))

    (wal-tab-buffers--on-close test-tab t)

    (should (eq 1 (length (hash-table-keys wal-tab-buffers--table))))

    (wal-tab-buffers--on-close test-tab nil)

    (should (eq 0 (length (hash-table-keys wal-tab-buffers--table))))))

(ert-deftest wtb--history ()
  (with-tab-history
    (should-not (wal-tab-buffers--history))

    (wal-tab-buffers--remember)

    (should (wal-tab-buffers--history))))

(ert-deftest wtb--current-p ()
  (with-tab-history
    (wal-tab-buffers--remember)
    (should (wal-tab-buffers--current-p (current-buffer)))))

(ert-deftest wtb--has-buffers-p ()
  (with-tab-history
    (should-not (wal-tab-buffers--has-buffers-p))

    (wal-tab-buffers--remember)

    (should (wal-tab-buffers--has-buffers-p))))

;;; wal-windows-test.el ends here
