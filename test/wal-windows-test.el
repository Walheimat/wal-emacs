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

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (wpr . "test-hash")))

(ert-deftest wpr--create-hash-key ()
  (with-mock ((random . (lambda () 42))
              (emacs-pid . (lambda () 1))
              (recent-keys . (lambda () 'keys))
              md5)

    (wal-partial-recall--create-hash-key "test")
    (was-called-with md5 (list "test421keys"))))

(ert-deftest wpr--on-create--sets-cdr ()
  (with-mock ((wal-partial-recall--create-hash-key . (lambda (_) "test")))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (wal-partial-recall--on-create tab)

      (should (string= "test" (alist-get 'wpr tab))))))

(ert-deftest wpr--key--returns-if-set ()
  (with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
    (should (string= "test-hash" (wal-partial-recall--key)))))

(defmacro with-tab-history (&rest body)
  "Run BODY with a clear tab history and a temp buffer."
  (declare (indent 0))
  `(with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
     (let ((wal-partial-recall--table (make-hash-table)))
       (with-temp-buffer
         ,@body))))

(ert-deftest wpr--remember--remembers ()
  (with-tab-history

   (wal-partial-recall--remember)

   (should-not (null (gethash (alist-get 'wpr test-tab) wal-partial-recall--table)))))

(ert-deftest wpr--remember--inserts-once ()
  (defvar wal-partial-recall--table)

  (with-tab-history

    (wal-partial-recall--remember)
    (wal-partial-recall--remember)

    (let* ((memory (gethash (alist-get 'wpr test-tab) wal-partial-recall--table))
           (ring (wal-partial-recall--memory-ring memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest wpr--remember--extends-ring ()
  (with-tab-history
    (with-mock ((wal-partial-recall--at-capacity . #'always)
                (wal-partial-recall--should-extend-p . #'always)
                ring-extend)

      (wal-partial-recall--remember (current-buffer))
      (was-called ring-extend))))

(ert-deftest wpr--forget--forgets ()
  (with-tab-history
    (wal-partial-recall--remember)

    (wal-partial-recall--forget)

    (let* ((memory (gethash (alist-get 'wpr test-tab) wal-partial-recall--table))
           (ring (wal-partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest wpr--on-close ()
  (with-tab-history
    (wal-partial-recall--remember)

    (should (eq 1 (length (hash-table-keys wal-partial-recall--table))))

    (wal-partial-recall--on-close test-tab t)

    (should (eq 1 (length (hash-table-keys wal-partial-recall--table))))

    (wal-partial-recall--on-close test-tab nil)

    (should (eq 0 (length (hash-table-keys wal-partial-recall--table))))))

(ert-deftest wpr--history ()
  (with-tab-history
    (should-not (wal-partial-recall--moments))

    (wal-partial-recall--remember)

    (should (wal-partial-recall--moments))))

(ert-deftest wpr--current-p ()
  (with-tab-history
    (wal-partial-recall--remember)
    (should (wal-partial-recall--current-p (current-buffer)))))

(ert-deftest wpr--has-buffers-p ()
  (with-tab-history
    (should-not (wal-partial-recall--has-buffers-p))

    (wal-partial-recall--remember)

    (should (wal-partial-recall--has-buffers-p))))

(ert-deftest wpr--known-buffer-p ()
  (with-tab-history
    (wal-partial-recall--remember)

    (should (wal-partial-recall--known-buffer-p (current-buffer)))))

(ert-deftest wpr--memory-buffer-p ()
  (with-tab-history
    (wal-partial-recall--remember)

    (let ((memory (gethash (alist-get 'wpr test-tab) wal-partial-recall--table)))

      (should (wal-partial-recall--memory-buffer-p memory (current-buffer))))))

(ert-deftest wpr-moment-buffer-p ()
  (with-tab-history
    (wal-partial-recall--remember)

    (let* ((memory (gethash (alist-get 'wpr test-tab) wal-partial-recall--table))
           (ring (wal-partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (wal-partial-recall--moment-buffer-p moment (current-buffer))))))

(ert-deftest wpr--maybe-remember ()
  (with-tab-history
    (with-mock (wal-partial-recall--remember
                (buffer-live-p . #'always)
                (wal-partial-recall--known-buffer-p . #'ignore))

      (wal-partial-recall--maybe-remember (current-buffer))
      (was-called wal-partial-recall--remember))))

(ert-deftest wpr--maybe-remember--reclaims ()
  (with-tab-history
    (with-mock (wal-partial-recall--remember
                wal-partial-recall--maybe-reclaim
                (buffer-live-p . #'always)
                (wal-partial-recall--known-buffer-p . #'always))

      (wal-partial-recall--maybe-remember (current-buffer))
      (was-called wal-partial-recall--maybe-reclaim))))

(ert-deftest wpr--maybe-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (wal-partial-recall-reclaim-threshold 0))
    (with-tab-history
      (with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))
        (wal-partial-recall--remember)

        (with-mock ((wal-partial-recall-current . (lambda () 'other))
                    wal-partial-recall--remember)

          (wal-partial-recall--maybe-reclaim (current-buffer))

          (was-called wal-partial-recall--remember))))))

(ert-deftest wpr--maybe-reclaim--no-op-for-same ()
  (with-tab-history
    (wal-partial-recall--remember)

    (with-mock (wal-partial-recall--remember)

        (wal-partial-recall--maybe-reclaim (current-buffer))

        (was-not-called wal-partial-recall--remember))))

(ert-deftest wpr--should-extend-p ()
  (let ((seconds '(10 11 12))
        (wal-partial-recall-limit 1)
        (wal-partial-recall-threshold 2))

    (with-tab-history
      (with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))

        (wal-partial-recall--remember)

        (let ((memory (wal-partial-recall--get-or-create-memory (wal-partial-recall--key))))

          (should (wal-partial-recall--should-extend-p memory))
          (should-not (wal-partial-recall--should-extend-p memory)))))))

(ert-deftest wpr-recall-owner ()
  (with-tab-history
    (wal-partial-recall--remember)

    (let ((memory (wal-partial-recall--get-or-create-memory (wal-partial-recall--key))))

      (should (eq memory (wal-partial-recall-owner))))))

(ert-deftest wpr--on-buffer-list-update--cancels-running-timer ()
  (let ((wal-partial-recall--timer nil))

    (with-mock ((buffer-file-name . #'always)
                (wal-partial-recall--known-buffer-p . #'ignore)
                cancel-timer
                run-at-time)

      (setq wal-partial-recall--timer 'timer)
      (wal-partial-recall--on-buffer-list-update)

      (was-called cancel-timer)

      (was-called run-at-time))))

(ert-deftest wpr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (with-mock (wal-partial-recall--on-close)

      (wal-partial-recall--on-frame-delete 'frame)

      (was-called-n-times wal-partial-recall--on-close 2))))

;;; wal-windows-test.el ends here
