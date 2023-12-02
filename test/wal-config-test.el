;;; wal-test.el --- Tests for main package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-config nil t)

(ert-deftest waw-animate ()
  (let ((wal-ascii-whale-key-frames ["testing" "resting"])
        (wal-ascii-whale-frame-index 0))

    (wal-ascii-whale-animate)

    (with-current-buffer (get-buffer-create wal-ascii-whale-buffer)
      (should (string-equal "testing" (buffer-string))))

    (should (eq 1 wal-ascii-whale-frame-index))))

(defmacro waw-with-animation (&rest body)
  "Execute BODY with animation mocked."
  (declare (indent 0))
  `(progn
     (defvar wal-config-ascii-whale)
     (let ((wal-ascii-whale-timer nil)
           (wal-ascii-whale-key-frames nil)
           (wal-config-ascii-whale nil)
           (wal-ascii-cachalot-whale-key-frames ["cachalot"])
           (wal-ascii-blue-whale-key-frames ["blue"]))

       (bydi (wal-ascii-whale-animate run-with-timer cancel-timer kill-buffer)
         ,@body))))

(ert-deftest waw--start-animation--no-op-for-timer ()
  (waw-with-animation
    (setq wal-ascii-whale-timer 'timer)

    (wal-ascii-whale--start-animation)

    (should-not wal-ascii-whale-key-frames)))

(ert-deftest waw--start-animation ()
  (waw-with-animation
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "blue"))))

(ert-deftest waw--start-animation--cachalot ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'cachalot)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "cachalot"))))

(ert-deftest waw--start-animation--blue ()
  (waw-with-animation
    (setq wal-config-ascii-whale 'blue)
    (wal-ascii-whale--start-animation)

    (should wal-ascii-whale-timer)
    (bydi-was-called wal-ascii-whale-animate)
    (should (string= (aref wal-ascii-whale-key-frames 0) "blue"))))

(ert-deftest waw--stop-animation--no-op-for-no-timer ()
  (waw-with-animation
    (wal-ascii-whale--stop-animation)

    (should-not wal-ascii-whale-timer)))

(ert-deftest waw--stop-animation--no-op-if-buffers-exist ()
  (waw-with-animation
    (with-temp-buffer
      (setq wal-ascii-whale-timer 'timer)
      (setq wal-ascii-whale-parent-buffer (current-buffer))

      (wal-ascii-whale--stop-animation))

    (should wal-ascii-whale-timer)))

(ert-deftest waw--stop-animation ()
  (waw-with-animation
    (setq wal-ascii-whale-timer 'timer)

    (wal-ascii-whale--stop-animation)

    (should-not wal-ascii-whale-timer)

    (bydi-was-called cancel-timer)
    (bydi-was-called kill-buffer)))

(ert-deftest waw-setup ()
  (bydi (wal-ascii-whale--start-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (bydi-was-called wal-ascii-whale--start-animation)

      (should (buffer-local-value 'kill-buffer-hook (current-buffer)))
      (should (buffer-local-value 'window-configuration-change-hook (current-buffer))))))

(ert-deftest waw-clean-up ()
  (bydi (posframe-delete
         wal-ascii-whale--start-animation
         wal-ascii-whale--stop-animation)
    (with-temp-buffer
      (wal-ascii-whale-setup)

      (wal-ascii-whale-clean-up)

      (bydi-was-called posframe-delete)
      (bydi-was-called wal-ascii-whale--stop-animation))))

(ert-deftest waw-poshandler ()
  (let ((result (wal-ascii-whale-poshandler `(:parent-window-left 4
                                                                  :parent-window-top 4
                                                                  :parent-window-width 8
                                                                  :posframe-width 2
                                                                  :parent-window ,(selected-window)))))

    (should (equal '(9 . 5) result))))

(ert-deftest waw-hidehandler ()
  (bydi ((:ignore get-buffer-window))

    (should (wal-ascii-whale-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest waw-display ()
  (let ((wal-ascii-whale-parent-buffer 'parent)
        (wal-ascii-whale-indirect-buffer 'indirect))

    (bydi ((:always require)
           posframe-show
           (:mock face-attribute :return "#ffffff"))

      (wal-ascii-whale-display)

      (bydi-was-called-with posframe-show (list 'indirect
                                                :accept-focus nil
                                                :border-width 12
                                                :border-color "#ffffff"
                                                :poshandler 'wal-ascii-whale-poshandler
                                                :posframe-parent-buffer 'parent
                                                :hidehandler 'wal-ascii-whale-hidehandler)))))

(ert-deftest waw--maybe-display ()
  (ert-with-test-buffer (:name "maybe-display")

    (bydi (wal-ascii-whale-setup
           wal-ascii-whale-display
           project-current
           (:mock project-buffers :return (list (current-buffer)))
           )

      (wal-ascii-whale--maybe-display (current-buffer))

      (bydi-was-called wal-ascii-whale-setup)
      (bydi-was-called wal-ascii-whale-display)

      (bydi-clear-mocks)

      (setq wal-ascii-whale-parent-buffer 'buffer)

      (wal-ascii-whale--maybe-display (current-buffer) t)

      (bydi-was-not-called wal-ascii-whale-setup)
      (bydi-was-not-called wal-ascii-whale-display)

      (wal-ascii-whale--maybe-display (current-buffer))

      (bydi-was-not-called wal-ascii-whale-setup)
      (bydi-was-not-called wal-ascii-whale-display))))

(ert-deftest waw--maybe-display--ignored-buffers ()
  (let ((wal-ascii-whale--ignored-buffers (list "test-file")))

    (bydi ((:mock buffer-file-name :return "test-file")
           wal-ascii-whale-setup
           wal-ascii-whale-display
           project-current
           (:mock project-buffers :return (list (current-buffer))))

      (wal-ascii-whale--maybe-display (current-buffer))

      (bydi-was-not-called wal-ascii-whale-setup)
      (bydi-was-not-called wal-ascii-whale-display))))

(ert-deftest waw--on-find-file ()
  (bydi wal-ascii-whale--maybe-display

    (wal-ascii-whale--on-find-file)

    (bydi-was-called wal-ascii-whale--maybe-display)))

(ert-deftest wal-describe-config-version ()
  (defvar wal--default-path)
  (let ((out '("1.0.0" "test everything" "1.0.1" "letting the world know"))
        (wal--default-path "~"))
    (bydi ((:mock shell-command-to-string :with (lambda (_) (pop out)))
           (:mock message :with bydi-rf))

      (should (equal "1.0.0: test everything" (wal-describe-config-version)))

      (let ((noninteractive nil))

        (should (equal "1.0.1: letting the world know" (wal-describe-config-version)))))))

(ert-deftest wal-show-config-diff-range ()
  (bydi ((:mock shell-command-to-string :return " testing ")
         magit-diff-range)

    (wal-show-config-diff-range)
    (bydi-was-called-with magit-diff-range (list "testing" '("--stat")))))

(ert-deftest wal-tangle-config-prompt ()
  (bydi ((:always wal-tangle-do-prompt)
         (:always y-or-n-p )
         wal-tangle-config)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)
      (bydi-was-called wal-tangle-config))))

(ert-deftest wal-tangle-config-prompt--after ()
  (bydi ((:always wal-tangle-do-prompt)
         (y-or-n-p . #'ignore)
         wal-tangle-config
         message)

    (let ((wal-tangle-do-prompt t))

      (wal-tangle-config-prompt)

      (bydi-was-called-with message "To tangle, call `wal-tangle-config'")
      (should-not wal-tangle-do-prompt)

      (bydi-clear-mocks)
      (wal-tangle-config-prompt)

      (bydi-was-called-with message "Config changed. To tangle, call `wal-tangle-config'"))))

(ert-deftest wal-config-switch-project ()
  (defvar wal--default-path)
  (let ((wal--default-path "/tmp/config"))

    (bydi (project-switch-project)

      (wal-config-switch-project)

      (bydi-was-called-with project-switch-project (list "/tmp/config")))))

(ert-deftest wal-config-lib-files ()
  (defvar wal--lib-path)
  (let ((wal--lib-path nil))

    (bydi ((:mock directory-files :return '("." ".." "/tmp/test.org" "/tmp/test-2.org")))

      (should (equal '("/tmp/test.org" "/tmp/test-2.org") (wal-config-lib-files))))))

(ert-deftest wal-config-consult-org-heading ()
  (bydi (consult-org-heading
         (:mock wal-config-lib-files :return '("/tmp/test.org" "/tmp/test-2.org")))

    (wal-config-consult-org-heading)

    (bydi-was-called-with consult-org-heading (list nil '("/tmp/test.org" "/tmp/test-2.org")))))

(ert-deftest wal-config-org-tags-view ()
  (bydi (org-tags-view
         wal-config-lib-files)

    (wal-config-org-tags-view)

    (bydi-was-called wal-config-lib-files)
    (bydi-was-called org-tags-view)

    (wal-config-org-tags-view t)

    (bydi-was-called-last-with org-tags-view (list nil wal-config--package-tag))))

(ert-deftest wal-customize-group ()
  (bydi customize-group

    (wal-customize-group)

    (bydi-was-called-with customize-group (list 'wal t))))

;;; wal-config-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
