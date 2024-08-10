;;; wal-test.el --- Tests for main package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-config nil t)

(ert-deftest animation-animate ()
  :tags '(config)

  (let ((wal-config-animation-key-frames ["testing" "resting"])
        (wal-config-animation-frame-index 0))

    (wal-config-animation-animate)

    (with-current-buffer (get-buffer-create wal-config-animation-buffer)
      (should (string-equal "testing" (buffer-string))))

    (should (eq 1 wal-config-animation-frame-index))))

(defmacro animation-with-animation (&rest body)
  "Execute BODY with animation mocked."
  (declare (indent 0))
  `(progn
     (defvar wal-config-animation-type)
     (let ((wal-config-animation-timer nil)
           (wal-config-animation-key-frames nil)
           (wal-config-animation-type nil)
           (wal-config-animation--cachalot-key-frames ["cachalot"])
           (wal-config-animation--blue-whale-key-frames ["blue"]))

       (bydi (wal-config-animation-animate run-with-timer cancel-timer kill-buffer)
         ,@body))))

(ert-deftest animation--start-animation--no-op-for-timer ()
  :tags '(config)

  (animation-with-animation
    (setq wal-config-animation-timer 'timer)

    (wal-config-animation--start-animation)

    (should-not wal-config-animation-key-frames)))

(ert-deftest animation--start-animation ()
  :tags '(config)

  (animation-with-animation
    (wal-config-animation--start-animation)

    (should wal-config-animation-timer)
    (bydi-was-called wal-config-animation-animate)
    (should (string= (aref wal-config-animation-key-frames 0) "blue"))))

(ert-deftest animation--start-animation--cachalot ()
  :tags '(config)

  (animation-with-animation
    (setq wal-config-animation-type 'cachalot)
    (wal-config-animation--start-animation)

    (should wal-config-animation-timer)
    (bydi-was-called wal-config-animation-animate)
    (should (string= (aref wal-config-animation-key-frames 0) "cachalot"))))

(ert-deftest animation--start-animation--blue ()
  :tags '(config)

  (animation-with-animation
    (setq wal-config-animation-type 'blue)
    (wal-config-animation--start-animation)

    (should wal-config-animation-timer)
    (bydi-was-called wal-config-animation-animate)
    (should (string= (aref wal-config-animation-key-frames 0) "blue"))))

(ert-deftest animation--stop-animation--no-op-for-no-timer ()
  :tags '(config)

  (animation-with-animation
    (wal-config-animation--stop-animation)

    (should-not wal-config-animation-timer)))

(ert-deftest animation--stop-animation--no-op-if-buffers-exist ()
  :tags '(config)

  (animation-with-animation
    (with-temp-buffer
      (setq wal-config-animation-timer 'timer)
      (setq wal-config-animation-parent-buffer (current-buffer))

      (wal-config-animation--stop-animation))

    (should wal-config-animation-timer)))

(ert-deftest animation--stop-animation ()
  :tags '(config)

  (animation-with-animation
    (setq wal-config-animation-timer 'timer)

    (wal-config-animation--stop-animation)

    (should-not wal-config-animation-timer)

    (bydi-was-called cancel-timer)
    (bydi-was-called kill-buffer)))

(ert-deftest animation-setup ()
  :tags '(config)

  (bydi (wal-config-animation--start-animation)
    (with-temp-buffer
      (wal-config-animation-setup)

      (bydi-was-called wal-config-animation--start-animation)

      (should (buffer-local-value 'kill-buffer-hook (current-buffer)))
      (should (buffer-local-value 'window-configuration-change-hook (current-buffer)))
      (should (buffer-local-value 'wal-fonts-updated-hook (current-buffer))))))

(ert-deftest animation-clean-up ()
  :tags '(config)

  (bydi (posframe-delete
         wal-config-animation--start-animation
         wal-config-animation--stop-animation)
    (with-temp-buffer
      (wal-config-animation-setup)

      (wal-config-animation-clean-up)

      (bydi-was-called posframe-delete)
      (bydi-was-called wal-config-animation--stop-animation))))

(ert-deftest animation-poshandler ()
  :tags '(config)

  (let ((result (wal-config-animation-poshandler `(:parent-window-left 4
                                                                  :parent-window-top 4
                                                                  :parent-window-width 8
                                                                  :posframe-width 2
                                                                  :parent-window ,(selected-window)))))

    (should (equal '(9 . 5) result))))

(ert-deftest animation-hidehandler ()
  :tags '(config)

  (bydi ((:ignore get-buffer-window))

    (should (wal-config-animation-hidehandler '(:posframe-parent-buffer '(nil nil))))))

(ert-deftest animation-display ()
  :tags '(config)

  (let ((wal-config-animation-parent-buffer 'parent)
        (wal-config-animation-indirect-buffer 'indirect)
        (wal-transparency 50))

    (bydi ((:always require)
           posframe-show
           modify-frame-parameters
           (:mock wal-transparency--param :return 'alpha-background)
           (:mock face-attribute :return "#ffffff"))

      (wal-config-animation-display)

      (bydi-was-called-with posframe-show (list 'indirect
                                                :accept-focus nil
                                                :poshandler 'wal-config-animation-poshandler
                                                :posframe-parent-buffer 'parent
                                                :hidehandler 'wal-config-animation-hidehandler))
      (bydi-was-called-with modify-frame-parameters '(... ((alpha-background . 50)))))))

(ert-deftest animation--reset ()
  :tags '(config)

  (bydi (wal-config-animation--set-font-height
         wal-config-animation-display)

    (wal-config-animation--reset)

    (bydi-was-called wal-config-animation--set-font-height)
    (bydi-was-called wal-config-animation-display)))

(ert-deftest animation--maybe-display ()
  :tags '(config)

  (ert-with-test-buffer (:name "maybe-display")

    (bydi (wal-config-animation-setup
           wal-config-animation-display
           project-current
           (:mock project-buffers :return (list (current-buffer)))
           )

      (wal-config-animation--maybe-display (current-buffer))

      (bydi-was-called wal-config-animation-setup)
      (bydi-was-called wal-config-animation-display)

      (bydi-clear-mocks)

      (setq wal-config-animation-parent-buffer 'buffer)

      (wal-config-animation--maybe-display (current-buffer) t)

      (bydi-was-not-called wal-config-animation-setup)
      (bydi-was-not-called wal-config-animation-display)

      (wal-config-animation--maybe-display (current-buffer))

      (bydi-was-not-called wal-config-animation-setup)
      (bydi-was-not-called wal-config-animation-display))))

(ert-deftest animation--maybe-display--ignored-buffers ()
  :tags '(config)

  (let ((wal-config-animation--ignored-buffers (list "test-file")))

    (bydi ((:mock buffer-file-name :return "test-file")
           wal-config-animation-setup
           wal-config-animation-display
           project-current
           (:mock project-buffers :return (list (current-buffer))))

      (wal-config-animation--maybe-display (current-buffer))

      (bydi-was-not-called wal-config-animation-setup)
      (bydi-was-not-called wal-config-animation-display))))

(ert-deftest animation--on-find-file ()
  :tags '(config)

  (bydi wal-config-animation--maybe-display

    (wal-config-animation--on-find-file)

    (bydi-was-called wal-config-animation--maybe-display)))

(ert-deftest wal-describe-config-version ()
  :tags '(config user-facing)

  (defvar wal-default-path)
  (let ((out '("1.0.0" "test everything" "1.0.1" "letting the world know"))
        (wal-default-path "~"))
    (bydi ((:mock shell-command-to-string :with (lambda (_) (pop out)))
           (:mock message :with bydi-rf))

      (should (equal "1.0.0: test everything" (wal-describe-config-version)))

      (let ((noninteractive nil))

        (should (equal "1.0.1: letting the world know" (wal-describe-config-version)))))))

(ert-deftest wal-show-config-diff-range ()
  :tags '(config user-facing)

  (bydi ((:mock shell-command-to-string :return " testing ")
         magit-diff-range)

    (wal-show-config-diff-range)
    (bydi-was-called-with magit-diff-range (list "testing" '("--stat")))))

(ert-deftest wal-config-switch-project ()
  :tags '(config user-facing)

  (defvar wal-default-path)
  (let ((wal-default-path "/tmp/config"))

    (bydi (project-switch-project)

      (wal-config-switch-project)

      (bydi-was-called-with project-switch-project (list "/tmp/config")))))

(ert-deftest wal-config-lib-files ()
  :tags '(config)

  (defvar wal-lib-path)
  (let ((wal-lib-path nil))

    (bydi ((:mock directory-files :return '("." ".." "/tmp/test.org" "/tmp/test-2.org")))

      (should (equal '("/tmp/test.org" "/tmp/test-2.org") (wal-config-lib-files))))))

(ert-deftest wal-config-consult-org-heading ()
  :tags '(config user-facing)

  (bydi (consult-org-heading
         (:mock wal-config-lib-files :return '("/tmp/test.org" "/tmp/test-2.org")))

    (wal-config-consult-org-heading)

    (bydi-was-called-with consult-org-heading (list nil '("/tmp/test.org" "/tmp/test-2.org")))))

(ert-deftest wal-customize-group ()
  :tags '(config)

  (bydi customize-group

    (wal-customize-group)

    (bydi-was-called-with customize-group (list 'wal t))))

;;; wal-config-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
