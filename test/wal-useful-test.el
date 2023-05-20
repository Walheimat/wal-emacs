;;; wal-useful-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'wal-useful nil t)

(ert-deftest test-wal-modern-emacs-p ()
  (let ((emacs-major-version 30))
    (should (wal-modern-emacs-p 30)))

  (let ((emacs-major-version 29))
    (should (wal-modern-emacs-p)))

  (let ((emacs-major-version 28))
    (should (wal-modern-emacs-p)))

  (let ((emacs-major-version 27))
    (should-not (wal-modern-emacs-p))))

(ert-deftest test-wal-modern-emacs-p--errors-for-bad-arguments ()
  (should-error (wal-modern-emacs-p 27) :type 'user-error)
  (should-error (wal-modern-emacs-p "testing") :type 'user-error))

(ert-deftest test-wal-create-non-existent-directory ()
  (let ((temp-dir "/tmp/some-other/dir/"))

    (with-mock ((file-name-directory . (lambda (&rest _r) temp-dir))
                (y-or-n-p . #'always)
                make-directory)

      (wal-create-non-existent-directory)

      (was-called-with make-directory (list temp-dir t)))))

(ert-deftest test-wal-create-non-existent-directory--aborts ()
  (let ((temp-dir "/tmp/some-other/dir/"))

    (make-directory temp-dir t)

    (with-mock ((file-name-directory . (lambda (&rest _r) temp-dir)))

      (should-not (wal-create-non-existent-directory)))

    (delete-directory temp-dir)))

(ert-deftest test-wal-display-buffer--condition--passes-strings ()
  (should (string-equal "testing" (wal-display-buffer--condition "testing"))))

(ert-deftest test-wal-display-buffer--condition--considers-symbols-major-modes ()
  (should (equal '(major-mode . test-mode) (wal-display-buffer--condition 'test-mode))))

(ert-deftest test-wal-display-buffer--condition--errors-for-unsupported-types ()
  (should-error (wal-display-buffer--condition '(hello world)) :type 'user-error))

(ert-deftest wal-display-buffer-same-place-or-nearby ()
  (let ((display-buffer-alist '()))

    (wal-display-buffer-same-place-or-nearby 'test-mode :side 'top :loose nil :no-other t :height 12)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-in-side-window display-buffer-in-direction)
                     (side . top)
					 (direction . right)
					 (window-width)
                     (window-height . 12)
					 (dedicated . t)
                     (window-parameters . ((no-other-window . t))))))

    (setq display-buffer-list '())

    (wal-display-buffer-same-place-or-nearby 'test-mode :loose t :direction 'below :width 0.4)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-in-direction display-buffer-in-side-window)
                     (side . bottom)
					 (direction . below)
					 (window-width . 0.4)
					 (window-height)
					 (dedicated)
                     (window-parameters . ((no-other-window))))))))

(ert-deftest test-wal-display-buffer-use-some-frame--with-display-p ()
  (let ((frame nil)
        (params nil))
    (with-mock ((selected-frame . (lambda () frame))
                get-lru-window
                (frame-parameters . (lambda (_) params)))


      (setq frame 1)
      (setq params '((display . t)))

      (should (wal-display-buffer-use-some-frame--with-display-p 0))
      (should-not (wal-display-buffer-use-some-frame--with-display-p 1))

      (setq params '((display . nil)))

      (should-not (wal-display-buffer-use-some-frame--with-display-p 0)))))

(ert-deftest wal-display-buffer-same-place-or-faraway ()
  (let ((display-buffer-alist '()))

    (wal-display-buffer-same-place-or-faraway 'test-mode)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window
                      display-buffer-reuse-mode-window
                      display-buffer-use-some-frame
					  display-buffer-pop-up-window)
                     (frame-predicate . wal-display-buffer-use-some-frame--with-display-p)
                     (inhibit-switch-frame . t)
					 (window-width)
					 (window-height)
					 (dedicated)
					 (window-parameters . ((no-other-window))))))

	(setq display-buffer-list '())

    (wal-display-buffer-same-place-or-faraway 'test-mode :bottom t)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window
                      display-buffer-reuse-mode-window
					  display-buffer-at-bottom
                      display-buffer-use-some-frame
					  display-buffer-pop-up-window)
                     (frame-predicate . wal-display-buffer-use-some-frame--with-display-p)
                     (inhibit-switch-frame . t)
					 (window-width)
					 (window-height)
					 (dedicated)
					 (window-parameters . ((no-other-window))))))))

(ert-deftest wal-kill-some-file-buffers ()
  (wal-with-temp-file "to-be-killed"
    (with-mock kill-buffer-ask

      (find-file-noselect wal-tmp-file)

      (get-buffer-create "killer-buffer")

      (wal-kill-some-file-buffers)

      (was-called-with kill-buffer-ask (list (get-buffer "to-be-killed"))))))

(defvar useful-killing (ert-resource-file "killing.txt"))

(ert-deftest test-wal-kill-ring-save-buffer ()
  (with-temp-buffer
    (insert-file-contents useful-killing)

    (wal-kill-ring-save-whole-buffer)

    (should (string-equal "I hope I don't get killed\nThat would be terrible\n" (car kill-ring)))))

(ert-deftest wal-then-add-delete-trailing-whitespace-hook--does-nothing-if-unset ()
  (let ((wal-delete-trailing-whitespace nil))

    (with-temp-buffer
      (wal-then-add-delete-trailing-whitespace-hook)

      (should-not (buffer-local-value 'before-save-hook (current-buffer))))))

(ert-deftest wal-then-add-delete-trailing-whitespace-hook ()
  (let ((wal-delete-trailing-whitespace t))

    (with-temp-buffer
      (wal-then-add-delete-trailing-whitespace-hook)

      (should (buffer-local-value 'before-save-hook (current-buffer))))))

(ert-deftest test-wal-set-cursor-type--sets-and-resets ()
  (with-temp-buffer
    (with-mock ((completing-read . (lambda (&rest _) "hollow")))

      (wal-set-cursor-type)

      (should (eq cursor-type 'hollow)))

    (wal-set-cursor-type t)

    (should (eq cursor-type t))))

(ert-deftest test-wal-kwim--kills-forward-in-line ()
  (with-temp-buffer
    (insert-file-contents useful-killing)
    (goto-char 7)

    (wal-kwim)

    (should (equal (buffer-string) "I hope\nThat would be terrible\n"))))

(ert-deftest test-wal-kwim--kills-line-at-end ()
  (with-temp-buffer
    (insert-file-contents useful-killing)
    (goto-char 0)
    (end-of-line)

    (wal-kwim)

    (should (equal (buffer-string) "That would be terrible\n"))))

(ert-deftest test-wal-kwim--kills-line-at-beg ()
  (with-temp-buffer
    (insert-file-contents useful-killing)

    (goto-char 0)

    (wal-kwim)

    (should (equal (buffer-string) "That would be terrible\n"))))

(ert-deftest test-wal-kwim--kills-region-if-active ()
  (with-mock ((region-active-p . #'always) kill-region)
    (with-temp-buffer
      (wal-kwim))

    (was-called-with kill-region (list nil nil t))))

(ert-deftest test-wal-mwim-beginning ()
  (with-temp-buffer
    (insert "hello\n  world")
    (goto-char 7)
    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 9))

    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 7))

    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 9))))

(ert-deftest test-wal-spill-paragraph ()
  (let ((sentence-end-double-space nil))
    (with-temp-buffer
      (insert "This is the first sentence.\nThis is the second one.")

      (call-interactively 'wal-spill-paragraph)

      (should (string-equal "This is the first sentence. This is the second one." (buffer-string)))))

  (with-mock (fill-paragraph)

    (funcall-interactively 'wal-spill-paragraph t)

    (was-called-with fill-paragraph (list nil t))))

(ert-deftest wal-increase-gc-cons-threshold ()
  (defvar gc-cons-threshold)
  (let ((gc-cons-threshold 0))
    (wal-increase-gc-cons-threshold)

    (should (eq 104857600 gc-cons-threshold))))

(ert-deftest test-wal-l ()
  (with-temp-buffer
    (wal-l)

    (should (window-dedicated-p))

    (wal-l)

    (should-not (window-dedicated-p))))

(ert-deftest test-wal-force-delete-other-windows ()
  (let ((ignore-window-parameters nil))

    (with-mock (delete-other-windows)

      (wal-force-delete-other-windows)

      (was-called delete-other-windows))))

(ert-deftest supernova ()
  (let ((a (get-buffer-create "stays"))
        (b (get-buffer-create "*goes*"))
        (c (get-buffer-create " also goes")))

    (display-buffer-full-frame a nil)
    (display-buffer-pop-up-window b nil)
    (display-buffer-below-selected c nil)

    (should (eq (length (window-list-1)) 3))

    (wal-supernova)

    (should (eq (length (window-list-1)) 1))))

(ert-deftest test-wal-find-custom-file ()
  (wal-with-temp-file "custom.el"

    (let ((custom-file wal-tmp-file))

      (wal-find-custom-file)

      (should (string-equal (buffer-name) "custom.el")))))

(ert-deftest test-wal-find-init ()
  (with-mock ((file-truename . (lambda (_) wal-emacs-config-default-path)))

    (wal-find-init)

    (should (string-match-p (buffer-name) wal-emacs-config-default-path))))

(ert-deftest test-wal-find-fish-config ()
  (wal-with-temp-file "config.fish"

    (let ((wal-fish-config-locations `(,wal-tmp-file)))

      (wal-find-fish-config)

      (should (string-equal (buffer-name) "config.fish")))))

(ert-deftest test-wal-find-fish-config--errors-if-not-found ()
  (defvar wal-fish-config-locations)
  (let ((wal-fish-config-locations '()))

    (should-error (wal-find-fish-config) :type 'user-error)))

(ert-deftest test-wal-capture-flag ()
  (match-expansion
   (wal-capture-flag some-flag
     "We need to capture some flag.")
   `(when-let* ((flags wal-custom-flags)
                (dash-flag (cdr (assoc 'some-flag flags)))
                (found (member dash-flag command-line-args)))
      (message "We need to capture some flag.")

      (setq command-line-args (delete dash-flag command-line-args))
      (setq wal-flag-some-flag t))))

(defvar test-target nil)

(ert-deftest test-wal-append--appends ()
  (let ((test-target '(a b c))
        (sequence '(d e f)))

    (wal-append 'test-target sequence)

    (should (equal test-target '(a b c d e f)))))

(ert-deftest test-wal-append--removes-duplicates ()
  (let ((test-target '(a b c))
        (sequence '(c d a)))

    (wal-append 'test-target sequence)

    (should (equal test-target '(a b c d)))))

(ert-deftest test-wal-replace-in-alist--replaces ()
  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat"))))

    (wal-replace-in-alist 'test-target values)

    (should (equal test-target '((a . "whale") (b . "heimat"))))))

(ert-deftest test-wal-replace-in-alist--refuses-new-keys ()
  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat") (c . "dolphin"))))

    (should-error (wal-replace-in-alist 'test-target values) :type 'user-error)
    (should (equal test-target '((a . "whale") (b . "home"))))))

(ert-deftest test-wal-insert-after--inserts ()
  (let ((test-target '(hello my old friend))
        (preceding 'hello)
        (item 'darkness))

    (wal-insert-after 'test-target preceding item)
    (should (equal test-target '(hello darkness my old friend)))))

(ert-deftest test-wal-insert-after--errors-if-key-already-in-list ()
  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should-error (wal-insert-after 'test-target preceding item t))))

(ert-deftest test-wal-insert-after--errors-if-key-not-in-list ()
  (let ((test-target '(hello my old friend))
        (preceding 'darkness)
        (item 'hello-again))

    (should-error (wal-insert-after 'test-target preceding item) :type 'user-error)))

(ert-deftest test-wal-list-from--builds-list-if-element ()
  (let ((test-target "testing"))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest test-wal-list-from--appends-if-list ()
  (let ((test-target '("testing")))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest test-wal-list-from--deletes-duplicates ()
  (let ((test-target '("testing" "again")))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest test-wal-plist-keys--errors-if-invalid ()
  (should-error (wal-plist-keys '(:test a :best))))

(ert-deftest test-wal-plist-keys--extracts-keys ()
  (should (equal '(:test :this :function) (wal-plist-keys '(:test "whether" :this "hacky" :function "works")))))

(ert-deftest test-parallel ()
  (match-expansion
   (parallel some-fun other-fun)
   `(defun some-fun||other-fun (&optional arg)
      "Call `some-fun' or `other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior."
      (interactive "P")

      (cond
       ((not arg)
        (call-interactively ',some-fun))
       ((equal 0 arg)
        (setq current-prefix-arg nil)
        (prefix-command-update)
        (call-interactively ',other-fun))
       ((equal (prefix-numeric-value arg) arg)
        (call-interactively ',other-fun))
       (t
        (call-interactively 'some-fun))))))

(ert-deftest test-parallel--universalize ()
  (match-expansion
   (parallel some-fun other-fun :universalize t)
   `(defun some-fun||other-fun (&optional arg)
      "Call `some-fun' or `other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior.\n\nThis function is universalized."
      (interactive "P")

      (cond
       ((not arg)
        (call-interactively ',some-fun))
       ((equal 0 arg)
        (setq current-prefix-arg nil)
        (prefix-command-update)
        (call-interactively ',other-fun))
       ((equal (prefix-numeric-value arg) arg)
        (progn
          (setq current-prefix-arg (list arg))
          (prefix-command-update)
          (call-interactively ',other-fun)))
       (t
        (call-interactively 'some-fun))))))

(ert-deftest test-wal-scratch-buffer ()
  (with-mock ((pop-to-buffer . (lambda (n &rest _) (buffer-name n))))

    (should (equal (wal-scratch-buffer) "*scratch*"))
    (should (equal (wal-scratch-buffer t) "*scratch*<2>"))
    (should (equal (wal-scratch-buffer 4) "*scratch*<4>")))

  (kill-buffer "*scratch*<2>")
  (kill-buffer "*scratch*<4>"))

(ert-deftest test-wal-persist-scratch-and-rehydrate ()
  (defvar wal-scratch-persist-file)
  (wal-with-temp-file "scratch"
    (let ((wal-scratch-persist-file wal-tmp-file))

      (delete-file wal-tmp-file)

      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)
        (insert "This one's itchy"))

      (wal-persist-scratch)

      (let ((file (find-file-noselect wal-scratch-persist-file)))

        (with-current-buffer file

          (should (string-equal "This one's itchy" (buffer-string)))))

      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)

        (should (string-equal "" (buffer-string)))

        (wal-rehydrate-scratch)

        (should (string-equal "This one's itchy" (buffer-string)))))))

(ert-deftest test-wal-disable-tabs--disables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode t)

    (wal-disable-tabs)

    (should (eq indent-tabs-mode nil))))

(ert-deftest test-wal-enable-tabs--enables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (should (eq indent-tabs-mode nil))

    (wal-enable-tabs)

    (should (eq indent-tabs-mode t))))

(ert-deftest test-wal-maybe-enable-tabs--enables-if-tabs-preferred ()
  (with-temp-buffer
    (setq-local wal-prefer-tabs t)

    (wal-maybe-enable-tabs)

    (should (eq indent-tabs-mode t))))

(ert-deftest test-wal-maybe-enable-tabs--disables-unless-preferred ()
  (with-temp-buffer
    (setq-local wal-prefer-tabs nil)

    (wal-maybe-enable-tabs)

    (should (eq indent-tabs-mode nil))))

(ert-deftest test-wal-biased-random ()
  (let ((vals '(1 2 3 4)))

    (with-mock ((random . (lambda (_) (pop vals))))

      (should (eq (wal-biased-random 4) 3))

      (setq vals '(1 2 3 4))

      (should (eq (wal-biased-random 4 t) 1)))))

(ert-deftest test-wal-bytes-per-mb--floors ()
  (should (equal 314572 (wal-bytes-per-mb 0.3))))

(ert-deftest test-wal-truncate--truncates ()
  (should (string-equal (wal-truncate "This is it" 7) "This...")))

(ert-deftest test-wal-truncate--truncates-without-len ()
  (should (string-equal (wal-truncate "This is it") "This ...")))

(ert-deftest test-wal-truncate--leaves-as-is-if-below ()
  (should (string-equal (wal-truncate "This is it" 24) "This is it")))

(ert-deftest test-wal-pad-string--pads ()
  (let ((test-string "hello"))

    (should (equal " hello" (wal-pad-string test-string)))))

(ert-deftest test-wal-pad-string--pads-right ()
  (let ((test-string "hello"))

    (should (equal "hello " (wal-pad-string test-string t)))))

(ert-deftest test-wal-univ-p ()
  (let ((current-prefix-arg '(4)))

    (should (wal-univ-p))))

(defvar test-standard 'standard)

(ert-deftest test-wal-reset-to-standard--resets ()
  (setq test-standard 'global)

  (should (equal 'global test-standard))

  (wal-reset-to-standard 'test-standard)

  (should (equal nil test-standard))
  (should-error (wal-reset-to-standard 'test-standard t) :type 'user-error)

  (with-temp-buffer
    (setq-local test-standard 'local)

    (wal-reset-to-standard 'test-standard t)

    (should (equal nil test-standard))))

(ert-deftest test-wal-try ()
  (match-expansion
   (wal-try test
     (message "Testing again"))
   `(when (require 'test nil :no-error)
      (message "Testing again"))))

(ert-deftest test-wal-server-edit-p ()
  (defvar server-buffer-clients)
  (defvar with-editor-mode)
  (let ((server-buffer-clients '(test)))

    (should (wal-server-edit-p)))

  (let ((server-buffer-clients '(test))
        (with-editor-mode t))

    (should-not (wal-server-edit-p)))

  (let ((server-buffer-clients '(test))
        (with-editor-mode nil))

    (should (wal-server-edit-p))))

(ert-deftest test-wal-delete-edit-or-kill ()
  (with-mock ((wal-server-edit-p . #'always)
              (server-edit-abort . (lambda () 'abort))
              (server-edit . (lambda () 'edit)))

    (should (equal (wal-delete-edit-or-kill) 'edit))
    (should (equal (wal-delete-edit-or-kill t) 'abort)))

  (with-mock ((wal-server-edit-p . #'ignore)
              (daemonp . #'always)
              (delete-frame . (lambda () 'delete-frame)))

    (should (equal (wal-delete-edit-or-kill) 'delete-frame)))

  (with-mock ((wal-server-edit-p . #'ignore)
              (daemonp . #'ignore)
              (save-buffers-kill-terminal . (lambda () 'kill)))

    (should (equal (wal-delete-edit-or-kill) 'kill))))

(require 'shell)

(ert-deftest test-wal-dead-shell-p ()
  (with-temp-buffer
    (shell-mode)

    (should (wal-dead-shell-p))))

(ert-deftest test-wal-prefix-user-key ()
  (defvar wal-use-hyper-prefix)
  (cl-letf (((symbol-function 'daemonp) #'always)
            (wal-use-hyper-prefix t))

    (should (string-equal (wal-prefix-user-key "k") "H-k")))

  (cl-letf (((symbol-function 'daemonp) #'ignore)
            (wal-use-hyper-prefix t))

    (should (string-equal (wal-prefix-user-key "k") "C-c w k"))))

(ert-deftest test-wal-on-boot ()
  (defvar wal-booting nil)

  (let ((wal-booting t))
    (match-expansion
     (wal-on-boot test
       (setq wal-is-testing t))
     `(progn
        (setq wal-is-testing t))))

  (let ((wal-booting nil))

    (match-expansion
     (wal-on-boot test
       (setq wal-is-testing t))
     `(message "Ignoring statements in '%s'" 'test))))

(ert-deftest test-wal-transient-define-major ()
  (match-expansion
   (wal-transient-define-major test-mode ()
     "This is a world."
     [("i" "ignore" ignore)])
   `(transient-define-prefix test-mode-major ()
      "This is a world."
      [("i" "ignore" ignore)]))

  (defun test-mode-major () nil)
  (match-expansion
   (wal-transient-define-major test-mode ()
     "This is a world."
     [("i" "ignore" ignore)])
   `nil)
  (let ((wal-transient-may-redefine t))
    (match-expansion
     (wal-transient-define-major test-mode ()
       "This is a world."
       [("i" "ignore" ignore)])
     `(transient-define-prefix test-mode-major ()
        "This is a world."
        [("i" "ignore" ignore)])))
  (fmakunbound 'test-prefix))

(ert-deftest test-wal-when-ready ()
  (with-mock ((daemonp . #'ignore))

    (match-expansion
     (wal-when-ready (message "No demon ..."))
     `(add-hook 'emacs-startup-hook (lambda () (message "No demon ...")))))

  (with-mock ((daemonp . #'always))

    (match-expansion
     (wal-when-ready (message "Demon!"))
     `(add-hook 'server-after-make-frame-hook (lambda () (message "Demon!"))))))

(defvar wal-test-setq-a nil)
(defvar wal-test-setq-b nil)

(ert-deftest test-setq-unless--only-sets-falsy ()
  (let ((wal-test-setq-a nil)
        (wal-test-setq-b "hello"))
    (match-expansion
     (setq-unless wal-test-setq-a "this"
                  wal-test-setq-b "but not this")
     `(progn
        (setq wal-test-setq-a "this")))))

(ert-deftest test-setq-unless--sets-unset ()
  (let ((wal-test-setq-a "hi")
        (wal-test-setq-b nil))

    (match-expansion
     (setq-unless wal-test-setq-b "this"
                  wal-test-setq-d "unknown")
     `(progn
        (setq wal-test-setq-b "this")
        (setq wal-test-setq-d "unknown")))))

(ert-deftest test-wal-define-init-setup ()
  (match-expansion
   (wal-define-init-setup test
     "Nothing else."
     :initial
     ((message "Hello"))
     :always
     ((message "Bye"))
     :immediately t)
   `(wal-on-boot test
      (defun wal-init-setup-test ()
        "Do base setup for test. Do minimal setup on repeats.\nNothing else."
        (unless (memq 'test wal-performed-setups)
          (progn
            (message "Initial setup of '%s'" "test")
            (message "Hello")
            (add-to-list 'wal-performed-setups 'test)))
        (message "Bye"))
      (if (daemonp)
          (progn
            (when t
              (funcall 'wal-init-setup-test))
            (add-hook 'server-after-make-frame-hook #'wal-init-setup-test))
        (add-hook 'emacs-startup-hook #'wal-init-setup-test)))))

(ert-deftest test-wal-duck-duck-go-region--succeeds-if-region ()
  (with-temp-buffer
    (insert "where is my mind")
    (set-mark (point-min))
    (goto-char (point-max))
    (with-mock ((browse-url . (lambda (url &rest _r) url)))
      (should (string-equal
               (wal-duck-duck-go-region)
               "https://duckduckgo.com/html/?q=where%20is%20my%20mind")))))

(ert-deftest test-wal-duck-duck-go-region--fails-if-no-region ()
  (should-error (wal-duck-duck-go-region) :type 'user-error))

(ert-deftest test-wal-message-in-a-bottle--shows-blue-whale ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'wal-rf))

      (should (string-equal (wal-message-in-a-bottle bottle) "}    , ﬞ   ⎠ Sting is playing bass, yeah")))))

(ert-deftest test-wal-message-in-a-bottle--shows-passed-string ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'wal-rf))

      (should (string-equal (wal-message-in-a-bottle bottle wal-ascii-cachalot-whale) "}< ,.__) Sting is playing bass, yeah")))))

(ert-deftest test-junk--install ()
  (with-mock (package-install
              delete-other-windows
              package-vc-install)

    (junk--install '(one two) :delete-windows t)
    (was-called-nth-with package-install '(one) 0)
    (was-called-nth-with package-install '(two) 1)
    (was-called delete-other-windows)
    (wal-clear-mocks)

    (junk--install '(four) :installer 'package-vc-install)
    (was-called-with package-vc-install '(four))
    (was-not-called delete-other-windows)))

(ert-deftest test-junk-expand ()
  (match-expansion
   (junk-expand test
     "Tasteful expansion pack."
     :packages '(pull out of the package)
     :extras '(prep some ketchup)
     :recipes '(heat in oven))
   `(add-to-list
     'junk-expansion-packs
     '(test . (:packages '(pull out of the package)
                         :extras '(prep some ketchup)
                         :docs "Tasteful expansion pack."
                         :recipes '(heat in oven))))))

(defvar wal-test-packs '((one :packages
                              (one)
                              :extras nil :docs "That's one." :recipes nil)
                         (two :packages
                              (two)
                              :extras (twofer) :docs "That's two." :recipes nil)
                         (three :packages nil :extras nil :docs "That's three." :recipes
                                ((three-mode "https://get-three-mode")))))

(ert-deftest test-junk--packs ()
  (let ((junk-expansion-packs wal-test-packs))

    (should (equal (junk--packs) '(one two twofer three-mode)))))

(ert-deftest test-junk--pack-p ()
  (let ((junk-expansion-packs wal-test-packs))

    (should (junk--pack-p 'three-mode))))

(ert-deftest test-junk--filter--items-may-be-mapped ()
  (with-mock ((package-installed-p . (lambda (p) (memq p '(test best)))))

    (should (equal (junk--filter '((test "test") (rest "rest") (best "best")) :mapper #'car)
                   '((rest "rest"))))))

(ert-deftest test-junk--install-extras ()
  (let ((extras (plist-get (nth 2 junk-expansion-packs) :extras))
        (selection 'all))

    (ert-with-message-capture messages
      (with-mock ((package-installed-p . #'ignore)
                  (package-install . #'always)
                  (completing-read . (lambda (_m _l) selection)))

        (junk--install-extras extras)

        (setq selection 'twofer)
        (junk--install-extras extras)

        (should (string-equal messages "Installed all extras.\nInstalled extra ’twofer’.\n"))))))

(ert-deftest test-junk-install ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "one"))
                (package-installed-p . #'ignore)
                (package-install . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))

      (let ((junk-expansion-packs wal-test-packs))

        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Installed 'one'."))))))

(ert-deftest test-junk-install--installed-already ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "one"))
                (package-installed-p . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))
      (let ((junk-expansion-packs wal-test-packs))

        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Package 'one' is already installed."))))))

(ert-deftest test-junk-install--with-extras ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "two"))
                (package-installed-p . #'ignore)
                (package-install . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args)))))
                (yes-or-no-p . #'ignore))

      (let ((junk-expansion-packs wal-test-packs))
        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Installed 'two'."))))

    (with-mock ((completing-read . (lambda (_m _v) "two"))
                (package-installed-p . (lambda (it) (equal 'two it)))
                (package-install . #'always)
                (yes-or-no-p . #'always)
                (junk--install-extras . (lambda (_) 'extra)))

      (let ((junk-expansion-packs wal-test-packs))

        (should (equal (call-interactively 'junk-install) 'extra))))))

(ert-deftest test--junk-package-vc-install ()
  (with-mock (package-vc-install package--update-selected-packages)

    (junk-package-vc-install '(test "http://test.com"))

    (was-called-with package-vc-install (list "http://test.com"))
    (was-called-with package--update-selected-packages (list '(test) nil))))

(ert-deftest test--junk-package-vc-install--shows-error-if-not-present ()
  (with-mock ((fboundp . #'ignore))

    (should-error (junk-package-vc-install '(test "http://test.com")) :type 'user-error)))

(ert-deftest test-junk-install--errors-for-non-existing ()
  (let ((junk-expansion-packs wal-test-packs))

    (should-error (junk-install 'four))))

(ert-deftest test-junk--stringify ()
  (should (string-equal (junk--stringify '(one two three)) "one, two, three"))
  (should (string-empty-p (junk--stringify '()))))

(ert-deftest test-wal-prog-like ()
  (with-mock ((run-hooks . #'wal-rf))

    (should (equal (wal-prog-like) 'prog-like-hook))))

(ert-deftest test-harpoon--treesit-ready-p ()
  (defvar harpoon--treesit-alist)
  (with-mock ((wal-modern-emacs-p . #'always)
              (require . #'always)
              (treesit-available-p . #'always)
              (treesit-ready-p . (lambda (it &rest _) (equal 'testable it))))

    (let ((harpoon--treesit-modes '((test-mode . testable) (zest-mode . zestable))))

      (should (harpoon--treesit-ready-p 'test-mode))
      (should-not (harpoon--treesit-ready-p 'zest-mode))
      (should-not (harpoon--treesit-ready-p 'no-mapping-mode)))))

(ert-deftest test-harpoon ()
  (match-expansion
   (harpoon test-mode
     :messages ("Just testing")
     :lsp t
     :tabs t)
   `(progn
      (harpoon-function test-mode
        :messages ("Just testing")
        :lsp t
        :tabs t)

      (harpoon-hook test-mode)

      (harpoon-ligatures test-mode
        :messages ("Just testing")
        :lsp t
        :tabs t)

      (harpoon-lsp
       :messages ("Just testing")
       :lsp t
       :tabs t)

      (harpoon-treesit test-mode))))

(ert-deftest test-harpoon-function ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (wal-lsp))))

(ert-deftest test-harpoon-function--some-symbol ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t
     :tabs anything)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (progn
        (hack-local-variables)
        (wal-maybe-enable-tabs))
      (wal-lsp))))

(ert-deftest test-harpoon-function--enable-indent ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t
     :tabs always)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (wal-enable-tabs)
      (wal-lsp))))

(ert-deftest test-harpoon-function--no-tabs ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp nil
     :tabs never)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (wal-disable-tabs))))

(ert-deftest test-harpoon-function--prog-like ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :prog-like t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (message "hi")
      (run-hooks 'prog-like-hook))))

(ert-deftest test-harpoon-function--major ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :major t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (message "hi")
      (local-set-key (kbd (wal-key-combo-for-leader 'major)) 'test-mode-major))))

(ert-deftest test-harpoon--corfu ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :corfu (0.2 4)
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (wal-message-in-a-bottle '("Just testing"))
      (message "hi")
      (progn
        (wal-corfu-auto '(0.2 4))
        (local-set-key (kbd "C-M-i") #'completion-at-point)))))

(ert-deftest test-harpoon--functions ()
  (match-expansion
   (harpoon-function test-mode
     :functions (test-mode testable-mode)
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (message "hi")
      (progn
        (when (fboundp 'test-mode)
          (test-mode))
        (when (fboundp 'testable-mode)
          (testable-mode))))))

(ert-deftest test-harpoon-ligatures ()
  (match-expansion
   (harpoon-ligatures test-mode
     :ligatures ("?!"))
   `(wal-set-ligatures 'test-mode '("?!"))))

(ert-deftest test-harpoon-lsp ()
  (match-expansion
   (harpoon-lsp :lsp (:ignore-dirs (".ignoramus")))
   `(with-eval-after-load 'lsp-mode
      (wal-lsp-ignore-directory '(".ignoramus")))))

(ert-deftest test-harpoon-treesit ()
  (with-mock ((harpoon--treesit-ready-p . #'always))

    (match-expansion
     (harpoon-treesit test-mode)
     `(progn
        (message "Remapping %s to %s" 'test-mode 'test-ts-mode)
        (add-to-list 'major-mode-remap-alist
                     '(test-mode . test-ts-mode))
        (with-eval-after-load 'all-the-icons
          (defvar all-the-icons-mode-icon-alist)

          (when-let ((setting
                      (cdr
                       (assoc 'test-mode all-the-icons-mode-icon-alist)))
                     (name 'test-ts-mode))

            (add-to-list 'all-the-icons-mode-icon-alist
                         (cons name setting))))))))

(ert-deftest test-harpoon--mode-name--with-treeesit ()
  (with-mock ((harpoon--treesit-ready-p . #'always))
    (should (equal 'test-ts-mode (harpoon--mode-name 'test-mode)))))

(ert-deftest test-harpoon-hook ()
  (match-expansion
   (harpoon-hook test-mode)
   `(add-hook
     'test-mode-hook
     'test-mode-harpoon)))

(ert-deftest test-wal-fundamental-mode--switches ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (wal-fundamental-mode)

    (should (equal major-mode 'fundamental-mode))
    (should (equal wal-before-fundamental-mode 'emacs-lisp-mode))

    (wal-fundamental-mode)

    (should (equal major-mode 'emacs-lisp-mode))))

(ert-deftest test-wal-async-process--buffer-name ()
  (should (string= (wal-async-process--buffer-name 'test-mode) wal-async-process-buffer-name)))

(ert-deftest test-wal-async-process--finalize ()
  (with-temp-buffer
    (rename-buffer "*async-finalize-test*")

    (with-mock (delete-window delete-other-windows)

      (let ((finalizer (wal-async-process--finalize #'delete-window #'delete-other-windows)))

        (apply finalizer (list (current-buffer) "finished\n"))

        (was-called delete-window)
        (was-not-called delete-other-window)
        (wal-clear-mocks)))

    (with-mock ((delete-window . (lambda () (error "Oops"))) delete-other-windows)

      (let ((finalizer (wal-async-process--finalize #'delete-window #'delete-other-windows)))

        (apply finalizer (list (current-buffer) "finished\n"))

        (was-called delete-window)
        (was-called-with delete-other-windows "Oops*async-finalize-test*")
        (wal-clear-mocks)

        (apply finalizer (list (current-buffer) "something else "))

        (was-not-called delete-window)
        (was-called-with delete-other-windows "something else")))))

(ert-deftest test-wal-aysnc-process--maybe-interrupt ()
  (with-mock ((compilation-find-buffer . (lambda () (message "found-buffer") "buffer"))
              (get-buffer-process . (lambda (m) (message m)))
              (interrupt-process . (lambda (_) (message "interrupted"))))

    (ert-with-message-capture messages
      (wal-async-process--maybe-interrupt)
      (should (string= "found-buffer\nbuffer\ninterrupted\n" messages)))))

(ert-deftest test-wal-async-process ()
  (with-mock ((wal-async-process--maybe-interrupt . (lambda () (message "interrupted")))
              (compilation-start . (lambda (&rest _) (message "compiles"))))
    (ert-with-message-capture messages
      (wal-async-process
       "compiles"
       (lambda () (message "finishes"))
       (lambda (_m) nil)
       t)
      (with-current-buffer "*wal-async*"
        (funcall (car compilation-finish-functions) nil "finished\n"))
      (should (string= "interrupted\ncompiles\nfinishes\n" messages)))))

(ert-deftest test-wal-matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (wal-matches-in-string pattern str)))))

(ert-deftest test-wal-advise-many ()
  (defun wal-test-fun-1 (arg1 arg2)
    "Do something with ARG1 and ARG2."
    (list arg1 arg2))

  (defun wal-test-fun-2 (arg1 arg2)
    "Do something with ARG1 and ARG2."
    (list arg1 arg2))

  (defun wal-test-advice (fun &rest args)
    "Just call as is."
    (apply fun args))

  (wal-advise-many #'wal-test-advice :around '(wal-test-fun-1 wal-test-fun-2))

  (should (and (advice--p (advice--symbol-function 'wal-test-fun-1))
               (advice--p (advice--symbol-function 'wal-test-fun-2)))))

;;; wal-useful-test.el ends here
