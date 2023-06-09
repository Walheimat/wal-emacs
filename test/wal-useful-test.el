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

    (bydi ((:mock file-name-directory :return temp-dir)
           (:always y-or-n-p)
           make-directory)

      (wal-create-non-existent-directory)

      (bydi-was-called-with make-directory (list temp-dir t)))))

(ert-deftest test-wal-create-non-existent-directory--aborts ()
  (bydi (file-name-directory
         (:always file-exists-p))

    (should-not (wal-create-non-existent-directory))))

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
                     (display-buffer-reuse-window display-buffer-in-side-window display-buffer-in-direction display-buffer-use-some-window)
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
                     (display-buffer-reuse-window display-buffer-in-direction display-buffer-in-side-window display-buffer-use-some-window)
                     (side . right)
					 (direction . below)
					 (window-width . 0.4)
					 (window-height)
					 (dedicated)
                     (window-parameters . ((no-other-window))))))))

(ert-deftest test-wal-display-buffer-use-some-frame--with-display-p ()
  (let ((frame nil)
        (params nil))
    (bydi ((:mock selected-frame :return frame)
           get-lru-window
           (:mock frame-parameters :return params))


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
					  display-buffer-pop-up-window
                      display-buffer-use-some-window)
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
					  display-buffer-pop-up-window
                      display-buffer-use-some-window)
                     (frame-predicate . wal-display-buffer-use-some-frame--with-display-p)
                     (inhibit-switch-frame . t)
					 (window-width)
					 (window-height)
					 (dedicated)
					 (window-parameters . ((no-other-window))))))))

(ert-deftest wal-kill-some-file-buffers ()
  (bydi-with-temp-file "to-be-killed"
    (bydi kill-buffer-ask

      (find-file-noselect bydi-tmp-file)

      (get-buffer-create "killer-buffer")

      (wal-kill-some-file-buffers)

      (bydi-was-called-with kill-buffer-ask (list (get-buffer "to-be-killed"))))))

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
    (bydi ((:mock completing-read :return "hollow"))

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
  (bydi ((:always region-active-p) kill-region)
    (with-temp-buffer
      (wal-kwim))

    (bydi-was-called-with kill-region (list nil nil t))))

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

  (bydi (fill-paragraph)

    (funcall-interactively 'wal-spill-paragraph t)

    (bydi-was-called-with fill-paragraph (list nil t))))

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

    (bydi (delete-other-windows)

      (wal-force-delete-other-windows)

      (bydi-was-called delete-other-windows))))

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
  (bydi-with-temp-file "custom.el"

    (let ((custom-file bydi-tmp-file))

      (wal-find-custom-file)

      (should (string-equal (buffer-name) "custom.el")))))

(ert-deftest test-wal-find-init ()
  (bydi ((:mock file-truename :return wal-emacs-config-default-path))

    (wal-find-init)

    (should (string-match-p (buffer-name) wal-emacs-config-default-path))))

(ert-deftest test-wal-find-fish-config ()
  (bydi-with-temp-file "config.fish"

    (let ((wal-fish-config-locations `(,bydi-tmp-file)))

      (wal-find-fish-config)

      (should (string-equal (buffer-name) "config.fish")))))

(ert-deftest test-wal-find-fish-config--errors-if-not-found ()
  (defvar wal-fish-config-locations)
  (let ((wal-fish-config-locations '()))

    (should-error (wal-find-fish-config) :type 'user-error)))

(ert-deftest test-wal-capture-flag ()
  (bydi-match-expansion
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

(ert-deftest wal-insert-after--does-not-error-if-duplicates-allowed ()
  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should (wal-insert-after 'test-target preceding item t))))

(ert-deftest test-wal-insert-after--errors-if-key-already-in-list ()
  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should-error (wal-insert-after 'test-target preceding item))))

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
  (bydi-match-expansion
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
  (bydi-match-expansion
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
  (bydi ((:mock pop-to-buffer :with (lambda (n &rest _) (buffer-name n))))

    (should (equal (wal-scratch-buffer) "*scratch*"))
    (should (equal (wal-scratch-buffer t) "*scratch*<2>"))
    (should (equal (wal-scratch-buffer 4) "*scratch*<4>")))

  (kill-buffer "*scratch*<2>")
  (kill-buffer "*scratch*<4>"))

(ert-deftest test-wal-persist-scratch-and-rehydrate ()
  (defvar wal-scratch-persist-file)
  (bydi-with-temp-file "scratch"
    (let ((wal-scratch-persist-file bydi-tmp-file))

      (delete-file bydi-tmp-file)

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

(ert-deftest test-wal-biased-random ()
  (let ((vals '(1 2 3 4)))

    (bydi ((:mock random :with (lambda (_) (pop vals))))

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
  (bydi-match-expansion
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
  (bydi ((:always wal-server-edit-p)
         (:mock server-edit-abort :return 'abort)
         (:mock server-edit :return 'edit))

    (should (equal (wal-delete-edit-or-kill) 'edit))
    (should (equal (wal-delete-edit-or-kill t) 'abort)))

  (bydi ((:ignore wal-server-edit-p)
         (:always daemonp)
         (:mock delete-frame :return 'delete-frame))

    (should (equal (wal-delete-edit-or-kill) 'delete-frame)))

  (bydi ((:ignore wal-server-edit-p)
         (:ignore daemonp)
         (:mock save-buffers-kill-terminal :return 'kill))

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
    (bydi-match-expansion
     (wal-on-boot test
       (setq wal-is-testing t))
     `(progn
        (setq wal-is-testing t))))

  (let ((wal-booting nil))

    (bydi-match-expansion
     (wal-on-boot test
       (setq wal-is-testing t))
     `(message "Ignoring statements in '%s'" 'test))))

(ert-deftest test-wal-transient-define-major ()
  (bydi-match-expansion
   (wal-transient-define-major test-mode ()
     "This is a world."
     [("i" "ignore" ignore)])
   `(transient-define-prefix test-mode-major ()
      "This is a world."
      [("i" "ignore" ignore)]))

  (defun test-mode-major () nil)
  (bydi-match-expansion
   (wal-transient-define-major test-mode ()
     "This is a world."
     [("i" "ignore" ignore)])
   `nil)
  (let ((wal-transient-may-redefine t))
    (bydi-match-expansion
     (wal-transient-define-major test-mode ()
       "This is a world."
       [("i" "ignore" ignore)])
     `(transient-define-prefix test-mode-major ()
        "This is a world."
        [("i" "ignore" ignore)])))
  (fmakunbound 'test-prefix))

(ert-deftest test-wal-when-ready ()
  (bydi ((:ignore daemonp))

    (bydi-match-expansion
     (wal-when-ready (message "No demon ..."))
     `(add-hook 'emacs-startup-hook (lambda () (message "No demon ...")))))

  (bydi ((:always daemonp))

    (bydi-match-expansion
     (wal-when-ready (message "Demon!"))
     `(add-hook 'server-after-make-frame-hook (lambda () (message "Demon!"))))))

(defvar wal-test-setq-a nil)
(defvar wal-test-setq-b nil)

(ert-deftest test-setq-unless--only-sets-falsy ()
  (let ((wal-test-setq-a nil)
        (wal-test-setq-b "hello"))
    (bydi-match-expansion
     (setq-unless wal-test-setq-a "this"
                  wal-test-setq-b "but not this")
     `(progn
        (setq wal-test-setq-a "this")))))

(ert-deftest test-setq-unless--sets-unset ()
  (let ((wal-test-setq-a "hi")
        (wal-test-setq-b nil))

    (bydi-match-expansion
     (setq-unless wal-test-setq-b "this"
                  wal-test-setq-d "unknown")
     `(progn
        (setq wal-test-setq-b "this")
        (setq wal-test-setq-d "unknown")))))

(ert-deftest test-wal-define-init-setup ()
  (bydi-match-expansion
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
    (bydi ((:mock browse-url :with bydi-rf))
      (should (string-equal
               (wal-duck-duck-go-region)
               "https://duckduckgo.com/html/?q=where%20is%20my%20mind")))))

(ert-deftest test-wal-duck-duck-go-region--fails-if-no-region ()
  (should-error (wal-duck-duck-go-region) :type 'user-error))

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

    (bydi (delete-window delete-other-windows)

      (let ((finalizer (wal-async-process--finalize #'delete-window #'delete-other-windows)))

        (apply finalizer (list (current-buffer) "finished\n"))

        (bydi-was-called delete-window)
        (bydi-was-not-called delete-other-window)
        (bydi-clear-mocks)))

    (bydi ((:mock delete-window :with (lambda () (error "Oops"))) delete-other-windows)

      (let ((finalizer (wal-async-process--finalize #'delete-window #'delete-other-windows)))

        (apply finalizer (list (current-buffer) "finished\n"))

        (bydi-was-called delete-window)
        (bydi-was-called-with delete-other-windows "Oops*async-finalize-test*")
        (bydi-clear-mocks)

        (apply finalizer (list (current-buffer) "something else "))

        (bydi-was-not-called delete-window)
        (bydi-was-called-with delete-other-windows "something else")))))

(ert-deftest test-wal-aysnc-process--maybe-interrupt ()
  (bydi ((:mock compilation-find-buffer :with (lambda () (message "found-buffer") "buffer"))
         (:mock get-buffer-process :with (lambda (m) (message m)))
         (:mock interrupt-process :with (lambda (_) (message "interrupted"))))

    (ert-with-message-capture messages
      (wal-async-process--maybe-interrupt)
      (should (string= "found-buffer\nbuffer\ninterrupted\n" messages)))))

(ert-deftest test-wal-async-process ()
  (bydi ((:mock wal-async-process--maybe-interrupt :with (lambda () (message "interrupted")))
         (:mock compilation-start :with (lambda (&rest _) (message "compiles"))))
    (ert-with-message-capture messages
      (wal-async-process
       "compiles"
       (lambda () (message "finishes"))
       (lambda (_m) nil)
       t)
      (with-current-buffer "*wal-async*"
        (funcall (car compilation-finish-functions) nil "finished\n"))
      (should (string= "interrupted\ncompiles\nfinishes\n" messages)))))

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

;; Local Variables:
;; no-byte-compile: t
;; End:
