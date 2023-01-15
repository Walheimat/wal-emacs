;;; wal-func-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'wal-func nil t)

(ert-deftest test-wal/modern-emacs-p ()
  (let ((emacs-major-version 30))
    (should (wal/modern-emacs-p)))

  (let ((emacs-major-version 29))
    (should (wal/modern-emacs-p)))

  (let ((emacs-major-version 28))
    (should (wal/modern-emacs-p)))

  (let ((emacs-major-version 27))
    (should-not (wal/modern-emacs-p))))

(ert-deftest test-wal/create-non-existent-directory ()
  (let ((temp-dir "/tmp/some-other/dir/"))

    (with-mock ((file-name-directory . (lambda (&rest _r) temp-dir))
                (y-or-n-p . #'always)
                make-directory)

      (wal/create-non-existent-directory)

      (was-called-with make-directory (list temp-dir t)))))

(ert-deftest test-wal/create-non-existent-directory--aborts ()
  (let ((temp-dir "/tmp/some-other/dir/"))

    (make-directory temp-dir t)

    (with-mock ((file-name-directory . (lambda (&rest _r) temp-dir)))

      (should-not (wal/create-non-existent-directory)))

    (delete-directory temp-dir)))

(ert-deftest test-wal/display-buffer-condition--passes-strings ()
  (should (string-equal "testing" (wal/display-buffer-condition "testing"))))

(ert-deftest test-wal/display-buffer-condition--considers-symbols-major-modes ()
  (should (equal '(major-mode . test-mode) (wal/display-buffer-condition 'test-mode))))

(ert-deftest test-wal/display-buffer-condition--errors-for-unsupported-types ()
  (should-error (wal/display-buffer-condition '(hello world)) :type 'user-error))

(ert-deftest test-wal/display-buffer-in-pop-up ()
  (let ((display-buffer-alist '()))

    (wal/display-buffer-in-pop-up 'test-mode)

    (should (equal
             (car display-buffer-alist)
             '((major-mode . test-mode) (display-buffer-pop-up-window))))
    (setq display-buffer-alist '())

    (wal/display-buffer-in-pop-up 'test-mode t)

    (should (equal
             (car display-buffer-alist)
             '((major-mode . test-mode) (display-buffer-pop-up-frame))))))

(ert-deftest test-wal/display-buffer-in-side-window ()
  (let ((display-buffer-alist '()))

    (wal/display-buffer-in-side-window 'test-mode :side 'top :loose nil :no-other t :height 12)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . top)
                     (dedicated . t)
                     (reusable-frames . visible)
                     (window-height . 12)
                     (window-parameters . ((no-other-window . t))))))

    (setq display-buffer-list '())

    (wal/display-buffer-in-side-window 'test-mode :loose t)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . bottom)
                     (dedicated)
                     (reusable-frames . visible)
                     (window-height . 10)
                     (window-parameters . ((no-other-window))))))
    ))

(ert-deftest test-wal/display-buffer-in-direction ()
  (let ((display-buffer-alist '()))

    (wal/display-buffer-in-direction 'test-mode 'leftmost)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-mode-window display-buffer-in-direction)
                     (direction . leftmost))))))

(ert-deftest test-wal/display-buffer-ethereally ()
  (let ((display-buffer-alist '()))

    (wal/display-buffer-ethereally 'test-mode)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     nil
                     (window-parameters . ((mode-line-format . none))))))))

(ert-deftest wal/display-buffer-reuse-same-window ()
  (let ((display-buffer-alist '()))

    (wal/display-buffer-reuse-same-window 'test-mode)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-same-window))))))

(ert-deftest wal/kill-some-file-buffers ()
  (wal/with-temp-file "to-be-killed"
    (with-mock kill-buffer-ask

      (find-file-noselect wal/tmp-file)

      (get-buffer-create "killer-buffer")

      (wal/kill-some-file-buffers)

      (was-called-with kill-buffer-ask (list (get-buffer "to-be-killed"))))))

(ert-deftest test-wal/kill-ring-save-buffer ()
  (with-temp-buffer
    (insert "I hope I don't get killed")

    (wal/kill-ring-save-whole-buffer)

    (should (string-equal "I hope I don't get killed" (car kill-ring)))))

(ert-deftest test-wal/set-cursor-type--sets-and-resets ()
  (with-temp-buffer
    (with-mock ((completing-read . (lambda (&rest _) "hollow")))

      (wal/set-cursor-type)

      (should (eq cursor-type 'hollow)))

    (wal/set-cursor-type t)

    (should (eq cursor-type t))))

(ert-deftest test-wal/kwim--kills-forward-in-line ()
  (with-temp-buffer
    (insert "I hope I don't get killed")
    (goto-char 7)

    (wal/kwim)

    (should (equal (buffer-string) "I hope"))))

(ert-deftest test-wal/kwim--kills-line-at-end ()
  (with-temp-buffer
    (insert "This is a nice line\nThis will stay")
    (goto-char 0)
    (end-of-line)

    (wal/kwim)

    (should (equal (buffer-string) "This will stay"))))

(ert-deftest test-wal/kwim--kills-line-at-beg ()
  (with-temp-buffer
    (insert "This is a nice line\nThis will stay")
    (goto-char 0)

    (wal/kwim)

    (should (equal (buffer-string) "This will stay"))))


(ert-deftest test-wal/kwim--kills-region-if-active ()
  (with-mock ((region-active-p . #'always) kill-region)
    (with-temp-buffer
      (wal/kwim))

    (was-called-with kill-region (list nil nil t))))

(ert-deftest test-wal/split-window-the-other-way ()
  (with-temp-buffer
    (save-window-excursion
      (split-window-horizontally)
      (wal/split-window-the-other-way)

      (should (windows-sharing-edge (selected-window) 'below)))
    (save-window-excursion
      (split-window-horizontally)
      (other-window 1)
      (wal/split-window-the-other-way)

      (should (windows-sharing-edge (selected-window) 'below)))
    (save-window-excursion
      (split-window-vertically)
      (wal/split-window-the-other-way)

      (should (windows-sharing-edge (selected-window) 'right)))))

(ert-deftest test-wal/other-window ()
  (with-mock ((active-minibuffer-window . #'always)
              (switch-to-minibuffer . (lambda () 'mini)))

    (should (equal (wal/other-window) 'mini)))

  (with-mock ((active-minibuffer-window . #'ignore)
              (next-frame . (lambda () 'other))
              (other-frame . (lambda (_) 'frame)))

    (should (equal (wal/other-window) 'frame)))

  (with-mock ((active-minibuffer-window . #'ignore)
              (next-frame . (lambda () (selected-frame)))
              (one-window-p . #'ignore)
              (other-window . (lambda (_) 'window)))

    (should (equal (wal/other-window) 'window)))

  (with-mock ((active-minibuffer-window . #'ignore)
              (next-frame . (lambda () (selected-frame)))
              (one-window-p . #'always)
              (switch-to-buffer . (lambda (_) 'buffer)))

    (should (equal (wal/other-window) 'buffer)))

  (with-mock ((active-window-buffer . #'ignore)
              (next-frame . (lambda () 'other))
              (other-frame . (lambda (_) 'frame))
              (one-window-p . #'ignore)
              (other-window . (lambda (_) 'otherw)))

    (should (equal (wal/other-window t) 'frame))
    (should (equal (wal/other-window) 'otherw)))

  (with-mock ((active-window-buffer . (lambda () nil))
              (next-frame . (lambda () 'other))
              (other-frame . (lambda (_) 'frame))
              (one-window-p . (lambda () t)))

    (should (equal (wal/other-window t) 'frame))
    (should (equal (wal/other-window) 'frame))))

(ert-deftest test-wal/l ()
  (with-temp-buffer
    (wal/l)

    (should (window-dedicated-p))

    (wal/l t)

    (should-not (window-dedicated-p))))

(ert-deftest test-wal/find-custom-file ()
  (wal/with-temp-file "custom.el"

    (let ((custom-file wal/tmp-file))

      (wal/find-custom-file)

      (should (string-equal (buffer-name) "custom.el")))))

(ert-deftest test-wal/find-init ()
  (with-mock ((file-truename . (lambda (_) wal/emacs-config-default-path)))

    (wal/find-init)

    (should (string-equal (buffer-name) "emacs-config"))))

(ert-deftest test-wal/find-fish-config ()
  (wal/with-temp-file "config.fish"

    (let ((wal/fish-config-locations `(,wal/tmp-file)))

      (wal/find-fish-config)

      (should (string-equal (buffer-name) "config.fish")))))

(ert-deftest test-wal/find-fish-config--errors-if-not-found ()
  (defvar wal/fish-config-locations)
  (let ((wal/fish-config-locations '()))

    (should-error (wal/find-fish-config) :type 'user-error)))

(ert-deftest test-wal/capture-flag ()
  (match-expansion
   (wal/capture-flag some-flag
     "We need to capture some flag.")
   `(when-let* ((flags wal/custom-flags)
                (dash-flag (cdr (assoc 'some-flag flags)))
                (found (member dash-flag command-line-args)))
      (message "We need to capture some flag.")

      (setq command-line-args (delete dash-flag command-line-args))
      (setq wal/flag-some-flag t))))

(defvar test-target nil)

(ert-deftest test-wal/append--appends ()
  (let ((test-target '(a b c))
        (sequence '(d e f)))

    (wal/append 'test-target sequence)

    (should (equal test-target '(a b c d e f)))))

(ert-deftest test-wal/append--removes-duplicates ()
  (let ((test-target '(a b c))
        (sequence '(c d a)))

    (wal/append 'test-target sequence)

    (should (equal test-target '(a b c d)))))

(ert-deftest test-wal/replace-in-alist--replaces ()
  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat"))))

    (wal/replace-in-alist 'test-target values)

    (should (equal test-target '((a . "whale") (b . "heimat"))))))

(ert-deftest test-wal/replace-in-alist--refuses-new-keys ()
  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat") (c . "dolphin"))))

    (should-error (wal/replace-in-alist 'test-target values) :type 'user-error)
    (should (equal test-target '((a . "whale") (b . "home"))))))

(ert-deftest test-wal/insert-after--inserts ()
  (let ((test-target '(hello my old friend))
        (preceding 'hello)
        (item 'darkness))

    (wal/insert-after 'test-target preceding item)
    (should (equal test-target '(hello darkness my old friend)))))

(ert-deftest test-wal/insert-after--errors-if-key-already-in-list ()
  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should-error (wal/insert-after 'test-target preceding item t))))

(ert-deftest test-wal/insert-after--errors-if-key-not-in-list ()
  (let ((test-target '(hello my old friend))
        (preceding 'darkness)
        (item 'hello-again))

    (should-error (wal/insert-after 'test-target preceding item) :type 'user-error)))

(ert-deftest test-wal/list-from--builds-list-if-element ()
  (let ((test-target "testing"))

    (should (equal '("testing" "again") (wal/list-from 'test-target "again")))))

(ert-deftest test-wal/list-from--appends-if-list ()
  (let ((test-target '("testing")))

    (should (equal '("testing" "again") (wal/list-from 'test-target "again")))))

(ert-deftest test-wal/list-from--deletes-duplicates ()
  (let ((test-target '("testing" "again")))

    (should (equal '("testing" "again") (wal/list-from 'test-target "again")))))

(ert-deftest test-wal/univ ()
  (match-expansion
   (wal/univ some-fun other-fun)
   `(defun wal/univ-some-fun (&optional call-other)
      "Call `some-fun' or `other-fun' depending on prefix argument.\nNo argument means: call the prior. A single `C-u' means: call the latter. Two or more `C-u' means: call the prior with `universal-argument'."
      (interactive "P")
      (if (> (prefix-numeric-value call-other) 4)
          (call-interactively 'some-fun)
        (setq current-prefix-arg nil)
        (prefix-command-update)
        (if call-other
            (call-interactively 'other-fun)
          (call-interactively 'some-fun))))))

(ert-deftest test-wal/scratch-buffer ()
  (with-mock ((pop-to-buffer . (lambda (n &rest _) (buffer-name n))))

    (should (equal (wal/scratch-buffer) "*scratch*"))
    (should (equal (wal/scratch-buffer t) "*scratch*<2>"))
    (should (equal (wal/scratch-buffer 4) "*scratch*<4>")))

  (kill-buffer "*scratch*<2>")
  (kill-buffer "*scratch*<4>"))

(ert-deftest test-wal/persist-scratch-and-rehydrate ()
  (wal/with-temp-file "scratch"
    (let ((wal/scratch-persist wal/tmp-file))

      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)
        (insert "This one's itchy"))

      (wal/persist-scratch)

      (let ((file (find-file-noselect wal/scratch-persist)))

        (with-current-buffer file

          (should (string-equal "This one's itchy" (buffer-string)))))

      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)

        (should (string-equal "" (buffer-string)))

        (wal/rehydrate-scratch)

        (should (string-equal "This one's itchy" (buffer-string)))))))

(ert-deftest test-wal/disable-tabs--disables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode t)

    (wal/disable-tabs)

    (should (eq indent-tabs-mode nil))))


(ert-deftest test-wal/enable-tabs--enables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (should (eq indent-tabs-mode nil))

    (wal/enable-tabs)

    (should (eq indent-tabs-mode t))))


(ert-deftest test-wal/maybe-enable-tabs--enables-if-tabs-preferred ()
  (with-temp-buffer
    (setq-local wal/prefer-tabs t)

    (wal/maybe-enable-tabs)

    (should (eq indent-tabs-mode t))))

(ert-deftest test-wal/maybe-enable-tabs--sets-function ()
  (with-temp-buffer
    (setq-local wal/prefer-tabs nil)

    (wal/maybe-enable-tabs :indent-with 'some-fun)

    (should (eq indent-line-function 'some-fun))))

(ert-deftest test-wal/maybe-enable-tabs--disables-unless-preferred ()
  (with-temp-buffer
    (setq-local wal/prefer-tabs nil)

    (wal/maybe-enable-tabs)

    (should (eq indent-tabs-mode nil))))


(defvar wal/indent-offset 6)
(ert-deftest test-wal/set-indent-defaults ()
  (setq wal/prefer-tabs nil)

  (wal/set-indent-defaults 1)

  (should (eq python-indent-offset 1))
  (should (eq js-indent-level 1))
  (should (eq css-indent-offset 1))
  (should (eq tab-width 1))
  (should (string-equal json-encoding-default-indentation " "))
  (should electric-indent-inhibit)
  (should-not indent-tabs-mode)

  (wal/set-indent-defaults)

  (should (eq python-indent-offset 6))
  (should (eq js-indent-level 6))
  (should (eq css-indent-offset 6))
  (should (eq tab-width 6))
  (should (string-equal json-encoding-default-indentation "      "))
  (should electric-indent-inhibit)
  (should-not indent-tabs-mode))

(ert-deftest test-wal/biased-random ()
  (let ((vals '(1 2 3 4)))

    (with-mock ((random . (lambda (_) (pop vals))))

      (should (eq (wal/biased-random 4) 3))

      (setq vals '(1 2 3 4))

      (should (eq (wal/biased-random 4 t) 1)))))


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


(ert-deftest test-wal/pad-string--pads ()
  (let ((test-string "hello"))

    (should (equal " hello" (wal/pad-string test-string)))))

(ert-deftest test-wal/pad-string--pads-right ()
  (let ((test-string "hello"))

    (should (equal "hello " (wal/pad-string test-string t)))))


(ert-deftest test-wal/univ-p ()
  (let ((current-prefix-arg '(4)))

    (should (wal/univ-p))))


(defvar test-standard 'standard)

(ert-deftest test-wal/reset-to-standard--resets ()
  (setq test-standard 'global)

  (should (equal 'global test-standard))

  (wal/reset-to-standard 'test-standard)

  (should (equal nil test-standard))
  (should-error (wal/reset-to-standard 'test-standard t) :type 'user-error)

  (with-temp-buffer
    (setq-local test-standard 'local)

    (wal/reset-to-standard 'test-standard t)

    (should (equal nil test-standard))))

(ert-deftest test-wal/try ()
  (match-expansion
   (wal/try test
     (message "Testing again"))
   `(when (require 'test nil :no-error)
      (message "Testing again"))))


(ert-deftest test-wal/server-edit-p ()
  (defvar server-buffer-clients)
  (defvar with-editor-mode)
  (let ((server-buffer-clients '(test)))

    (should (wal/server-edit-p)))

  (let ((server-buffer-clients '(test))
        (with-editor-mode t))

    (should-not (wal/server-edit-p)))

  (let ((server-buffer-clients '(test))
        (with-editor-mode nil))

    (should (wal/server-edit-p))))


(ert-deftest test-wal/delete-edit-or-kill ()
  (with-mock ((wal/server-edit-p . #'always)
              (server-edit-abort . (lambda () 'abort))
              (server-edit . (lambda () 'edit)))

    (should (equal (wal/delete-edit-or-kill) 'edit))
    (should (equal (wal/delete-edit-or-kill t) 'abort)))

  (with-mock ((wal/server-edit-p . #'ignore)
              (daemonp . #'always)
              (delete-frame . (lambda () 'delete-frame)))

    (should (equal (wal/delete-edit-or-kill) 'delete-frame)))

  (with-mock ((wal/server-edit-p . #'ignore)
              (daemonp . #'ignore)
              (save-buffers-kill-terminal . (lambda () 'kill)))

    (should (equal (wal/delete-edit-or-kill) 'kill))))


(require 'shell)

(ert-deftest test-wal/dead-shell-p ()
  (with-temp-buffer
    (shell-mode)

    (should (wal/dead-shell-p))))


(ert-deftest test-wal/prefix-user-key ()
  (defvar wal/use-hyper-prefix)
  (cl-letf (((symbol-function 'daemonp) #'always)
            (wal/use-hyper-prefix t))

    (should (string-equal (wal/prefix-user-key "k") "H-k")))

  (cl-letf (((symbol-function 'daemonp) #'ignore)
            (wal/use-hyper-prefix t))

    (should (string-equal (wal/prefix-user-key "k") "C-c w k"))))

(ert-deftest test-wal/on-boot ()
  (let ((wal/booting t))
    (match-expansion
     (wal/on-boot test
       (setq wal/is-testing t))
     `(progn
        (setq wal/is-testing t))))

  (let ((wal/booting nil))

    (match-expansion
     (wal/on-boot test
       (setq wal/is-testing t))
     `(message "Ignoring statements in '%s'" 'test))))

(ert-deftest test-wal/transient-define-prefix-once ()
  (match-expansion
   (wal/transient-define-prefix-once test-prefix ()
     "This is a world."
     [("i" "ignore" ignore)])
   `(transient-define-prefix test-prefix ()
      "This is a world."
      [("i" "ignore" ignore)]))

  (defun test-prefix () nil)
  (match-expansion
   (wal/transient-define-prefix-once test-prefix ()
     "This is a world."
     [("i" "ignore" ignore)])
   `nil)
  (let ((wal/transient-may-redefine t))
    (match-expansion
     (wal/transient-define-prefix-once test-prefix ()
       "This is a world."
       [("i" "ignore" ignore)])
     `(transient-define-prefix test-prefix ()
        "This is a world."
        [("i" "ignore" ignore)])))
  (fmakunbound 'test-prefix))

(ert-deftest test-wal/when-ready ()
  (with-mock ((daemonp . #'ignore))

    (match-expansion
     (wal/when-ready (message "No demon ..."))
     `(add-hook 'emacs-startup-hook (lambda () (message "No demon ...")))))

  (with-mock ((daemonp . #'always))

    (match-expansion
     (wal/when-ready (message "Demon!"))
     `(add-hook 'server-after-make-frame-hook (lambda () (message "Demon!"))))))

(defvar wal/test-setq-a nil)
(defvar wal/test-setq-b nil)

(ert-deftest test-setq-unless--only-sets-falsy ()
  (let ((wal/test-setq-a nil)
        (wal/test-setq-b "hello"))
    (match-expansion
     (setq-unless wal/test-setq-a "this"
                  wal/test-setq-b "but not this")
     `(progn
        (setq wal/test-setq-a "this")))))

(ert-deftest test-setq-unless--sets-unset ()
  (let ((wal/test-setq-a "hi")
        (wal/test-setq-b nil))

    (match-expansion
     (setq-unless wal/test-setq-b "this"
                  wal/test-setq-d "unknown")
     `(progn
        (setq wal/test-setq-b "this")
        (setq wal/test-setq-d "unknown")))))

(ert-deftest test-wal/define-init-setup ()
  (match-expansion
   (wal/define-init-setup test
     "Nothing else."
     :initial
     ((message "Hello"))
     :always
     ((message "Bye"))
     :immediately t)
   `(progn
      (defun wal/init-setup-test ()
        "Do base setup for test. Do minimal setup on repeats.\nNothing else."
        (unless (memq 'test wal/setup-list)
          (progn
            (message "Initial setup of '%s'" "test")
            (message "Hello")
            (add-to-list 'wal/setup-list 'test)))
        (message "Bye"))
      (if (daemonp)
          (progn
            (when t
              (funcall 'wal/init-setup-test))
            (add-hook 'server-after-make-frame-hook #'wal/init-setup-test))
        (add-hook 'emacs-startup-hook #'wal/init-setup-test)))))

(ert-deftest test-wal/duck-duck-go-region--succeeds-if-region ()
  (with-temp-buffer
    (insert "where is my mind")
    (set-mark (point-min))
    (goto-char (point-max))
    (with-mock ((browse-url . (lambda (url &rest _r) url)))
      (should (string-equal
               (wal/duck-duck-go-region)
               "https://duckduckgo.com/html/?q=where%20is%20my%20mind")))))

(ert-deftest test-wal/duck-duck-go-region--fails-if-no-region ()
  (should-error (wal/duck-duck-go-region) :type 'user-error))

(ert-deftest test-wal/message-in-a-bottle--shows-blue-whale ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'wal/rf))

      (should (string-equal (wal/message-in-a-bottle bottle) "}    , ﬞ   ⎠ Sting is playing bass, yeah")))))

(ert-deftest test-wal/message-in-a-bottle--shows-passed-string ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'wal/rf))

      (should (string-equal (wal/message-in-a-bottle bottle wal/ascii-cachalot-whale) "}< ,.__) Sting is playing bass, yeah")))))

(ert-deftest test-wal/install-packages ()
  (with-mock ((package-install . #'wal/rf)
              (package-installed-p . (lambda (package) (memq package '(three)))))

    (should (eq 2 (wal/install-packages '(one two) :delete-windows t)))
    (should (eq 1 (wal/install-packages '(three four))))))

(ert-deftest test-wal/install-recipes ()
  (with-mock ((quelpa . #'wal/rf)
              (package-installed-p . (lambda (package) (memq package '(three)))))

    (should (eq 2 (wal/install-recipes '((one rest) (two rest)))))
    (should (eq 1 (wal/install-recipes '((three rest) (four reset)))))))

(ert-deftest test-wal/define-expansion-pack ()
  (match-expansion
   (wal/define-expansion-pack test
     "Tasteful expansion pack."
     :packages '(pull out of the package)
     :extras '(prep some ketchup)
     :recipes '(heat in oven))
   `(add-to-list
     'wal/expansion-packs
     '(test . (:packages '(pull out of the package)
                         :extras '(prep some ketchup)
                         :docs "Tasteful expansion pack."
                         :recipes '(heat in oven))))))

(defvar wal/test-packs '((one :packages
                              (one)
                              :extras nil :docs "That's one." :recipes nil)
                         (two :packages
                              (two)
                              :extras (twofer) :docs "That's two." :recipes nil)
                         (three :packages nil :extras nil :docs "That's three." :recipes
                                ((three-mode :fetcher url :url "https://get-three-mode")))))

(ert-deftest test-wal/expansion-packs ()
  (let ((wal/expansion-packs wal/test-packs))

    (should (equal (wal/expansion-packs) '(one two twofer three-mode)))))

(ert-deftest test-wal/expansion-pack-p ()
  (let ((wal/expansion-packs wal/test-packs))

    (should (wal/expansion-pack-p 'three-mode))))


(ert-deftest test-wal/install-expansion-pack-extra ()
  (let ((messages '())
        (wal/expansion-packs wal/test-packs))

    (with-mock ((package-installed-p . #'ignore)
                (package-install . #'always)
                (completing-read . (lambda (_m _l) 'all)))

      (ert-with-message-capture messages
        (wal/install-expansion-pack-extra (nth 2 wal/expansion-packs))

        (should (string-match "Installed all extras" messages))))

    (with-mock ((package-installed-p . #'ignore)
                (package-install . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args)))))
                (completing-read . (lambda (_m _l) 'twofer)))

      (wal/install-expansion-pack-extra (nth 2 wal/expansion-packs))

      (should (string-equal (car messages) "Installed extra 'twofer'.")))))

(ert-deftest test-wal/install-expansion-pack ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "one"))
                (package-installed-p . #'ignore)
                (package-install . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))

      (let ((wal/expansion-packs wal/test-packs))

        (call-interactively 'wal/install-expansion-pack)

        (should (string-equal (car messages) "Installed expansion pack 'one'"))))))

(ert-deftest test-wal/install-expansion-pack--installed-already ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "one"))
                (package-installed-p . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))
      (let ((wal/expansion-packs wal/test-packs))

        (call-interactively 'wal/install-expansion-pack)

        (should (string-equal (car messages) "All core packages/recipes already installed."))))))

(ert-deftest test-wal/install-expansion-pack--with-extras ()
  (let ((messages '()))
    (with-mock ((completing-read . (lambda (_m _v) "two"))
                (package-installed-p . #'ignore)
                (package-install . #'always)
                (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args)))))
                (yes-or-no-p . #'ignore))

      (let ((wal/expansion-packs wal/test-packs))
        (call-interactively 'wal/install-expansion-pack)

        (should (string-equal (car messages) "Installed expansion pack 'two'"))))

    (with-mock ((completing-read . (lambda (_m _v) "two"))
                (package-installed-p . #'ignore)
                (package-install . #'always)
                (yes-or-no-p . #'always)
                (wal/install-expansion-pack-extra . (lambda (_) 'extra)))

      (let ((wal/expansion-packs wal/test-packs))

        (should (equal (call-interactively 'wal/install-expansion-pack) 'extra))))))

(ert-deftest test-wal/install-expansion-pack--errors-for-non-existing ()
  (let ((wal/expansion-packs wal/test-packs))

    (should-error (wal/install-expansion-pack 'four))))

(ert-deftest test-wal/expansion--stringify ()
  (should (string-equal (wal/expansion--stringify '(one two three)) "one, two, three"))
  (should (string-empty-p (wal/expansion--stringify '()))))

(ert-deftest test-wal/prog-like ()
  (with-mock ((run-hooks . #'wal/rf))

    (should (equal (wal/prog-like) 'prog-like-hook))))

(ert-deftest test-wal/hook ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :lsp t
     :lsp-ignores '(".ignoramus")
     :tabs t)
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/maybe-enable-tabs)
        (wal/lsp))
      (with-eval-after-load 'lsp-mode
        (wal/append 'lsp-file-watch-ignored-directories
                    '(".ignoramus")))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--custom-indent ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :lsp t
     :tabs 'some-fun)
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/maybe-enable-tabs :indent-with 'some-fun)
        (wal/lsp))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--enable-indent ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :lsp t
     :tabs always)
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/enable-tabs)
        (wal/lsp))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--with-tabs ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :lsp nil)
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/disable-tabs))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--prog-like ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :prog-like t
     (message "hi"))
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/disable-tabs)
        (message "hi")
        (run-hooks 'prog-like-hook))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--lieutenant ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :captain t
     (message "hi"))
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/disable-tabs)
        (message "hi")
        (local-set-key (kbd (wal/key-combo-for-leader 'wal/captain)) 'wal/test-dispatch))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--corfu ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :messages '("Just testing")
     :corfu (0.2 4)
     (message "hi"))
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (wal/message-in-a-bottle '("Just testing"))
        (hack-local-variables)
        (wal/disable-tabs)
        (message "hi")
        (wal/corfu-auto '(0.2 4)))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/hook--shallow ()
  (match-expansion
   (wal/hook test
     "We're just testing."
     :corfu (0.2 4)
     :shallow t
     (message "hi"))
   `(progn
      (defun wal/test-hook ()
        "We're just testing."
        (message "hi")
        (wal/corfu-auto '(0.2 4)))
      (add-hook 'test-hook 'wal/test-hook))))

(ert-deftest test-wal/fundamental-mode--switches ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (wal/fundamental-mode)

    (should (equal major-mode 'fundamental-mode))
    (should (equal wal/before-fundamental-mode 'emacs-lisp-mode))

    (wal/fundamental-mode)

    (should (equal major-mode 'emacs-lisp-mode))))

(ert-deftest test-wal/async-process--buffer-name ()
  (should (string= (wal/async-process--buffer-name major-mode) "*wal-async*")))

(ert-deftest test-wal/async-process--finalize ()
  (with-temp-buffer
    (rename-buffer "*async-finalize-test*")

      (with-mock (delete-window delete-other-windows)

        (let ((finalizer (wal/async-process--finalize #'delete-window #'delete-other-windows)))

          (apply finalizer (list (current-buffer) "finished\n"))

          (was-called delete-window)
          (was-not-called delete-other-window)
          (wal/clear-mocks)))

      (with-mock ((delete-window . (lambda () (error "Oops"))) delete-other-windows)

        (let ((finalizer (wal/async-process--finalize #'delete-window #'delete-other-windows)))

          (apply finalizer (list (current-buffer) "finished\n"))

          (was-called delete-window)
          (was-called-with delete-other-windows "Oops*async-finalize-test*")
          (wal/clear-mocks)

          (apply finalizer (list (current-buffer) "something else "))

          (was-not-called delete-window)
          (was-called-with delete-other-windows "something else")))))

(ert-deftest test-wal/aysnc-process--maybe-interrupt ()
  (with-mock ((compilation-find-buffer . (lambda () (message "found-buffer") "buffer"))
              (get-buffer-process . (lambda (m) (message m)))
              (interrupt-process . (lambda (_) (message "interrupted"))))

    (ert-with-message-capture messages
      (wal/async-process--maybe-interrupt)
      (should (string= "found-buffer\nbuffer\ninterrupted\n" messages)))))

(ert-deftest test-wal/async-process ()
  (with-mock ((wal/async-process--maybe-interrupt . (lambda () (message "interrupted")))
              (compilation-start . (lambda (c _ _n) (message "compiles") (get-buffer-create "async"))))
    (ert-with-message-capture messages
      (wal/async-process
       "compiles"
       (lambda () (message "finishes"))
       (lambda (_m) nil)
       t)
      (with-current-buffer "async"
        (funcall (car compilation-finish-functions) nil "finished\n"))
      (should (string= "interrupted\ncompiles\nfinishes\n" messages)))))

(ert-deftest test-wal/kill-async-process-buffers ()
  (get-buffer-create (generate-new-buffer-name "*wal-async*"))
  (get-buffer-create (generate-new-buffer-name "*wal-async*"))

  (let ((buf-count (length (buffer-list))))

    (call-interactively #'wal/kill-async-process-buffers)
    (should (> buf-count (length (buffer-list)))))


  (with-mock ((buffer-list . (lambda () (list (get-buffer-create (generate-new-buffer-name "*wal-async*")))))
              (get-buffer-window . (lambda (_) 'window))
              delete-window)

    (wal/kill-async-process-buffers)

    (was-called-with delete-window (list 'window))))

(ert-deftest test-wal/matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (wal/matches-in-string pattern str)))))

(ert-deftest test-wal/advise-many ()
  (defun wal/test-fun-1 (arg1 arg2)
    "Do something with ARG1 and ARG2."
    (list arg1 arg2))

  (defun wal/test-fun-2 (arg1 arg2)
    "Do something with ARG1 and ARG2."
    (list arg1 arg2))

  (defun wal/test-advice (fun &rest args)
    "Just call as is."
    (apply fun args))

  (wal/advise-many #'wal/test-advice :around '(wal/test-fun-1 wal/test-fun-2))

  (should (and (advice--p (advice--symbol-function 'wal/test-fun-1))
               (advice--p (advice--symbol-function 'wal/test-fun-2)))))

(ert-deftest test-wal/push-mark ()
  (with-mock message
    (with-temp-buffer
      (insert "testing")
      (goto-char (point-max))
      (wal/push-mark)

      (should (eq (mark t) 8))

      (call-interactively 'wal/push-mark)

      (should (eq (mark t) 8))

      (insert " still testing")
      (goto-char (point-max))
      (wal/push-mark)
      (insert " and still testing")
      (goto-char (point-max))

      (should (eq (length mark-ring) 1))

      (goto-char (+ 1 (point-min)))
      (wal/push-mark)

      (should (eq (length mark-ring) 2))

      (goto-char (point-max))
      (funcall-interactively 'wal/push-mark t)

      (should (eq (mark t) 40))
      (should (eq (length mark-ring) 1)))))

;;; wal-func-test.el ends here
