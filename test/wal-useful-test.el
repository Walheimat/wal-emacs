;;; wal-useful-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'wal-useful nil t)

(ert-deftest wal-modern-emacs-p ()
  :tags '(useful)

  (let ((emacs-major-version 30))
    (should (wal-modern-emacs-p 30)))

  (let ((emacs-major-version 29))
    (should (wal-modern-emacs-p)))

  (let ((emacs-major-version 28))
    (should (wal-modern-emacs-p)))

  (let ((emacs-major-version 27))
    (should-not (wal-modern-emacs-p))))

(ert-deftest wal-modern-emacs-p--errors-for-bad-arguments ()
  :tags '(useful)

  (should-error (wal-modern-emacs-p 27) :type 'user-error)
  (should-error (wal-modern-emacs-p "testing") :type 'user-error))

(ert-deftest wal-create-non-existent-directory ()
  :tags '(useful)

  (let ((temp-dir "/tmp/some-other/dir/"))

    (bydi ((:mock file-name-directory :return temp-dir)
           (:always y-or-n-p)
           make-directory)

      (wal-create-non-existent-directory)

      (bydi-was-called-with make-directory (list temp-dir t)))))

(ert-deftest wal-display-buffer--condition--passes-strings ()
  :tags '(useful)

  (should (string-equal "testing" (wal-display-buffer--condition "testing"))))

(ert-deftest wal-display-buffer--condition--considers-symbols-major-modes ()
  :tags '(useful)

  (should (equal '(major-mode . test-mode) (wal-display-buffer--condition 'test-mode))))

(ert-deftest wal-display-buffer--condition--errors-for-unsupported-types ()
  :tags '(useful)

  (should-error (wal-display-buffer--condition '(hello world)) :type 'user-error))

(ert-deftest wal-display-buffer-same-place-or-nearby ()
  :tags '(useful)

  (let ((display-buffer-alist '()))

    (wal-display-buffer-same-place-or-nearby 'test-mode :side 'top :loose nil :no-other t :height 12)

    (should (equal (car display-buffer-alist)
                   '((major-mode . test-mode)
                     (display-buffer-reuse-window display-buffer-in-side-window display-buffer-in-direction display-buffer-use-some-window)
                     (side . top)
					 (direction . right)
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
					 (window-width . 0.4))))))

(ert-deftest wal-display-buffer-use-some-frame--with-display-p ()
  :tags '(useful)

  (let ((frame nil)
        (params nil)
        (type nil))
    (bydi ((:mock selected-frame :return frame)
           (:mock framep :return 'x)
           get-lru-window
           (:mock frame-parameters :return params))

      (setq frame 1)
      (setq params '((display . t)))

      (should (wal-display-buffer-use-some-frame--with-display-p 0))
      (should-not (wal-display-buffer-use-some-frame--with-display-p 1))

      (setq params '((display . nil)))

      (should-not (wal-display-buffer-use-some-frame--with-display-p 0)))))

(ert-deftest wal-display-buffer-same-place-or-faraway ()
  :tags '(useful)

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
                     (inhibit-switch-frame . t))))

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
                     (inhibit-switch-frame . t))))))

(ert-deftest wal-kill-some-file-buffers ()
  :tags '(useful user-facing)

  (ert-with-temp-file to-be-killed
    :buffer killed

    (bydi (kill-buffer-ask
           (:mock buffer-list :return (list (get-buffer-create "killer-buffer") killed)))

      (wal-kill-some-file-buffers)

      (bydi-was-called-with kill-buffer-ask (list killed)))))

(defvar useful-killing (ert-resource-file "killing.txt"))

(ert-deftest wal-kill-ring-save-buffer ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert-file-contents useful-killing)

    (wal-kill-ring-save-whole-buffer)

    (should (string-equal "I hope I don't get killed\nThat would be terrible\n" (car kill-ring)))))

(ert-deftest wal-set-cursor-type--sets-and-resets ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (bydi ((:mock completing-read :return "hollow"))

      (wal-set-cursor-type)

      (should (eq cursor-type 'hollow)))

    (wal-set-cursor-type t)

    (should (eq cursor-type t))))

(ert-deftest wal-kwim--kills-forward-in-line ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert-file-contents useful-killing)
    (goto-char 7)

    (wal-kwim)

    (should (equal (buffer-string) "I hope\nThat would be terrible\n"))))

(ert-deftest wal-kwim--kills-line-at-end ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert-file-contents useful-killing)
    (goto-char 0)
    (end-of-line)

    (wal-kwim)

    (should (equal (buffer-string) "That would be terrible\n"))))

(ert-deftest wal-kwim--kills-line-at-beg ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert-file-contents useful-killing)

    (goto-char 0)

    (wal-kwim)

    (should (equal (buffer-string) "That would be terrible\n"))))

(ert-deftest wal-kwim--kills-region-if-active ()
  :tags '(useful user-facing)

  (bydi ((:always region-active-p) kill-region)
    (with-temp-buffer
      (wal-kwim))

    (bydi-was-called-with kill-region (list nil nil t))))

(ert-deftest wal-mwim-beginning ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert "hello\n  world")
    (goto-char 7)
    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 9))

    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 7))

    (call-interactively 'wal-mwim-beginning)

    (should (eq (point) 9))))

(ert-deftest wal-spill-paragraph ()
  :tags '(useful user-facing)

  (let ((sentence-end-double-space nil))
    (with-temp-buffer
      (insert "This is the first sentence.\nThis is the second one.")

      (call-interactively 'wal-spill-paragraph)

      (should (string-equal "This is the first sentence. This is the second one." (buffer-string)))))

  (bydi (fill-paragraph)

    (funcall-interactively 'wal-spill-paragraph t)

    (bydi-was-called-with fill-paragraph (list nil t))))

(ert-deftest wal-increase-gc-cons-threshold ()
  :tags '(useful)

  (defvar gc-cons-threshold)
  (let ((gc-cons-threshold 0))
    (wal-increase-gc-cons-threshold)

    (should (eq 104857600 gc-cons-threshold))))

(ert-deftest wal-l ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (shut-up (wal-l))

    (should (window-dedicated-p))

    (shut-up (wal-l))

    (should-not (window-dedicated-p))))

(ert-deftest wal-force-delete-other-windows ()
  :tags '(useful user-facing)

  (let ((ignore-window-parameters nil))

    (bydi (delete-other-windows)

      (wal-force-delete-other-windows)

      (bydi-was-called delete-other-windows))))

(ert-deftest supernova ()
  :tags '(useful user-facing)

  (let ((a (get-buffer-create "stays"))
        (b (get-buffer-create "*goes*"))
        (c (get-buffer-create " also goes")))

    (display-buffer-full-frame a nil)
    (display-buffer-pop-up-window b nil)
    (display-buffer-below-selected c nil)

    (should (eq (length (window-list-1)) 3))

    (wal-supernova)

    (should (eq (length (window-list-1)) 1))))

(ert-deftest wal-doppelganger ()
  :tags '(useful user-facing)

  (ert-with-test-buffer (:name "doppelganger")
    (let* ((expected (format "%s<doppelganger>" (buffer-name)))
           (finder (lambda (it) (string= expected (buffer-name (window-buffer it))))))

      (bydi ((:spy read-only-mode)
             (:spy clone-indirect-buffer)
             (:spy quit-window))

        (wal-doppelganger)

        (should (get-buffer expected))

        (bydi-was-called read-only-mode t)
        (bydi-was-called clone-indirect-buffer t)

        (wal-doppelganger)

        (bydi-was-not-called read-only-mode)
        (bydi-was-not-called clone-indirect-buffer)

        (should (seq-find finder (window-list-1)))

        (wal-doppelganger t)

        (bydi-was-called quit-window)))))

(ert-deftest wal-switch-to-buffer-obeying-display-actions ()
  :tags '(useful)

  (bydi (switch-to-buffer)

    (wal-switch-to-buffer-obeying-display-actions (current-buffer) t)

    (bydi-was-called-with switch-to-buffer (list (current-buffer) t))))

(ert-deftest wal-switch-to-other-buffer ()
  :tags '(useful user-facing)

  (bydi (switch-to-buffer)

    (wal-switch-to-other-buffer)

    (bydi-was-called-with switch-to-buffer (list nil t))))

(ert-deftest wal-interesting-windows ()
  :tags '(useful)

  (let ((term "other_terminal"))
    (bydi ((:mock window-frame :return 'window-frame)
           (:mock frame-list :return '(frame))
           (:mock window-list :return '(a b c))
           (:sometimes frame-live-p)
           (:sometimes frame-visible-p)
           (:mock terminal-name :return term)
           (:othertimes frame-parent))

      (should (equal '(a b c) (wal-interesting-windows))))))

(ert-deftest wal-swipe-window-prefix ()
  :tags '(useful)

  (shut-up
    (bydi ((:mock
            display-buffer-override-next-command
            :with
            bydi-rf)
           delete-other-windows
           (:mock display-buffer-in-direction :var direction :initial 'direction)
           (:always display-buffer-use-some-window))

      (let ((callback (wal-swipe-window-prefix)))

        (should (equal (cons 'direction 'window)
                       (funcall callback (current-buffer) nil)))

        (setq direction nil
              callback (wal-swipe-window-prefix))

        (should (equal (cons t 'reuse)
                       (funcall callback (current-buffer) nil)))))))

(ert-deftest wal-with-scrolling-window ()
  :tags '(useful)

  (bydi-match-expansion
   (wal-with-scrolling-window
     (message "test"))
   `(progn
     (when (one-window-p)
       (user-error "No scrolling window"))

     (with-selected-window (other-window-for-scrolling)
       (message "test")))))

(ert-deftest wal-isearch-other-window ()
  :tags '(useful)

  (bydi ((:mock other-window-for-scrolling :return (selected-window))
         (:othertimes one-window-p)
         isearch-forward
         recenter-top-bottom)

    (wal-isearch-other-window t)

    (bydi-was-called-with isearch-forward t)
    (bydi-was-called recenter-top-bottom)

    (bydi-toggle-volatile 'one-window-p)

    (should-error (wal-isearch-other-window))))

(ert-deftest wal-other-window-mru ()
  :tags '(useful windows)

  (bydi ((:mock get-mru-window :var mru)
         select-window)

    (should-not (wal-other-window-mru))

    (setq mru 'current)

    (should (wal-other-window-mru))

    (bydi-was-called-with select-window 'current)))

(ert-deftest wal-other-window-for-scrolling ()
  :tags '(useful windows)

  (bydi ((:sometimes get-mru-window)
         (:sometimes next-window))

    (wal-other-window-for-scrolling)

    (bydi-was-not-called next-window)

    (bydi-toggle-volatile 'get-mru-window)

    (wal-other-window-for-scrolling)

    (bydi-was-called next-window t)

    (bydi-toggle-volatile 'next-window)

    (wal-other-window-for-scrolling)

    (bydi-was-called-n-times next-window 2)))

(ert-deftest wal-find-custom-file ()
  :tags '(useful user-facing)

  (bydi ((:mock file-truename :return "/tmp/custom.el")
         (:mock find-file-noselect :with bydi-rf)
         switch-to-buffer)

    (wal-find-custom-file)
    (bydi-was-called-with switch-to-buffer "/tmp/custom.el")))

(ert-deftest wal-find-init ()
  :tags '(useful user-facing)

  (bydi ((:mock file-truename :return "/tmp/init.el")
         (:mock find-file-noselect :with bydi-rf)
         switch-to-buffer)

    (wal-find-init)
    (bydi-was-called-with switch-to-buffer "/tmp/init.el")))

(ert-deftest wal-find-fish-config ()
  :tags '(useful user-facing)

  (ert-with-temp-file fish
    :text "# configure"

    (let ((wal-fish-config-locations (list fish)))

      (wal-find-fish-config)

      (should (string-equal (buffer-string) "# configure")))))

(ert-deftest wal-find-fish-config--errors-if-not-found ()
  :tags '(useful)

  (defvar wal-fish-config-locations)
  (let ((wal-fish-config-locations '()))

    (should-error (wal-find-fish-config) :type 'user-error)))

(ert-deftest wal-capture-flag ()
  :tags '(useful)

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

(ert-deftest wal-append--appends ()
  :tags '(useful)

  (let ((test-target '(a b c))
        (sequence '(d e f)))

    (wal-append 'test-target sequence)

    (should (equal test-target '(a b c d e f)))))

(ert-deftest wal-append--removes-duplicates ()
  :tags '(useful)

  (let ((test-target '(a b c))
        (sequence '(c d a)))

    (wal-append 'test-target sequence)

    (should (equal test-target '(a b c d)))))

(ert-deftest wal-replace-in-alist--replaces ()
  :tags '(useful)

  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat"))))

    (wal-replace-in-alist 'test-target values)

    (should (equal test-target '((a . "whale") (b . "heimat"))))))

(ert-deftest wal-replace-in-alist--refuses-new-keys ()
  :tags '(useful)

  (let ((test-target '((a . "whale") (b . "home")))
        (values '((b . "heimat") (c . "dolphin"))))

    (should-error (wal-replace-in-alist 'test-target values) :type 'user-error)
    (should (equal test-target '((a . "whale") (b . "home"))))))

(ert-deftest wal-insert--inserts ()
  :tags '(useful)

  (let ((test-target '(hello my old friend))
        (preceding 'hello)
        (item 'darkness))

    (wal-insert 'test-target preceding item)
    (should (equal test-target '(hello darkness my old friend)))))

(ert-deftest wal-insert--does-not-error-if-duplicates-allowed ()
  :tags '(useful)

  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should (wal-insert 'test-target preceding item :allow-duplicates t))))

(ert-deftest wal-insert--errors-if-key-already-in-list ()
  :tags '(useful)

  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should-error (wal-insert 'test-target preceding item))))

(ert-deftest wal-insert--no-op-if-no-dupes-and-quiet ()
  :tags '(useful)

  (let ((test-target '(hello darkness my old friend))
        (preceding 'darkness)
        (item 'my))

    (should-not (wal-insert 'test-target preceding item :quiet t))))

(ert-deftest wal-insert--errors-if-key-not-in-list ()
  :tags '(useful)

  (let ((test-target '(hello my old friend))
        (preceding 'darkness)
        (item 'hello-again))

    (should-error (wal-insert 'test-target preceding item) :type 'user-error)))

(ert-deftest wal-insert--no-op-if-not-in-list-and-quiet ()
  :tags '(useful)

  (let ((test-target '(hello my old friend))
        (preceding 'darkness)
        (item 'hello-again))

    (should-not (wal-insert 'test-target preceding item :quiet t))))

(ert-deftest wal-insert--can-add-before ()
  :tags '(useful)

  (let ((test-target '(hello my old friend))
        (point 'my)
        (item 'darkness))

    (wal-insert 'test-target point item :before t)

    (should (equal '(hello darkness my old friend) test-target))))

(ert-deftest wal-list-from--builds-list-if-element ()
  :tags '(useful)

  (let ((test-target "testing"))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest wal-list-from--appends-if-list ()
  :tags '(useful)

  (let ((test-target '("testing")))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest wal-list-from--deletes-duplicates ()
  :tags '(useful)

  (let ((test-target '("testing" "again")))

    (should (equal '("testing" "again") (wal-list-from 'test-target "again")))))

(ert-deftest wal-plist-keys--errors-if-invalid ()
  :tags '(useful)

  (should-error (wal-plist-keys '(:test a :best))))

(ert-deftest wal-plist-keys--extracts-keys ()
  :tags '(useful)

  (should (equal '(:test :this :function) (wal-plist-keys '(:test "whether" :this "hacky" :function "works")))))

(ert-deftest wal-scratch-buffer ()
  :tags '(useful user-facing)

  (bydi ((:mock pop-to-buffer :with (lambda (n &rest _) (buffer-name n))))

    (should (equal (wal-scratch-buffer) "*scratch*"))
    (kill-buffer "*scratch*")
    (should (equal (wal-scratch-buffer) "*scratch*"))
    (should (equal (wal-scratch-buffer t) "*scratch*<2>"))
    (should (equal (wal-scratch-buffer 4) "*scratch*<4>"))
    (should (equal (wal-scratch-buffer 4) "*scratch*<4>")))

  (kill-buffer "*scratch*<2>")
  (kill-buffer "*scratch*<4>"))

(ert-deftest wal-persist-scratch-and-rehydrate ()
  :tags '(useful user-facing)

  (defvar wal-scratch-persist-file)
  (defvar wal-scratch-persist--marker)

  (ert-with-temp-file scratch
    (let ((wal-scratch-persist-file scratch)
          (wal-scratch-persist--marker "mark-it"))

      (delete-file scratch)

      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)
        (insert "This one has mark-it in it\n")
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

(ert-deftest wal-biased-random ()
  :tags '(useful)

  (let ((vals '(1 2 3 4)))

    (bydi ((:mock random :with (lambda (_) (pop vals))))

      (should (eq (wal-biased-random 4) 3))

      (setq vals '(1 2 3 4))

      (should (eq (wal-biased-random 4 t) 1)))))

(ert-deftest wal-bytes-per-mb--floors ()
  :tags '(useful)

  (should (equal 314572 (wal-bytes-per-mb 0.3))))

(ert-deftest wal-truncate--truncates ()
  :tags '(useful)

  (should (string-equal (wal-truncate "This is it" 7) "This...")))

(ert-deftest wal-truncate--truncates-without-len ()
  :tags '(useful)

  (should (string-equal (wal-truncate "This is it") "This ...")))

(ert-deftest wal-truncate--leaves-as-is-if-below ()
  :tags '(useful)

  (should (string-equal (wal-truncate "This is it" 24) "This is it")))

(ert-deftest wal-univ-p ()
  :tags '(useful user-facing)

  (let ((current-prefix-arg '(4)))

    (should (wal-univ-p))))

(defvar test-standard 'standard)

(ert-deftest wal-try ()
  :tags '(useful)

  (bydi-match-expansion
   (wal-try test
     (message "Testing again"))
   `(when (require 'test nil :no-error)
      (message "Testing again"))))

(ert-deftest wal-server-edit-p ()
  :tags '(useful)

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

(require 'shell)

(ert-deftest wal-dead-shell-p ()
  :tags '(useful)

  (with-temp-buffer
    (shell-mode)

    (should (wal-dead-shell-p))))

(ert-deftest wal-on-boot ()
  :tags '(useful)

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

(ert-deftest wal-transient-define-major ()
  :tags '(useful)

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

(ert-deftest wal-when-ready ()
  :tags '(useful)

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

(ert-deftest setq-unless--only-sets-falsy ()
  :tags '(useful)

  (let ((wal-test-setq-a nil)
        (wal-test-setq-b "hello"))
    (bydi-match-expansion
     (setq-unless wal-test-setq-a "this"
                  wal-test-setq-b "but not this")
     `(progn
        (setq wal-test-setq-a "this")))))

(ert-deftest setq-unless--sets-unset ()
  :tags '(useful)

  (let ((wal-test-setq-a "hi")
        (wal-test-setq-b nil))

    (bydi-match-expansion
     (setq-unless wal-test-setq-b "this"
                  wal-test-setq-d "unknown")
     `(progn
        (setq wal-test-setq-b "this")
        (setq wal-test-setq-d "unknown")))))

(ert-deftest wal-define-init-setup ()
  :tags '(useful)

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

(ert-deftest wal-duck-duck-go-region--succeeds-if-region ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (insert "where is my mind")
    (set-mark (point-min))
    (goto-char (point-max))
    (bydi ((:mock browse-url :with bydi-rf))
      (should (string-equal
               (wal-duck-duck-go-region)
               "https://duckduckgo.com/html/?q=where%20is%20my%20mind")))))

(ert-deftest wal-duck-duck-go-region--fails-if-no-region ()
  :tags '(useful user-facing)

  (should-error (wal-duck-duck-go-region) :type 'user-error))

(ert-deftest wal-fundamental-mode--switches ()
  :tags '(useful user-facing)

  (with-temp-buffer
    (emacs-lisp-mode)
    (wal-fundamental-mode)

    (should (equal major-mode 'fundamental-mode))
    (should (equal wal-before-fundamental-mode 'emacs-lisp-mode))

    (wal-fundamental-mode)

    (should (equal major-mode 'emacs-lisp-mode))))

(ert-deftest wal-async-process--buffer-name ()
  :tags '(useful)

  (should (string= (wal-async-process--buffer-name 'test-mode) wal-async-process-buffer-name)))

(ert-deftest wal-async-process--finalize ()
  :tags '(useful)

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

(ert-deftest wal-aysnc-process--maybe-interrupt ()
  :tags '(useful)

  (bydi ((:always compilation-find-buffer)
         (:mock get-buffer-process :return 'proc)
         interrupt-process)

    (wal-async-process--maybe-interrupt)

    (bydi-was-called-with interrupt-process 'proc)))

(ert-deftest wal-async-process ()
  :tags '(useful)

  (bydi ((:mock wal-async-process--maybe-interrupt :with (lambda () (message "interrupted")))
         (:mock compilation-start :with (lambda (&rest _) (message "compiles"))))
    (shut-up (ert-with-message-capture messages
               (wal-async-process
                "compiles"
                (lambda () (message "finishes"))
                (lambda (_m) nil)
                t)
               (with-current-buffer "*wal-async*"
                 (funcall (car compilation-finish-functions) nil "finished\n"))
               (should (string= "interrupted\ncompiles\nfinishes\n" messages))))))

(ert-deftest wal-advise-many ()
  :tags '(useful)

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
