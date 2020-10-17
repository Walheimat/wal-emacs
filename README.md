

# Emacs Org Config

This project is my personal Emacs (26.3)<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> config.

Its base is an org file so it doubles as a readme<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>.


# Table of Contents

1.  [Emacs Org Config](#org1a4d39f)
    1.  [Setup](#org5399d04)
    2.  [Config](#org22ea85f)
        1.  [Personal Config](#orgdc1fcd3)
        2.  [Initialization](#org8987cf6)
        3.  [Built-in](#orgbda5725)
        4.  [Packages](#orgb179f96)
        5.  [Mode Configs](#org71a33f0)
        6.  [Tweaks](#orgf8076d1)


<a id="org5399d04"></a>

## Setup

If you're interested in trying out Emacs using my config, here are the necessary steps:

-   Install Emacs if you haven't
-   Clone this repository into your `user-emacs-directory`<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>
-   Copy the config file included in this repo by running `cp ~/.emacs.d/emacs-config/.emacs.example ~/.emacs`
-   Close and re-run Emacs which should download and install (almost<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup>) all packages

If you did not init this repo in your `user-emacs-directory` using the default name, you will need to adapt
the variable `walheimat-emacs-config-default-path` in the example config you just copied.


<a id="org22ea85f"></a>

## Config

The init script will evaluate <span class="underline">everything</span> that follows.


<a id="orgdc1fcd3"></a>

### Personal Config

Set some personal info.<sup><a id="fnr.5" class="footref" href="#fn.5">5</a></sup>

    (setq user-full-name "Krister Schuchardt"
          user-mail-address "krister.schuchardt@theventury.com")


<a id="org8987cf6"></a>

### Initialization

Set up Emacs, package manager and packages.

1.  Start-Up

    Unclutter a bit.
    
        ;; start maximized
        (add-to-list 'default-frame-alist '(fullscreen . maximized))
        
        ;; ... or start fullscreen, you decide
        ;; (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
        
        ;; no splash
        (setq inhibit-startup-message t)
        
        ;; empty scratch message
        (setq initial-scratch-message ";; Howdy, stranger ...")
        
        ;; use a distinct file for customization
        (setq custom-file (expand-file-name "custom.el" walheimat-emacs-config-default-path))
        
        ;; we'll create that file if it doesn't yet exist
        (unless (file-exists-p custom-file)
          (write-region "" nil custom-file))
        
        (load custom-file)
        
        ;; use python3 as default python command
        (setq py-python-command "python3")

2.  MELPA

    Add MELPA to our package archives.
    We'll be getting most (if not all) packages from there.
    
        (require 'package)
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
        		  (not (gnutls-available-p))))
             (proto (if no-ssl "http" "https")))
          (when no-ssl (warn "\
        Your version of Emacs does not support SSL connections,
        which is unsafe because it allows man-in-the-middle attacks.
        There are two things you can do about this warning:
        1. Install an Emacs version that does support SSL and be safe.
        2. Remove this warning from your init file so you won't see it again."))
          (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
          ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
          ;; and `package-pinned-packages`. Most users will not need or want to do this.
          ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
          )
        (setq package-pinned-packages '())
        (package-initialize)

3.  Dependencies

    We manage our packages with `use-package`, before we can use it,
    we have to install it and a few other packages the hard way.
    
        ;; we need dash for the upcoming loop
        (unless (package-installed-p 'dash)
          (package-install 'dash))
        (require 'dash)
        
        (defun packages-install (packages)
          (--each packages
            (when (not (package-installed-p it))
              (package-install it)))
          (delete-other-windows))
        
        ;; packages that either don't need configuration (themes)
        ;; or are requirements in and of themselves are put here
        (defun init--install-packages ()
          (packages-install
           '(
             use-package
             diminish
             delight
             ample-theme
             doom-themes
             kaolin-themes
             naysayer-theme
             nord-theme
             panda-theme
             paper-theme
             spacemacs-theme
             )))
        
        (condition-case nil
            (init--install-packages)
          (error
            (package-refresh-contents)
            (init--install-packages)))

4.  Site-Lisp

    Add `site-lisp` directory and sub-directories to load path.
    I put non-MELPA packages here.
    If the directory doesn't exist, it will get created.
    Adapt if necessary.
    
        ;; create the dir if it does not exist to avoid error
        (unless (file-directory-p (expand-file-name "site-lisp" user-emacs-directory))
          (make-directory (expand-file-name "site-lisp" user-emacs-directory)))
        
        (setq site-lisp-dir
          (expand-file-name "site-lisp" user-emacs-directory))
        
        (add-to-list 'load-path site-lisp-dir)
        
        ;; add subdirs as well
        (dolist (project (directory-files site-lisp-dir t "\\w+"))
          (when (file-directory-p project)
            (add-to-list 'load-path project)))

5.  Package Management

    We also always want to ensure the package, i.e. if it's not there, get it.
    I'm also using chords for a few, common actions.
    
        (require 'use-package-ensure)
        (setq use-package-always-ensure t)
        
        (require 'diminish)
        (require 'delight)
        (require 'bind-key)
        
        ;; has to come here to be useable
        (use-package use-package-chords
          :config
          (key-chord-mode 1))


<a id="orgbda5725"></a>

### Built-in

Configure built-in settings.

1.  Saving and backups

    Don't clutter up workspaces.
    
        ;; save places and do so in a file
        (setq save-place-file (expand-file-name ".places" user-emacs-directory))
        
        ;; store backups in backups folder.
        (setq backup-directory-alist
          `(("." . ,(expand-file-name
            (concat user-emacs-directory "backups")))))
        
        ;; store autosaves in temp folder.
        (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
        
        ;; we don't want this to mess with git.
        (setq create-lockfiles nil)

2.  Prettifying

    Easy on the eyes.
    
        ;; a bunch of useful modes
        (show-paren-mode 1)
        (global-auto-revert-mode t)
        (global-hl-line-mode)
        ;; (add-hook 'prog-mode-hook 'linum-mode)
        (add-hook 'prog-mode-hook 'display-line-numbers-mode)
        (global-prettify-symbols-mode +1)
        ;; (global-whitespace-mode)
        (save-place-mode 1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        (global-font-lock-mode 1)
        (delete-selection-mode 1)
        
        ;; simple y/n is enough
        (defalias 'yes-or-no-p 'y-or-n-p)
        
        ;; I want my comments slanted and my keywords bold
        (defun my-font-lock-hook ()
          "Slanted and enchanted."
          (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
          (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
        )
        (add-hook 'font-lock-mode-hook 'my-font-lock-hook)
        
        ;; huge cursor
        ;; (setq x-stretch-cursor t)

3.  Reasonable Settings

    Make things snappier.
    
        (setq mouse-yank-at-point t)
        (setq show-paren-delay 0.0)
        (setq read-process-output-max (* 1024 1024)) ;; 1mb
        (setq sentence-end-double-space nil)
        (setq echo-keystrokes 0.1)

4.  Indentation

    I (generally) prefer tabs over spaces.
    To make this work, we need to tweak a few things.
    
        (setq custom-tab-width 4)
        
        (defun disable-tabs ()
          "Disable indent-tabs-mode."
          (interactive)
          (setq indent-tabs-mode nil))
        
        (defun enable-tabs  ()
          "Use TAB key and turn on indent-tabs-mode."
          (interactive)
          (local-set-key (kbd "TAB") 'tab-to-tab-stop)
          (setq indent-tabs-mode t)
          (setq tab-width custom-tab-width))
        
        (setq-default python-indent-offset custom-tab-width) ;; Python
        (setq-default js-indent-level custom-tab-width)      ;; Javascript
        
        ;; this always messes me up
        (setq-default electric-indent-inhibit t)
        
        (setq backward-delete-char-untabify-method 'hungry)

5.  Key Bindings

    Change up the key bindings a bit.
    
    I try to have most actions use user-reserved `C-c <key>` combinations,
    but some `C-x <key>` mappings snuck in.
    
    If you want to see all personal keybindings, execute `describe-personal-keybindings`.
    
    -   `<key-chord> # #` (un-)comments.
    -   `C-+` expands region.
    -   `C-c a <key>` opens today's (`t`), this week's (`w`) or your personal (`p`) agenda.
    -   `C-c b <key>` toggle(`t`) or shows(`s`) bookmarks.
    -   `C-c c <key>` to duplicate (`d`) the current line, kill (`k`) other buffers and (`o`) open with outside program.
    -   `C-c d` for docker actions.
    -   `C-c e` opens eshell.
    -   `C-c f a <key>` runs ag (`a` for generic, `p` for in-project search). <span class="underline">Requires ag</span>!
    -   `C-c f f` finds a project file.
    -   `C-c g` opens magit status.
    -   `C-c j=/=<key-chord> j j` dumb-jumps.
    -   `C-c m <key>` for multiple cursors.
    -   `C-c o` for code-folding.
    -   `C-c p <key>` interacts with perspectives
    -   `C-c q` opens the time machine.
    -   `C-c r s` restarts.
    -   `C-c s` uses swiper to search.
    -   `C-c t t` opens treemacs.
    -   `C-c v <key>` jumps to char (`c`) or line (`l`) with avy.
    -   `C-x C-c` opens this config org file.
    -   `C-x r q` (really) quits.
    -   `C-z=/=C-S-z` undos/redos.
    -   `M-o` goes to the "other" window or the last buffer.
    -   `M-x` opens smex.
    -   `s-(S)-RET` will open an indented line above (below).
    -   `s-k` kills the whole line.
    
    Note that all bindings for external packages are declared in the [packages](#orgb179f96) section.
    
        (global-set-key (kbd "C-c e") 'eshell)
        (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
        (global-set-key
          (kbd "C-x C-c")
          (lambda () (interactive)(switch-to-buffer (find-file-noselect (expand-file-name "configuration.org" walheimat-emacs-config-default-path)))))

6.  Theme

    Be sure to check out [Peach Melpa](https://peach-melpa.org/) to find a theme you like.
    
        (setq my-dark-emacs-theme 'doom-challenger-deep)
        (setq my-light-emacs-theme 'paper)
        (load-theme my-dark-emacs-theme t)

7.  Font

    Prefer FiraCode (-> mononoki -> Liberation -> DejaVu).
    If Emacs runs with the custom argument `-bigger`, the default font size is 14 (instead of 10).
    
    To get support for ligatures, install the symbol font from [here](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip).
    
        (defun font-candidate (&rest fonts)
          "Return the first available font from a list of fonts."
          (--first (find-font (font-spec :name it)) fonts))
        
        (set-face-attribute 'default nil :font (font-candidate '"Fira Code 12" "mononoki 12" "Liberation Mono 12" "DejaVu Sans Mono 12"))
        
        (defun found-custom-arg (switch)
          "Check for custom arg and delete it right away so emacs doesn't complain."
          (let ((found-switch (member switch command-line-args)))
            (setq command-line-args (delete switch command-line-args))
            found-switch))
        
        (if (found-custom-arg "-bigger")
          (set-default-font (font-candidate '"Fira Code 14" "mononoki 14" "Liberation Mono 14" "DejaVu Sans Mono 14"))
        )
        
        ;; use fira mode if it's the default font and the symbol font is installed
        (use-package fira-code-mode
          :if (and (x-list-fonts "Fira Code Symbol") (string= "Fira Code" (face-attribute 'default :family)))
          ;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
          :hook prog-mode)                                         ; mode to enable fira-code-mode in

8.  Zoning

    Zone out after a couple of minutes.
    
        (require 'zone)
        (zone-when-idle 180)

9.  Additional Functions

    Only need one so far.
    
        ;; check if buffer is treemacs buffer
        ;; similar to minibufferp
        (defun treemacsbufferp ()
          "Check if this is the treemacs buffer."
          (eq (current-buffer) (treemacs-get-local-buffer)))
        
        ;; trick garbage collection
        (defvar hundred-mb 100000000)
        (defun my-minibuffer-setup-hook ()
          (setq gc-cons-threshold most-positive-fixnum))
        (defun my-minibuffer-exit-hook ()
          (setq gc-cons-threshold hundred-mb))
        
        (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
        (add-hook 'minibuffer-exit-hook  #'my-minibuffer-exit-hook)


<a id="orgb179f96"></a>

### Packages

What follows is a list of MELPA packages that make Emacs even more awesome.

If you wish to know more about any of them, check out the list<sup><a id="fnr.6" class="footref" href="#fn.6">6</a></sup> of repositories
at the end of this readme/configuration or the [awesome-emacs](https://github.com/emacs-tw/awesome-emacs) project.

Many packages bind keys.
Check the [key bindings section](#org69917a3) if you need a list of all of them.

1.  add-node-modules-path

    Allows accessing a project's `node_modules`.
    
        (use-package add-node-modules-path)

2.  ag

    Highlight search results using the **Silver Searcher**.
    
    This <span class="underline">requires</span> the `ag` binary which you can get from [here](https://github.com/ggreer/the_silver_searcher#installation) (we will try
    to download it automatically, but might fail).
    
        (use-package ag
          :ensure-system-package ag
          :init
          (setq ag-highlight-search t)
          :bind ("C-c f a a" . ag)
        	("C-c f a p" . ag-project))

3.  all-the-icons

    You need to install the icons yourself<sup><a id="fnr.3.100" class="footref" href="#fn.3">3</a></sup>.
    
        (use-package all-the-icons)

4.  auto-package-update

    Keep packages updated (disabled for now).
    
        ;; (use-package auto-package-update
        ;;   :init
        ;;   (setq auto-package-update-delete-old-versions t)
        ;;   (setq auto-package-update-hide-results t)
        ;;   :config
        ;;   (auto-package-update-maybe))

5.  avy

    Jumping to (visible) lines and chars is fun if you are too lazy to use your mouse.
    
        (use-package avy
          :bind (("C-c v l" . avy-goto-line)
        	 ("C-c v c" . avy-goto-char)))

6.  beacon

    Help me find my cursor!
    
        (use-package beacon
          :config
          (beacon-mode 1)
          (setq beacon-color 0.4
        	beacon-blink-duration 0.4
        	beacon-size 60
        	beacon-blink-when-point-moves-vertically 2
          ))

7.  bm

    Bookmarks are useful. I don't remember where I was. <span class="underline">Who are you?!</span>
    
        (use-package bm
          :init
          (setq bm-restore-repository-on-load t)
          (setq bm-repository-file (expand-file-name ".cache/bm-persist" user-emacs-directory))
          (setq-default bm-buffer-persistence t)
          (setq bm-annotate-on-create t)
          (setq bm-highlight-style 'bm-highlight-only-fringe)
          (setq bm-cycle-all-buffers t)
          :hook
          ((after-init   .      bm-repository-load)
           (after-save   .      bm-buffer-save)
           (kill-buffer  .      bm-buffer-save)
           (kill-emacs   .      (lambda nil
        			  (bm-buffer-save-all)
        			  (bm-repository-save)))
           (find-file    .      bm-buffer-restore)
           (after-revert .      bm-buffer-restore)
           (vc-before-checkin . bm-buffer-save))
          :bind
           (("C-c b s" . bm-show)
            ("C-c b n" . bm-next)
            ("C-c b b" . bm-toggle)))

8.  company

    Code-completion. In a box.
    
        (use-package company-box
          :diminish
          :hook (company-mode . company-box-mode))
        
        (use-package company
          :delight " co"
          :init
          (setq company-prefer-capf t)
          (setq company-minimum-prefix-length 3)
          (setq company-idle-delay 0.5)
          :hook (prog-mode . company-mode))
        
        (use-package company-restclient
          :after company)
        
        (use-package company-web
          :after company)

9.  crux

    Let's use `crux` for some editing magic.
    Check the [key bindings section](#org69917a3) for descriptions.
    
        (use-package crux
          :bind (("M-o"          . crux-other-window-or-switch-buffer)
        	 ("C-c c k"      . crux-kill-other-buffers)
        	 ;; need to find solution with treemacs open
        	 ;; ("C-x 4 t")   .crux-transpose-windows
        	 ("C-c c o"      . crux-open-with)
        	 ("S-s-<return>" . crux-smart-open-line-above)
        	 ("s-<return>"   . crux-smart-open-line)
        	 ("s-k"          . crux-kill-whole-line)
        	 ("C-c c d"      . crux-duplicate-current-line-or-region)))

10. dap

    Debugging using VSCode's DAP.
    We register a node template for attaching to a Docker host.
    I currently only use it for JavaScript and Python.
    
        (when (version< emacs-version "27.0")
          (use-package dap-mode
            :delight " dap"
            :init
            (require 'cl) ;; deprecated in 27
            (setq dap-python-executable "python3")
            (setq dap-auto-configure-features '(sessions locals breakpoints))
            :config
            (require 'dap-node)
            (require 'dap-python)
            (dap-register-debug-template
              "Node::Attach"
              (list :type "node"
        	    :request "attach"
        	    :remoteRoot "/usr/src/app"
        	    :localRoot "/home/krister/theventury"
        	    :port 9229
        	    :name "Node::Attach"))
            :hook 
            ((js2-mode    . dap-mode)
             (python-mode . dap-mode)
             (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))))

11. delight

    Refine a couple of major-mode names.
    
        (use-package delight
          :config
          (delight 'js2-mode "js" :major)
          (delight 'emacs-lisp-mode "elisp" :major))

12. diff-hl

    Refresh post magit.
    
        (use-package diff-hl
          :init
          (global-diff-hl-mode)
          :hook (magit-post-refresh  . diff-hl-magit-post-refresh))

13. diminish

    See individual `use-package` declarations as well, since we delight in/diminish them there.
    
        (use-package diminish
          :config
          (diminish 'eldoc-mode))

14. dimmer

    Dim inactive frames.
    Make dimmed frames a bit dimmer.
    
        (use-package dimmer
          :diminish
          :config
          (dimmer-mode t)
          (setq dimmer-fraction 0.3)
          (dimmer-configure-org)
          (dimmer-configure-magit)
          (dimmer-configure-hydra)
          (setq dimmer-adjustmentmode :both))

15. docker

    I use Docker a lot, don't always have to use the command line.
    
        (use-package docker
          :bind ("C-c d" . docker))

16. drag stuff

    Use the default key bindings.
    
        (use-package drag-stuff
          :delight " drag"
          :config
          (drag-stuff-define-keys)
          (drag-stuff-global-mode))

17. dumb-jump

    Jump to definitions (in other files).
    Configure it for `ivy`.
    
        ;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
        (use-package dumb-jump
          :config
          (setq dumb-jump-selector 'ivy
        	dumb-jump-force-searcher 'ag)
          (dumb-jump-mode)
          :chords ("jj" . dumb-jump-go)
          :bind ("C-c j" . dumb-jump-go))

18. eshell

    Set up eshell.
    
        (use-package esh-autosuggest)
        (use-package eshell-prompt-extras)
        (use-package eshell-syntax-highlighting
          :config
          (eshell-syntax-highlighting-global-mode))
        
        (defun setup-eshell-ivy-completion ()
          (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point))
        
        (defun my-eshell-mode-hook ()
          "Hooks for eshell mode."
          (esh-autosuggest-mode)
          (setup-eshell-ivy-completion))
        
        ;; override how clearing the eshell works
        (defun eshell/clear ()
          "Clear the shell by truncating everything."
          (interactive)
          (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))
        
        (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)
        (with-eval-after-load "esh-opt"
          (autoload 'epe-theme-lambda "eshell-prompt-extras")
          (setq eshell-highlight-prompt nil
        	eshell-prompt-function 'epe-theme-lambda))

19. evilnc

    Comment code like in `vim`, evil, evil `vim`.
    
        (use-package evil-nerd-commenter
          :chords ("##" . evilnc-comment-or-uncomment-lines))

20. expand-region

    One thing that can be a bit tricky is selecting regions, not anymore.
    
        (use-package expand-region
          :bind ("C-+" . er/expand-region))

21. find-file-in-project

    Finding files by name should be easy.
    
        (use-package find-file-in-project
          :config
          (global-set-key (kbd "C-c f f") 'find-file-in-project))

22. fira-code

    I use FiraCode, this mode allows us to use ligatures.
    
        (use-package fira-code-mode
          :diminish fira-code-mode)

23. flycheck

    `flycheck` is for all of our linting/code quality needs.
    I prefer pop-ups over mode-line info.
    
        ;; (use-package flycheck-popup-tip)
        (use-package flycheck
          :delight " fly"
          :hook ((flycheck-mode . my-use-eslint-from-node-modules)
        	 (flycheck-mode . my-use-tslint-from-node-modules)
        	 ;; (flycheck-mode . flycheck-popup-tip-mode)
        ))

24. flyspell

    My spelling is bad.
    Use American English for flyspell.
    
        (use-package flyspell
          :delight " fsp"
          :init (setq ispell-dictionary "american")
          :config
          (setq flyspell-issue-message-flag nil))
        
        (use-package flyspell-correct
          :after flyspell
          :bind (:map flyspell-mode-map ("C-c ä" . flyspell-correct-wrapper)))
        
        (use-package flyspell-correct-ivy
          :after flyspell-correct)

25. gitignore-mode

    Syntax highlighting.
    
    Necessary even for `.gitignore` files.
    
        (use-package gitignore-mode
          :mode "^.gitignore")

26. git-timemachine

    If you want to go back in time and point fingers at the progenitors of doom.
    
        (use-package git-timemachine
          :bind ("C-c q" . git-timemachine-toggle))

27. highlight-indent-guides

    Show indentation.
    
        (use-package highlight-indent-guides
          ;; don't need to see this
          :diminish highlight-indent-guides-mode
          :init
          (setq highlight-indent-guides-method 'character)
          :hook (prog-mode . highlight-indent-guides-mode))

28. highlight numbers

    Make numbers stand out.
    
        (use-package highlight-numbers
          :hook (prog-mode . highlight-numbers-mode))

29. hl-todo

    Highlight `TODO`, `FIXME` etc. in prog modes.
    
        (use-package hl-todo
          :hook (prog-mode . hl-todo-mode))

30. hydra

    We use hydra to trigger grouped actions.
    
        (use-package hydra)

31. ivy

    We use `ivy` for narrowing our options.
    `swiper` is an alternative for normal search.
    
        (use-package swiper
          :after ivy
          :chords ((",." . persp-ivy-switch-buffer))
          :bind ("C-c s" . swiper))
        
        (use-package ivy
          :diminish
          :init
          (setq ivy-use-virtual-buffers t)
          (setq enable-recursive-minibuffers t)
          :bind (("C-x b" . persp-ivy-switch-buffer))
          :config
          (ivy-mode 1))
        
        (defun ivy-rich-switch-buffer-icon (candidate)
         (with-current-buffer
              (get-buffer candidate)
            (let ((icon (all-the-icons-icon-for-mode major-mode)))
              (if (symbolp icon)
        	  (all-the-icons-icon-for-mode 'fundamental-mode)
        	icon))))
        
        (use-package ivy-rich
          :after ivy
          :init
          (setq ivy-rich-display-transformers-list
              '(persp-ivy-switch-buffer
        	(:columns
        	 ((ivy-rich-switch-buffer-icon (:width 2))
        	  (ivy-rich-candidate (:width 30))
        	  (ivy-rich-switch-buffer-size (:width 7))
        	  (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
        	  (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
        	  (ivy-rich-switch-buffer-project (:width 15 :face success))
        	  (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
        	 :predicate
        	 (lambda (cand) (get-buffer cand)))))
          :config
          (ivy-rich-mode 1))

32. kaolin

    This is a themes collection I sometimes pick from.
    
        (use-package kaolin-themes
          :init
          (setq kaolin-ocean-alt-bg t)
          ;; Enable distinct background for fringe and line numbers.
          (setq kaolin-themes-distinct-fringe t)
          ;; Enable distinct colors for company popup scrollbar.
          (setq kaolin-themes-distinct-company-scrollbar t)
          :config
          ;; treemacs
          (kaolin-treemacs-theme))

33. kubernetes

    Who doesn't like pods and stuff?
    
        (use-package kubernetes
          :commands (kubernetes-overview))

34. lsp

    Prefer `capf`, bigger delay.
    
    If you use Elixir, get the language server from [here](https://github.com/elixir-lsp/elixir-ls).
    
        ;; you need to install the language server manually and point to the release
        (setq elixir-ls-release-location (expand-file-name "ls/elixir" user-emacs-directory))
        (if (file-exists-p (expand-file-name "language_server.sh" elixir-ls-release-location))
          (add-to-list 'exec-path elixir-ls-release-location)
          (add-hook 'elixir-mode-hook 'lsp))
        
        (use-package lsp-mode
          :init
          (setq lsp-completion-provider :capf)
          (setq lsp-prefer-capf t)
          (setq lsp-idle-delay 1.5)
          ;; (setq lsp-semantic-highlighting t)
          :config
          ;; ignore elixir build and dependency folders
          (add-to-list 'lsp-file-watch-ignored "[/\\\\]_build$")
          (add-to-list 'lsp-file-watch-ignored "[/\\\\]deps$"))
        
        (use-package lsp-ui
          :after lsp-mode
          :hook (lsp-mode . lsp-ui-mode))

35. magit

    Version control has never been this easy before.
    
        (use-package magit
          :bind ("C-c g" . magit-status))

36. mode-line bell

    Make the bell visual.
    
        (use-package mode-line-bell
          :config
          (mode-line-bell-mode))

37. multiple-cursors

    Sometimes a lot of things are similarly wrong.
    It's nice to change everything at once.
    
        (use-package multiple-cursors
          :bind
          (("C-c m n" . mc/mark-next-like-this)
           ("C-c m p" . mc/mark-previous-like-this)
           ("C-c m a" . mc/mark-all-like-this)))

38. mwim

    Let's try this.
    
        (use-package mwim
          :bind (
        	 ("C-a" . mwim-beginning)
        	 ("C-e" . mwim-end)))

39. origami

    Code folding. Unfortunately has some performance issues.
    
        (when (version< emacs-version "27.0")
          (use-package origami
            :hook (prog-mode . origami-mode)
            :chords ("ww" . origami-toggle-node)
            :bind (("C-c o" . origami-toggle-node))))

40. perspective

    Have some perspective, man.
    
        (use-package perspective
          :init
          (setq persp-state-default-file (expand-file-name ".cache/persp-persist" user-emacs-directory))
          (setq persp-mode-prefix-key (kbd "C-c p"))
          :config
          (persp-mode))
        
        ;; no idea why putting this in :hook kills the package
        (add-hook 'kill-emacs-hook #'persp-state-save)

41. prettier-js

    Format code quickly.
    
        (use-package prettier-js
          :init
          ;; you might want to remove/edit this
          (setq prettier-js-args '(
            "--print-width" "91"
          )))

42. projectile

    Projects in Emacs.
    
        (use-package projectile
          :init
          ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
          ;; (add-to-list 'projectile-globally-ignored-directories "build")
        )

43. rainbow

    Show colors in source code and make delimiters stand out.
    
        (use-package rainbow-delimiters
          :hook (prog-mode . rainbow-delimiters-mode))
        
        (use-package rainbow-mode
          :diminish
          :hook (prog-mode . rainbow-mode))

44. restart-emacs

    Sometimes I restart for fun.
    
        (use-package restart-emacs
          :init
          (setq restart-emacs-restore-frames t)
          :bind ("C-c r s" . restart-emacs))

45. restclient

    Postman is passé.
    I use a `.http` file extension for my request examples.
    
        (use-package restclient
          :mode ("\\.http\\'" . restclient-mode))

46. request

    Not used yet, but will in the future.
    
        (use-package request)

47. s

    String manipulation utility.
    
        (use-package s)

48. smartparens

    Create a pairs automatically.
    
        (use-package smartparens
          :diminish smartparens-mode
          :init
          (require 'smartparens-config)
          :hook (prog-mode . smartparens-mode))

49. smeargle

    Highlight sections by edit date.
    
        ;; make it toggle
        (defvar smeargle-on nil)
        
        (defun smeargle-toggle ()
          (interactive)
          (if smeargle-on
            (progn
              (setq smeargle-on nil)
              (smeargle-clear))
            (progn
              (setq smeargle-on t)
              (smeargle))))
        
        (use-package smeargle)

50. smex

    Show completions for `M-x` in a buffer.
    
        (use-package smex
          :bind ("M-x" . smex))

51. so-long

    This mode is included in Emacs > 27. Still using 26 here.
    
        (use-package so-long
          :config
          (global-so-long-mode 1))

52. symon

    Show some system stats when nothing else is going on.
    
        (use-package symon
          :init
          (setq symon-sparkline-type 'bounded
        	symon-delay 10
        	symon-monitors
        	  '(symon-linux-cpu-monitor
        	    symon-linux-memory-monitor
        	    symon-linux-network-rx-monitor
        	    symon-linux-network-tx-monitor))
          :config
          (symon-mode))

53. telephone-line

    A slightly nicer mode-line.
    
        (use-package telephone-line
          :init
          (setq telephone-line-lhs
            '((evil   . (telephone-line-buffer-segment))
              (accent . (telephone-line-vc-segment))
              (nil    . (telephone-line-minor-mode-segment
        		 telephone-line-process-segment))))
          (setq telephone-line-rhs
            '((nil    . (telephone-line-misc-info-segment
        		 telephone-line-flycheck-segment))
              (accent . (telephone-line-major-mode-segment))
              (evil  . (telephone-line-airline-position-segment))))
          (setq telephone-line-primary-right-separator 'telephone-line-identity-left
        	telephone-line-secondary-right-separator 'telephone-line-identity-hollow-left
        	telephone-line-primary-left-separator 'telephone-line-identity-right
        	telephone-line-secondary-left-separator 'telephone-line-identity-hollow-right)
          :config
          (telephone-line-mode t))

54. treemacs

    I'm not a fan of `dired`, so let's show some <span class="underline">dirs</span>.
    
        (use-package treemacs
          :defer t
          :init
          (with-eval-after-load 'winum
            (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
          :config
          (progn
            (setq treemacs-indentation                   1
        	  treemacs-width                         35
        	  treemacs-position                      'left
        	  treemacs-move-forward-on-expand        t
        	  treemacs-follow-after-init             nil
        	  treemacs-indentation-string            " ⁝ "
        	  treemacs-is-never-other-window         t
        	  treemacs-no-delete-other-windows       nil
        	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        	  treemacs-show-hidden-files             t
        	  treemacs-file-event-delay              1000)
        
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
        
            (pcase (cons (not (null (executable-find "git")))
        	       (not (null treemacs-python-executable)))
              (`(t . t)
        	(treemacs-git-mode 'deferred))
              (`(t . _)
        	(treemacs-git-mode 'extended))))
          :bind
            (:map global-map
        	("M-0"       . treemacs-select-window)
        	("C-c t 1"   . treemacs-delete-other-windows)
        	("C-c t t"   . treemacs)
        	("C-c t b"   . treemacs-bookmark)
        	("C-c t M-t" . treemacs-find-tag)))
        
        ;; (use-package treemacs-evil
        ;;   :after treemacs evil)
        
        (use-package treemacs-projectile
          :after treemacs projectile)
        
        ;; (use-package treemacs-icons-dired
        ;;   :after treemacs dired
        ;;   :config (treemacs-icons-dired-mode))
        
        (use-package treemacs-magit
          :after treemacs magit)
        
        ;; this supposedly works with perspective but it fails
        ;; (use-package treemacs-persp
        ;;   :after treemacs persp-mode
        ;;   :config (treemacs-set-scope-type 'Perspectives))
        
        ;; start with treemacs open
        ;; (treemacs)

55. undo-fu

    Undoing un-undoing is weird in Emacs.
    
        (use-package undo-fu
          :init
          (global-unset-key (kbd "C-z"))
          :bind ("C-z"   . undo-fu-only-undo)
        	("C-S-z" . undo-fu-only-redo))

56. use-package-ensure-system-package

    Ensure binaries.
    
        (use-package use-package-ensure-system-package)

57. which-key

    Show the next possible key presses towards an action.
    
        (use-package which-key
          :delight " wk"
          :init
          (setq which-key-idle-delay 0.8)
          :config
          (which-key-mode))

58. writeroom-mode

    Create a room of one's own.
    I use a different (light) theme here.
    
        (use-package writeroom-mode
          :hook ((writeroom-mode-enable . (lambda() 
        				      (disable-theme my-dark-emacs-theme)
        				      (load-theme my-light-emacs-theme t)
        				       ))
        	 (writeroom-mode-disable . (lambda()
        				     (disable-theme my-light-emacs-theme)
        				     (load-theme my-dark-emacs-theme t))))
          :bind ("C-c w" . writeroom-mode))

59. yasnippet

    Use snippets in prog mode buffers.
    Because I also use company, `yas-expand` is mapped to `C-c y`.
    
        (use-package yasnippet-snippets
          :after yasnippet
          :config
          (yas-reload-all))
        
        (use-package yasnippet
          :delight " yas"
          :init
          (define-key yas-minor-mode-map (kbd "<tab>") nil)
          (define-key yas-minor-mode-map (kbd "TAB") nil)
          (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
          ;; :config
          ;; (add-hook 'company-mode-hook (lambda ()
          ;;   (substitute-key-definition 'company-complete-common
          ;;                              'company-yasnippet-or-completion
          ;;                               company-active-map)))
          :hook (prog-mode . yas-minor-mode))
        
        ;; (defun company-yasnippet-or-completion ()
        ;;   (interactive)
        ;;   (let ((yas-fallback-behavior nil))
        ;;     (unless (yas-expand)
        ;;       (call-interactively #'company-complete-common))))

60. zoom

    Use the golden ratio between (in-)active buffers.
    
        (use-package zoom
         :diminish
         :init 
         (custom-set-variables
           '(zoom-size '(0.618 . 0.618)))
         :config
         (zoom-mode 1))


<a id="org71a33f0"></a>

### Mode Configs

Configure modes.

1.  angular mode

    You might think Angular is dead and you'd be right but not everyone knows yet.
    
        (use-package angular-mode
          :mode ("\\.component.css\\'" . css-mode)
          :init
          ;; adapt, obviouisly
          (setq lsp-clients-angular-language-server-command
            '("node"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules/@angular/language-server"
              "--ngProbeLocations"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules"
              "--tsProbeLocations"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules"
              "--stdio")))

2.  lisp mode

    Enable `flycheck`.
    
        (defun my-elisp-mode-hook ()
          "Hooks for lisp interaction mode."
          (flycheck-mode 1))
        
        (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

3.  css mode

    Just activate flycheck and tabs for now.
    
        (defun my-css-mode-hook ()
          "Hooks for css mode."
          (add-node-modules-path)
          (enable-tabs)
          (flycheck-mode))
        
        (add-hook 'css-mode-hook 'my-css-mode-hook)

4.  dockerfile mode

    Make `Dockerfiles` look nice.
    
        (use-package dockerfile-mode
          :mode "^Dockerfile")

5.  elixir mode

    Enable `flycheck`.
    
        (use-package elixir-mode
          :hook (elixir-mode . my-elixir-mode-hook))
        
        (defun my-elixir-mode-hook ()
          "Hooks for elixir mode."
          (lsp)
          (flycheck-mode))

6.  js2 mode

    Enable `flycheck` and disable internal checker.
    
        (use-package js2-mode
          :mode "\\.js\\'"
          :init
          (setq-default js2-show-parse-errors nil)
          (setq-default js2-strict-missing-semi-warning nil)
          :hook (js2-mode . my-js2-mode-hook))
        
        (defun my-js2-mode-hook ()
          "Hooks for js2 mode."
          (enable-tabs)
          (add-node-modules-path)
          (flycheck-mode 1)
          (rainbow-delimiters-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )

7.  markdown mode

    Markdown. Sometimes you need it.
    
        (use-package markdown-mode)

8.  org mode

    Org mode is the best thing about Emacs. Check out the [manual](https://orgmode.org/manual/).
    
    1.  The Mode Itself
    
        Use bullets mode and make the ellipses bendy arrows. When a `TODO` is `DONE`, log a note.
        We also make the sequence from `TODO` to `DONE` more granular and add another `DONE`-like
        state `CANCELLED`.
        
            (use-package org-bullets
              :hook (org-mode . (lambda() (org-bullets-mode t))))
            
            ;; use org-mode for presentations
            (use-package org-present)
            
            ;; change if necessary
            (defconst my-org-directory (expand-file-name "org" "~"))
            (unless (file-directory-p my-org-directory)
              (make-directory my-org-directory))
            
            (use-package org
              ;; disable drag-stuff-mode in org-mode
              :hook (org-mode . (lambda() (drag-stuff-mode -1)))
              :config
              ;; sometimes md export is missing
              (require 'ox-md nil t)
              :init
              (setq org-ellipsis "↷"
            	org-log-done t
            	org-startup-truncated nil
            	org-startup-folded 'showeverything
            	org-directory my-org-directory
            	org-default-notes-file (concat org-directory "/notes.org")
            	org-startup-with-inline-images t
            	org-todo-keywords
            	'((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
              (add-to-list 'org-global-properties
            	       '("Effort_ALL". "30m 1h 2h 4h 6h 1d 2d")))
            
            (setq org-log-done 'note
                  org-clock-idle-time nil
                  org-clock-continuously nil
                  org-clock-persist t
                  org-clock-in-switch-to-state "IN PROGRESS"
                  org-clock-in-resume nil
                  org-clock-report-include-clocking-task t
                  org-clock-out-remove-zero-time-clocks t
                  ;; Too many clock entries clutter up a heading
                  org-log-into-drawer t
                  org-clock-into-drawer 1)
            
            (require 'org-install)
            (setq org-modules '(org-habit org-info))
            (org-load-modules-maybe t)
            (setq org-habit-graph-column 105)
            
            ;; this doesn't seem to affect anything
            (setq org-archive-subtree-save-file-p t)
            
            (defun org-make-habit()
                (interactive)
                (org-set-property "STYLE" "habit"))
            
            (setq org-use-speed-commands t
                  org-speed-commands-user
                  '(
            	("w" widen)
            	("n" org-narrow-to-subtree)
            	;; defaults are I and O
            	("i" org-clock-in)
            	("o" org-clock-out)
            	("a" org-archive-subtree)
            	("r" org-clock-report)))
            
            (setq org-tag-alist '(
              ;; depth
              ("@immersive" . ?i)
              ("@shallow"   . ?s)
              ;; context
              ("@work"      . ?w)
              ("@home"      . ?h)
              ("@away"      . ?a)
              ("@repeated"  . ?r)
              ;; time
              ("@short"     . ?<)
              ("@medium"    . ?=)
              ("@long"      . ?>)
              ;; energy
              ("@easy"      . ?1)
              ("@average"   . ?2)
              ("@challenge" . ?3)
              ;; category
              ("@dev"       . ?d)
              ("@bla"       . ?b)
              ("@edu"       . ?e)
            ))
    
    2.  Agendas
    
        Everything concerning agendas.
        This is mostly based on [mwfogleman](https://github.com/mwfogleman/.emacs.d/blob/master/michael.org)'s emacs config.
        
            (use-package org-super-agenda
              :init
              (org-super-agenda-mode)
              (defun my-org-super-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Schedule"
            		    :time-grid t)
            	     (:discard (:anything t))
            	     )))
                  (org-agenda nil "a")))
            
              (defun my-org-super-agenda-today ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Schedule"
            	      :time-grid t)
            	     (:name "Unscheduled"
            	      :scheduled nil)
            	     (:name "Leftovers"
            	      :and (
            		:todo ("IN PROGRESS" "WAITING")
            		:scheduled past
            		:not (:tag "@repeated")))
            	     (:discard (:anything t))
            	      )))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (defun my-personal-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:discard (:tag ("@work"))))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (bind-keys ("C-c a t" . my-org-super-agenda-today)
            	     ("C-c a p" . my-personal-agenda)
            	     ("C-c a w" . my-org-super-agenda)))
            
            (setq org-agenda-hide-tags-regexp "@")
            
            ;; I put all of my tasks into a subfolder `tasks` inside the org directory
            (defconst my-agenda-tasks-directory
              (expand-file-name "tasks" org-directory)
              "One-size-fits-all directory for agenda tasks.")
            (unless (file-directory-p my-agenda-tasks-directory)
              (make-directory my-agenda-tasks-directory))
            (setq org-agenda-files `(,my-agenda-tasks-directory))
            
            ;; more cool stuff rom mwfogleman's emacs.d
            (defhydra hydra-org-clock (:color blue :hint nil)
              "
            Clock   In/out^     ^Edit^   ^Summary     (_?_)
            -----------------------------------------
            	_i_n         _e_dit   _g_oto entry
            	_c_ontinue   _q_uit   _d_isplay
            	_o_ut        ^ ^      _r_eport
                  "
              ("i" org-clock-in)
              ("o" org-clock-out)
              ("c" org-clock-in-last)
              ("e" org-clock-modify-effort-estimate)
              ("q" org-clock-cancel)
              ("g" org-clock-goto)
              ("d" org-clock-display)
              ("r" org-clock-report)
              ("?" (org-info "Clocking commands")))
            
            (defhydra hydra-org-agenda-clock (:color blue :hint nil)
              "
            Clock   In/out^
            -----------------------------------------
            	_i_n
            	_g_oto entry
            	_o_ut
            	_q_uit
                  "
              ("i" org-agenda-clock-in)
              ("o" org-agenda-clock-out)
              ("q" org-agenda-clock-cancel)
              ("g" org-agenda-clock-goto))
            
            (bind-keys :map org-mode-map
            	   ("C-c h" . hydra-org-clock/body)
            	   :map org-agenda-mode-map
            	   ("C-c h" . hydra-org-agenda-clock/body))

9.  python mode

    Enable `flycheck`.
    This mode is built-in.
    
        (defun my-python-mode-hook ()
          "Hooks for python mode."
          (flycheck-mode 1)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )
        
        (add-hook 'python-mode-hook 'my-python-mode-hook)

10. rjsx mode

    Pretty much like js2.
    
        (use-package rjsx-mode
          :mode "\\.jsx\\'"
          :hook (rjsx-mode . my-rjsx-mode-hook))
        
        (defun rjsx-indent ()
          (interactive)
          (setq-local indent-line-function 'js-jsx-indent-line)
        )
        
        (defun my-rjsx-mode-hook ()
          "Hooks for rjsx mode."
          (add-node-modules-path)
          (enable-tabs)
          (flycheck-mode)
          (rjsx-indent)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )

11. typescript mode

    Enable `lsp`, `flycheck`.
    
        (use-package typescript-mode
          :mode "\\.ts\\'"
          :hook (typescript-mode . my-typescript-mode-hook))
        
        (defun my-typescript-mode-hook ()
          "Hooks for typescript mode."
          (enable-tabs)
          (add-node-modules-path)
          (flycheck-mode 1)
          (lsp)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

12. web mode

    Web mode uses `flycheck`,
    prompts user if `lsp` should be enabled.
    
        (use-package web-mode
          :hook (web-mode . my-web-mode-hook)
          :init
          (setq web-mode-comment-style 2)
          :mode ("\\.vue\\'"
        	 "\\.component.html\\'"
        	 "\\.ejs\\'"))
        
        (defun my-web-mode-hook ()
          "Hooks for web mode."
          (enable-tabs)
          (web-mode-use-tabs)
          (add-node-modules-path)
          (if (y-or-n-p "Do you want to enable lsp?")
        	(lsp))
          (flycheck-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

13. yaml mode

    Sometimes you need YAMLs.
    
        (use-package yaml-mode)


<a id="orgf8076d1"></a>

### Tweaks

Some things don't always work out-of-the-box.

1.  Finding ESLint

    ESLint configs can be found using a file, not a directory.
    
        (defun flycheck-eslint-config-exists-p ()
          "Whether there is a valid eslint config for the current buffer."
          (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
        	 (exitcode (and executable (call-process executable nil nil nil
        						 "--print-config" ".eslintrc"))))
            (eq exitcode 0)))

2.  Switch Between Language Server and TSLint

    This is not good code, but `lsp` doesn't play nice with `tslint`.
    
        (defun switch-to-tslint ()
          (lsp-disconnect)
          (setq flycheck-checker 'typescript-tslint))
        
        (defun switch-back-to-lsp ()
          (lsp)
          (setq flycheck-checker 'lsp))
        
        (defun tslint ()
          (interactive)
          (if (bound-and-true-p lsp-mode)
              (switch-to-tslint)
            (switch-back-to-lsp)))

3.  Loading ESLint/TSLint

    Use the locally installed `eslint` and `tslint` binaries.
    
        (defun my-use-eslint-from-node-modules ()
          (let* ((root (locate-dominating-file
        		(or (buffer-file-name) default-directory)
        		"node_modules"))
        	 (eslint
        	  (and root
        	       (expand-file-name "node_modules/.bin/eslint"
        			       root))))
            (when (and eslint (file-executable-p eslint))
              (setq-local flycheck-javascript-eslint-executable eslint))))
        
        (defun my-use-tslint-from-node-modules ()
          (let* ((root (locate-dominating-file
        		(or (buffer-file-name) default-directory)
        		"node_modules"))
        	 (tslint
        	  (and root
        	       (expand-file-name "node_modules/.bin/tslint"
        				 root))))
            (when (and tslint (file-executable-p tslint))
              (setq-local flycheck-typescript-tslint-executable tslint))))


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> I built version `27.1` from source and tested the config a bit.
Apart from two packages that will be disabled for any `27.x` version,
it seem to work fine.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> What you're reading is likely a markdown version exported from it.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> **Note** that this config uses the `all-the-icons` package
whose icons need to be downloaded manually
by running `M-x all-the-icons-install-fonts` and selecting `yes`.

**Note** that this config uses `dash`.
The config will try to install it before installing the other packages
but this might fail.
If that is the case do the following:

-   hit `M-x`, type `package-install` and hit return
-   type `dash` and hit return again
-   once the installation is complete, re-run Emacs

<sup><a id="fn.4" href="#fnr.4">4</a></sup> If you're not sure where your `user-emacs-directory` might be,
you can do the following:

-   run Emacs
-   hit `M-x` (that is your Alt/Option key followed by the letter `x`)
-   type `describe-variable` and hit return
-   type `user-emacs-directory` and hit return again

A window (or is it a frame?) should pop up telling you the path

Finally run `git clone git@gitlab.com:Walheimat/emacs-config.git ~/.emacs.d`
(replace `~/.emacs.d` with your actual path if it differs)

<sup><a id="fn.5" href="#fnr.5">5</a></sup> Send me an email, why don't you?

<sup><a id="fn.6" href="#fnr.6">6</a></sup> Repositories (incomplete):

-   [ag](https://github.com/Wilfred/ag.el)
-   [all-the-icons](https://github.com/domtronn/all-the-icons.el)
-   [avy](https://github.com/abo-abo/avy)
-   [bm](https://github.com/joodland/bm)
-   [crux](https://github.com/bbatsov/crux)
-   [dap](https://github.com/emacs-lsp/dap-mode)
-   [ivy-rich](https://github.com/Yevgnen/ivy-rich)
-   [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
-   [mwim](https://github.com/alezost/mwim.el)
-   [org-present](https://github.com/rlister/org-present)
-   [org-super-agenda](https://github.com/alphapapa/org-super-agenda)
-   [symon](https://github.com/zk-phi/symon)
-   [telephone-line](https://github.com/dbordak/telephone-line)
-   [treemacs](https://github.com/Alexander-Miller/treemacs)
-   [use-package](https://github.com/jwiegley/use-package)
-   [which-key](https://github.com/justbur/emacs-which-key)
-   [yasnippet](https://github.com/joaotavora/yasnippet)
