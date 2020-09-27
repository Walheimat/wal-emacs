

# emacs config

This project is my personal Emacs (26.3) config.

Its base is an org file so it doubles as a readme<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>.


# Table of Contents

1.  [emacs config](#org043a737)
    1.  [setup](#org81c47fd)
    2.  [config](#org7df2c78)


<a id="org81c47fd"></a>

## setup

If you're interested in trying out Emacs using my config, here are the necessary steps:

-   Install Emacs if you haven't
-   Clone this repository into your `user-emacs-directory`:
    
    If you're not sure where your `user-emacs-directory` might be, you can do the following:
    
    -   run Emacs
    -   hit `M-x` (that is your Alt/Option key followed by the letter `x`)
    -   type `describe-variable` and hit return
    -   type `user-emacs-directory` and hit return again
    
    A window (or is it a frame?) should pop up telling you the path
    
    Finally run `git clone git@gitlab.com:Walheimat/emacs-config.git ~/.emacs.d`
    (replace `~/.emacs.d` with your actual path if it differs)

-   Copy the config file included in this repo by running `cp ~/.emacs.d/emacs-config/.emacs.example ~/.emacs`
    
    Adapt the path again if necessary

-   Close and re-run Emacs which should download and install (almost) all packages

If you did not init this repo in your `user-emacs-directory` using the default name, you will need to adapt
the variable `walheimat-emacs-config-default-path` in the example config you just copied.

**Note** that this config uses the `all-the-icons` package whose icons need to be downloaded manually
by running `M-x all-the-icons-install-fonts` and selecting `yes`.

**Note** that this config uses `dash`. The config will try to install it before installing the other packages
but this has failed before. If that is the case do the following:

-   hit `M-x`, type `package-install` and hit return
-   type `dash` and hit return again
-   once the installation is complete, re-run Emacs


<a id="org7df2c78"></a>

## config

Configure Emacs. The init script will evaluate <span class="underline">everything</span> that follows.


### personal

Set some personal info.

    (setq user-full-name "Krister Schuchardt"
          user-mail-address "krister.schuchardt@theventury.com")


### before init

Set up emacs, package manager and packages.

1.  general

    No splash. Use separate file for customizations (so we don't clutter up our init file). Use python3.
    
        (setq inhibit-startup-message t)
        ;; (setq initial-scratch-message nil)
        
        ;; custom.el
        (setq custom-file (expand-file-name "custom.el" walheimat-emacs-config-default-path))
        (unless (file-exists-p custom-file)
          (write-region "" nil custom-file))
        (load custom-file)
        
        ;; python
        (setq py-python-command "python3")

2.  MELPA

    Initialize MELPA.
    
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

3.  dependencies

    Define and install packages (if they're missing).
    
        ;; we need dash for the upcoming loop
        (unless (package-installed-p 'dash)
          (package-install 'dash))
        (require 'dash)
        
        ;; we need diminish and delight for use-package
        (unless (package-installed-p 'diminish)
          (package-install 'diminish))
        (require 'diminish)
        (unless (package-installed-p 'delight)
          (package-install 'delight))
        (require 'delight)
        
        ;; install use-package next
        (unless (package-installed-p 'use-package)
          (package-install 'use-package))
        ;; always ensure
        (require 'use-package-ensure)
        (setq use-package-always-ensure t)
        
        (defun packages-install (packages)
          (--each packages
            (when (not (package-installed-p it))
              (package-install it)))
          (delete-other-windows))
        
        ;; packages that either don't need configuration
        ;; or are requirements in and of themselves are put here
        (defun init--install-packages ()
          (packages-install
           '(
             ample-theme
             doom-themes
             kaolin-themes
             naysayer-theme
             ;; nodejs-repl
             nord-theme
             panda-theme
             spacemacs-theme
             )))
        
        (condition-case nil
            (init--install-packages)
          (error
            (package-refresh-contents)
            (init--install-packages)))

4.  site-lisp

    Add side lisp directory and subdirs to load path. I put non-MELPA packages here.
    
        ;; create the dir if it does not exist to avoid error
        (unless (file-directory-p (expand-file-name "site-lisp" user-emacs-directory))
          (make-directory (expand-file-name "site-lisp" user-emacs-directory)))
        (setq site-lisp-dir
          (expand-file-name "site-lisp" user-emacs-directory))
        (add-to-list 'load-path site-lisp-dir)
        (dolist (project (directory-files site-lisp-dir t "\\w+"))
          (when (file-directory-p project)
            (add-to-list 'load-path project)))


### global

Configure global settings.

1.  save place

    Save places, and do so in a file.
    
        (setq save-place-file (expand-file-name ".places" user-emacs-directory))

2.  autosave and backups

    Store backups in backups folder. Store autosaves in temp folder. We don't want this to mess with git.
    
        (setq backup-directory-alist
          `(("." . ,(expand-file-name
            (concat user-emacs-directory "backups")))))
        (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
        (setq create-lockfiles nil)

3.  global modes

    Turn on a lot of useful (and prettifying) modes.
    
        (show-paren-mode 1)
        (global-auto-revert-mode t)
        (global-hl-line-mode)
        (add-hook 'after-init-hook 'global-company-mode)
        (add-hook 'prog-mode-hook 'highlight-numbers-mode)
        (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
        (add-hook 'prog-mode-hook 'linum-mode)
        ;; (global-display-line-numbers-mode)
        (ivy-mode 1)
        (global-prettify-symbols-mode +1)
        (global-diff-hl-mode)
        (dimmer-mode t)
        ;; (global-whitespace-mode)
        (save-place-mode 1)
        (dumb-jump-mode)
        (which-key-mode)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (zoom-mode 1)
        (beacon-mode 1)
        (global-font-lock-mode 1)
        (mode-line-bell-mode)
        (eshell-syntax-highlighting-global-mode)
        (symon-mode)

4.  reasonable

    settings
    Insertion of text should delete region. Bracket pairs should be highlighted.
    Window (or frame &#x2026;) should start maximized. Garbage collection and memory.
    
        (setq mouse-yank-at-point t)
        (setq show-paren-delay 0.0)
        (setq gc-cons-threshold 100000000)
        (setq read-process-output-max (* 1024 1024)) ;; 1mb
        (setq sentence-end-double-space nil)
        (setq echo-keystrokes 0.1)
        (delete-selection-mode 1)
        (add-to-list 'default-frame-alist '(fullscreen . maximized))
        (defalias 'yes-or-no-p 'y-or-n-p)
        (defun my-font-lock-hook ()
          "Slanted and enchanted."
          (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
          (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
        )
        (add-hook 'font-lock-mode-hook 'my-font-lock-hook)

5.  tabs all the way

    Tabs are 4 spaces wide. No electric indent. Pipe char to show indentation.
    Commands to enable/disable sane tabs.
    
        (setq custom-tab-width 4)
        
        (defun disable-tabs ()
          (interactive)
          (setq indent-tabs-mode nil))
        (defun enable-tabs  ()
          (interactive)
          (local-set-key (kbd "TAB") 'tab-to-tab-stop)
          (setq indent-tabs-mode t)
          (setq tab-width custom-tab-width))
        
        (setq-default python-indent-offset custom-tab-width) ;; Python
        (setq-default js-indent-level custom-tab-width)      ;; Javascript
        
        (setq-default electric-indent-inhibit t)
        
        (setq backward-delete-char-untabify-method 'hungry)

6.  key bindings

    Change up the key bindings a bit.
    
    -   `C-c e` opens eshell.
    -   `C-x g` opens magit status.
    -   `M-x` opens smex.
    -   `s-,` (un-)comments.
    -   `C-x p a` runs ag. <span class="underline">Requires ag</span>!
    -   `C-x r q` (really) quits.
    -   `C-x C-c` opens this config org file.
    -   `M-o` goes to the "other" window or the last buffer.
    -   `C-x j` dumb-jumps.
    -   `C-x t m` opens the timemachine.
    -   `s-y` runs flyspell.
    -   `C-x p f` finds a project file.
    -   `C-c k` kills all other buffers.
    -   `C-c o` opens file with outside program.
    -   `s-RET` will open a (indented) line above.
    -   `s-k` kills the whole line.
    -   `C-c d` duplicates the current line (or region).
    -   `C-x 4 t` transposes windows (watch out for treemacs).
    -   `C-d d` opens docker.
    -   `C-+` expands region.
    -   `C-z=/=C-S-z` undos/redos.
    -   `C-ö` jumps to char with avy.
    -   `C-ä` jumps to line with avy.
    -   `C-s` uses swiper to search.
    -   `C-;` use iedit.
    
    Note that a lot of these are defined in the [packages](#orgb742491) section.
    
        (global-set-key (kbd "C-c e") 'eshell)
        (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
        (global-set-key
          (kbd "C-x C-c")
          (lambda () (interactive)(switch-to-buffer (find-file-noselect (expand-file-name "configuration.org" walheimat-emacs-config-default-path)))))

7.  theme

    Be sure to check out [Peach Melpa](https://peach-melpa.org/) to find a theme you like.
    
        (load-theme 'nord t)

8.  font size

    Prefer FiraCode (-> mononoki -> Liberation -> DejaVu). If emacs runs with the custom arg `-bigger`, the default font size is 14 (instead of 10).
    
    To get support for ligatures, install the symbol font from [here](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip).
    
        (require 'dash)
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
        
        ;; this requires you to have installed iosevka
        (if (found-custom-arg "-iosevka")
          (set-default-font "Iosevka 12")
        )
        
        ;; use fira mode if it's the default font and the symbol font is installed
        (use-package fira-code-mode
          :if (and (x-list-fonts "Fira Code Symbol") (string= "Fira Code" (face-attribute 'default :family)))
          :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
          :hook prog-mode)                                         ; mode to enable fira-code-mode in

9.  fun stuff

    Zone out after a minute.
    
        (require 'zone)
        (zone-when-idle 180)

10. func stuff

    Add some functions.
    
        ;; check if buffer is treemacs buffer
        ;; similar to minibufferp
        (defun treemacsbufferp ()
          "Check if this is the treemacs buffer."
          (eq (current-buffer) (treemacs-get-local-buffer)))


### mode mappings

Set up mode mappings for built-in modes.

    (add-to-list 'auto-mode-alist '("\\.component.css" . css-mode))


<a id="orgb742491"></a>

### packages

What follows is a list of MELPA packages that make Emacs even more awesome.

If you wish to know more about any of them, check out the list<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup> of repositories
at the end of this readme/configuration.

Many packages bind keys. Check the [key bindings section](#org7c93f64) if you need a list of all
of them.

1.  add-node-modules-path

        (use-package add-node-modules-path)

2.  all-the-icons

        (use-package all-the-icons)

3.  ag

    Highlight search results using the **Silver Searcher**.
    
    This <span class="underline">requires</span> the `ag` binary which you can get from [here](https://github.com/ggreer/the_silver_searcher#installation).
    
        (use-package ag
          :config
          (setq ag-highlight-search t)
          :bind ("C-x p a" . ag-project))

4.  avy

    Jumping to (visible) lines and chars is fun if you are to lazy to use your mouse.
    
        (use-package avy
          :bind (("C-ö" . avy-goto-char)
        	 ("C-ä" . avy-goto-line)))

5.  beacon

        (use-package beacon)

6.  company

    Set up company-box
    
        (use-package company-box)
        (use-package company
          :delight " co"
          :diminish company-box-mode
          :config 
          (setq company-minimum-prefix-length 3)
          (setq company-idle-delay 0.5)
          ;; :after (diminish company-box)
          :hook (company-mode . company-box-mode))
        (use-package company-lsp
          :after company)
        (use-package company-restclient
          :after company)
        (use-package company-web
          :after company)

7.  crux

    Let's use `crux` for some editing magic. Check the [key bindings section](#org7c93f64) for descriptions.
    
        (use-package crux
          :bind (("M-o"        . crux-other-window-or-switch-buffer)
        	 ("C-c k"      . crux-kill-other-buffers)
        	 ;; need to find solution with treemacs open
        	 ;; ("C-x 4 t")   .crux-transpose-windows)
        	 ("C-c o"      . crux-open-with)
        	 ("s-<return>" . crux-smart-open-line-above)
        	 ("s-k"        . crux-kill-whole-line)
        	 ("C-c d"      . crux-duplicate-current-line-or-region)))

8.  docker

    Use `docker` package with `C-x d`.
    
        (use-package docker
          :bind ("C-x d" . docker))

9.  dap

    Debugging using VSCode's DAP. Register a template for attaching to
    a docker host.
    
        (use-package dap-mode
          :delight " dap"
          :init
            (require 'cl)
          :config
            ;; (setq dap-auto-configure-features '(sessions locals breakpoints))
            (require 'dap-node)
            (require 'dap-python)
            (setq dap-python-executable "python3")
            (dap-auto-configure-mode 1)
            (dap-register-debug-template
              "Node::Attach"
              (list :type "node"
        	    :request "attach"
        	    :remoteRoot "/usr/src/app"
        	    :localRoot "/home/krister/theventury"
        	    :port 9229
        	    :name "Node::Attach"))
           :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

10. delight

    Refine a couple of major-mode names.
    
        (delight 'js2-mode "js" :major)
        (delight 'emacs-lisp-mode "elisp" :major)

11. diff-hl

    Refresh post magit.
    
        (use-package diff-hl
          :hook (magit-post-refresh  . diff-hl-magit-post-refresh))

12. diminish

    See individual `use-package` declarations as well, since we delight in/diminish them there.
    
        (diminish 'eldoc-mode)

13. dimmer

    Make dimmed frames a bit dimmer.
    
        (use-package dimmer
          ;; :delight " dimmer"
          :config
          (setq dimmer-fraction 0.3)
          (dimmer-configure-org)
          (dimmer-configure-magit)
          (dimmer-configure-hydra)
          (setq dimmer-adjustmentmode :both))

14. drag stuff

    Use the default key bindings.
    
        (use-package drag-stuff
          :delight " drag"
          :config
          (drag-stuff-define-keys))

15. dumb-jump

    Use ivy. We have ivy.
    
        ;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
        (use-package dumb-jump
          :config
          (setq dumb-jump-selector 'ivy
        	dumb-jump-force-searcher 'ag)
          :bind ("C-x j" . dumb-jump-go))

16. eshell

    Set up eshell.
    
        (use-package esh-autosuggest)
        (use-package eshell-prompt-extras)
        (use-package eshell-syntax-highlighting)
        
        (defun setup-eshell-ivy-completion ()
          (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point))
        
        (defun my-eshell-mode-hook ()
          "Hooks for eshell mode."
          (esh-autosuggest-mode)
          (setup-eshell-ivy-completion))
        
        (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)
        (with-eval-after-load "esh-opt"
          (autoload 'epe-theme-lambda "eshell-prompt-extras")
          (setq eshell-highlight-prompt nil
        	eshell-prompt-function 'epe-theme-lambda))

17. evilnc

    Comment code like in `vim`, evil, evil `vim`.
    
        (use-package evil-nerd-commenter
          :bind ("s-," . evilnc-comment-or-uncomment-lines))

18. expand-region

    One thing that can be a bit tricky is selecting regions, not anymore.
    
        (use-package expand-region
          :bind ("C-+" . er/expand-region))

19. find-file-in-project

        (use-package find-file-in-project
          :config
          (global-set-key (kbd "C-x p f") 'find-file-in-project))

20. flycheck

    Show flycheck suggestions in popup tip.
    
        (use-package flycheck-popup-tip)
        (use-package flycheck
          :delight " fly"
          :hook ((flycheck-mode . flycheck-popup-tip-mode)
        	 (flycheck-mode . my/use-eslint-from-node-modules)
        	 (flycheck-mode . my/use-tslint-from-node-modules)))

21. fira-code

    Hide it.
    
        (use-package fira-code-mode
          :diminish fira-code-mode)

22. git-timemachine

    If you want to go back in time and point fingers at the progenitors of doom.
    
        (use-package git-timemachine
          :bind ("C-x t m" . git-timemachine-toggle))

23. hydra

    Not used yet (but dependency of other packages).
    
        (use-package hydra)

24. iedit

    Edit multiple occurrences at once.
    
        (use-package iedit)

25. flyspell

    Use American English for flyspell.
    
        (use-package flyspell
          :config
          (setq flyspell-issue-message-flag nil)
          :bind ("s-y" . flyspell-prog-mode)
          :hook (flyspell-prog-mode . (lambda() (ispell-change-dictionary "american"))))

26. highlight-indent-guides

    While this is useful, I don't need to see the minor mode.
    
        (use-package highlight-indent-guides
          :diminish highlight-indent-guides-mode
          :init
          (setq highlight-indent-guides-method 'character))

27. higlight numbers

    Make numbers stand out.
    
        (use-package highlight-numbers)

28. ivy

    We use ivy for everything. No longer comes with swiper.
    
        (use-package swiper)
        (use-package ivy
          :diminish
          :after swiper
          :config
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq enable-recursive-minibuffers t)
            (global-set-key (kbd "\C-s") 'swiper))

29. kaolin

    Apply kaolin theme to treemacs.
    
        (use-package kaolin-themes
          :config
          (kaolin-treemacs-theme)
          (setq kaolin-ocean-alt-bg t)
          ;; Enable distinct background for fringe and line numbers.
          (setq kaolin-themes-distinct-fringe t)  
          ;; Enable distinct colors for company popup scrollbar.
          (setq kaolin-themes-distinct-company-scrollbar t))

30. lsp

    Prefer capf, bigger delay, configure for angular.
    
        (use-package lsp-mode
          :config
          (setq lsp-prefer-capf t)
          (setq lsp-idle-delay 0.500)
          (setq lsp-semantic-highlighting t))

31. magit

        (use-package magit
          :bind ("C-x g" . magit-status))

32. modeline bell

        (use-package mode-line-bell)

33. prettier-js

    Require so it can be used outside of minor mode.
    
        (use-package prettier-js
          :config
          (setq prettier-js-args '(
            "--print-width" "91"
          )))

34. projectile

    Projects in Emacs.
    
        (use-package projectile)

35. rainbow

    Show colors in source code and make delimiters stand out.
    
        (use-package rainbow-delimiters)
        (use-package rainbow-mode
          :diminish)

36. restclient

    Postman is passé.
    
        (use-package restclient
          :init
          (add-to-list 'auto-mode-alist '("\\.http" . restclient-mode)))

37. request

    Not used yet, but will in the future.
    
        (use-package request)

38. s

    String manipulation utility.
    
        (use-package s)

39. smartparens

    Configure.
    
        (use-package smartparens
          :diminish smartparens-mode
          :init
          (require 'smartparens-config))

40. smeargle

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
        
        (use-package smeargle
          :bind ("C-x t s" . smeargle-toggle))

41. smex

    Replace normal key binding.
    
        (use-package smex
          :bind ("M-x" . smex))

42. symon

    Make it look nice.
    
        (use-package symon
          :config
          (setq symon-sparkline-type 'bounded
        	symon-monitors
        	  '(symon-linux-cpu-monitor
        	    symon-linux-memory-monitor
        	    symon-linux-network-rx-monitor
        	    symon-linux-network-tx-monitor)))

43. treemacs

    Less indentation. Never other window.
    
        (use-package perspective)
        ;; (use-package treemacs-evil)
        (use-package treemacs
          :defer t
          :init
          (with-eval-after-load 'winum
            (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
          :config
          (progn
            (setq treemacs-indentation                   1
        	  treemacs-width                         35
        	  treemacs-move-forward-on-expand        t
        	  treemacs-follow-after-init             nil
        	  treemacs-indentation-string            " ⁝ "
        	  treemacs-is-never-other-window         t
        	  treemacs-no-delete-other-windows       nil
        	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        	  treemacs-show-hidden-files             t)
            (treemacs-follow-mode nil)
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
        	("C-x t 1"   . treemacs-delete-other-windows)
        	("C-x t t"   . treemacs)
        	("C-x t B"   . treemacs-bookmark)
        	("C-x t C-t" . treemacs-find-file)
        	("C-x t M-t" . treemacs-find-tag)))
        
        ;; (use-package treemacs-evil
        ;;   :after treemacs evil
        ;;   :ensure t)
        
        (use-package treemacs-projectile
          :after treemacs projectile)
        
        (use-package treemacs-icons-dired
          :after treemacs dired
          :config (treemacs-icons-dired-mode))
        
        (use-package treemacs-magit
          :after treemacs magit)
        
        (use-package treemacs-persp
          :after treemacs persp-mode
          :config (treemacs-set-scope-type 'Perspectives))

44. telephone-line

    A slightly nicer modeline.
    
        (use-package telephone-line
          :config
            (setq telephone-line-lhs
        	  '((evil   . (telephone-line-buffer-segment))
        	    (accent . (telephone-line-vc-segment))
        	    (nil    . (telephone-line-minor-mode-segment
        		       telephone-line-erc-modified-channels-segment
        		       telephone-line-process-segment))))
            (setq telephone-line-rhs
        	  '((nil    . (telephone-line-misc-info-segment
        		       telephone-line-flycheck-segment))
        	    (accent . (telephone-line-major-mode-segment))
        	    (evil   . (telephone-line-airline-position-segment))))
            (setq telephone-line-primary-right-separator 'telephone-line-identity-left
        	  telephone-line-secondary-right-separator 'telephone-line-identity-hollow-left
        	  telephone-line-primary-left-separator 'telephone-line-identity-right
        	  telephone-line-secondary-left-separator 'telephone-line-identity-hollow-right))
        (telephone-line-mode t)

45. undo-fu

        (use-package undo-fu
          :init
          (global-unset-key (kbd "C-z"))
          :bind ("C-z" . undo-fu-only-undo)
        	("C-S-z" . undo-fu-only-redo))

46. which-key

    Show me my options.
    
        (use-package which-key
          :delight " wk")

47. yasnippet

    Don't enable globally but prepare for per-buffer use.
    
        (use-package yasnippet-snippets)
        (use-package yasnippet
          :delight " yas"
          :after yasnippet-snippets
          :config
          (yas-reload-all))

48. zoom

    Use the golden ratio.
    
        (use-package zoom
         :diminish
         :config
         (custom-set-variables
           '(zoom-size '(0.618 . 0.618))))


### mode configs

Configure modes.

1.  angular mode

    You might think Angular is dead and you'd be right but not everyone knows yet.
    
        (use-package angular-mode
          :init
          (setq lsp-clients-angular-language-server-command
            '("node"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules/@angular/language-server"
              "--ngProbeLocations"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules"
              "--tsProbeLocations"
              "/home/krister/.config/nvm/12.16.1/lib/node_modules"
              "--stdio")))

2.  lisp mode

    Enable flycheck.
    
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

    Make Dockerfiles look nice.
    
        (use-package dockerfile-mode
          :init
          (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

5.  elixir mode

    Enable flycheck.
    
        (use-package elixir-mode
          :hook (elixir-mode . my-elixir-mode-hook))
        (defun my-elixir-mode-hook ()
          "Hooks for elixir mode."
          (flycheck-mode))

6.  js2 mode

    Enable Flycheck and disable internal checker.
    
        (use-package js2-mode
          :init
          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          :config
          (setq-default js2-show-parse-errors nil)
          (setq-default js2-strict-missing-semi-warning nil)
          :hook (js2-mode . my-js2-mode-hook))
        
        (defun my-js2-mode-hook ()
          "Hooks for js2 mode."
          (enable-tabs)
          (add-node-modules-path)
          (flycheck-mode 1)
          (rainbow-mode)
          (rainbow-delimiters-mode)
          (drag-stuff-mode)
          (smartparens-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )

7.  markdown mode

        (use-package markdown-mode)

8.  org mode

    1.  org mode itself
    
        Use bullets mode and make the ellipses bendy arrows. When a `TODO` is `DONE`, log a note.
        We also make the sequence from `TODO` to `DONE` more granular and add another `DONE`-like
        state `CANCELLED`.
        
            
            (use-package org-bullets
              :hook (org-mode . (lambda() (org-bullets-mode t))))
            (setq org-ellipsis "↷"
                  org-log-done 'note
                  org-directory "~/org/"
                  org-default-notes-file (concat org-directory "notes.org")
                  org-startup-with-inline-images t
                  org-todo-keywords
                  '((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
            (require 'org-install)
            (setq org-modules '(org-habit org-info org-tempo))
            (org-load-modules-maybe t)
            (setq org-habit-graph-column 105)
            
            ;; this doesn't seem to affect anything
            (setq org-archive-subtree-save-file-p t)
            
            (defun org-make-habit()
                (interactive)
                (org-set-property "STYLE" "habit"))
    
    2.  agendas
    
        Everything concerning agendas (thx to mwfogleman).
        
            (use-package org-super-agenda
              :init
              (org-super-agenda-mode)
              (defun my-org-super-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Schedule"
            		    :time-grid t)
            	     (:name "Development"
            		    :tag "dev")
            	     )))
                  (org-agenda nil "a")))
            
              (defun my-org-super-agenda-today ()
                (interactive)
                (progn
                  (my-org-super-agenda)
                  (org-agenda-day-view)))
            
              (defun my-personal-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:discard (:tag ("tv"))))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (defun my-dev-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Development"
            		    :tag "dev")
            	      (:discard (:anything t)))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (defun my-pow-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Meetings"
            		    :tag "pow")
            	      (:discard (:anything t)))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (defun my-edu-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "Education"
            		    :tag "edu")
            	      (:discard (:anything t)))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (defun my-okr-agenda ()
                (interactive)
                (let ((org-super-agenda-groups
            	   '((:name "OKR"
            		    :tag "okr")
            	      (:discard (:anything t)))))
                  (org-agenda nil "a")
                  (org-agenda-day-view)))
            
              (bind-keys ("C-c 0" . my-org-super-agenda-today)
            	     ("C-c 1" . my-dev-agenda)
            	     ("C-c 2" . my-pow-agenda)
            	     ("C-c 3" . my-edu-agenda)
            	     ("C-c 4" . my-okr-agenda)
            	     ("C-c 5" . my-personal-agenda)
            	     ("C-c 6" . my-org-super-agenda)))
            
            ;; you will need to create these (or other) files manually for now
            (setq org-agenda-files (quote ("~/org/tasks.org")))

9.  python mode

    Enable flycheck. This mode is built-in.
    
        (defun my-python-mode-hook ()
          "Hooks for python mode."
          (flycheck-mode 1)
          (drag-stuff-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )
        (add-hook 'python-mode-hook 'my-python-mode-hook)

10. rjsx mode

    Pretty much like js2.
    
        (use-package rjsx-mode
          :init
          (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
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
          (rainbow-mode)
          (rainbow-delimiters-mode)
          (drag-stuff-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil))
        )

11. typescript mode

    Enable lsp, flycheck and sane tabs. And some other stuff.
    
        (use-package typescript-mode
          :config
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
          :hook (typescript-mode . my-typescript-mode-hook))
        (defun my-typescript-mode-hook ()
          "Hooks for typescript mode."
          (enable-tabs)
          (drag-stuff-mode)
          (add-node-modules-path)
          (flycheck-mode 1)
          (lsp)
          (rainbow-delimiters-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

12. web mode

    Web mode uses flycheck with lsp enabled.
    
        (use-package web-mode
          :config
          (setq web-mode-comment-style 2)
          (add-to-list 'web-mode-comment-formats '("vue" . "//"))
          ;; associate files
          (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.component.html" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
          :hook (web . my-web-mode-hook))
        
        (defun my-web-mode-hook ()
          "Hooks for web mode."
          (enable-tabs)
          (web-mode-use-tabs)
          (drag-stuff-mode)
          (add-node-modules-path)
          (lsp)
          (flycheck-mode)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

13. yaml mode

        (use-package yaml-mode)


### functionality

1.  override finding eslint

    ESLint configs can be found using a file, not a directory.
    
        ;; (require 'flycheck)
        (defun flycheck-eslint-config-exists-p ()
          "Whether there is a valid eslint config for the current buffer."
          (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
        	 (exitcode (and executable (call-process executable nil nil nil
        						 "--print-config" ".eslintrc"))))
            (eq exitcode 0)))

2.  function to switch between tslint and lsp

    This is not good code, but lsp doesn't play nice with tslint.
    
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

3.  load eslint/tslint from local node modules

    Use the locally installed eslint/tslint binaries.
    
        (defun my/use-eslint-from-node-modules ()
          (let* ((root (locate-dominating-file
        		(or (buffer-file-name) default-directory)
        		"node_modules"))
        	 (eslint
        	  (and root
        	       (expand-file-name "node_modules/.bin/eslint"
        			       root))))
            (when (and eslint (file-executable-p eslint))
              (setq-local flycheck-javascript-eslint-executable eslint))))
        
        (defun my/use-tslint-from-node-modules ()
          (let* ((root (locate-dominating-file
        		(or (buffer-file-name) default-directory)
        		"node_modules"))
        	 (tslint
        	  (and root
        	       (expand-file-name "node_modules/.bin/tslint"
        				 root))))
            (when (and tslint (file-executable-p tslint))
              (setq-local flycheck-typescript-tslint-executable tslint))))
        
        ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
        ;; (add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> What you're reading is likely a markdown version exported from it.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> Repositories (incomplete):

-   [ag](https://github.com/Wilfred/ag.el)
-   [all the icons](https://github.com/domtronn/all-the-icons.el)
-   [avy](https://github.com/abo-abo/avy)
-   [dap](https://github.com/emacs-lsp/dap-mode)
