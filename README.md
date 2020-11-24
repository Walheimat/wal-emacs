

# Emacs Org Config

This project is my personal Emacs (27.1)<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> config.

Its base is an org file so it doubles as a readme<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>.


# Table of Contents

1.  [Emacs Org Config](#org9be5975)
    1.  [Setup](#org3cd206e)
        1.  [Heads-up](#org8e98442)
        2.  [Try-out](#org513d627)
    2.  [Config](#orga80542f)
        1.  [Personal](#org440552b)
        2.  [Initialization](#org6742fe8)
        3.  [Built-in](#org5bec9c8)
        4.  [Packages](#org79ffd6d)
        5.  [Mode Configs](#org2c1fd97)
        6.  [Tweaks](#orgf028dca)


<a id="org3cd206e"></a>

## Setup

Everything you need to know to use this config,
including the information that you maybe shouldn't.


<a id="org8e98442"></a>

### Heads-up

If you're a complete beginner,
you will find [more user-friendly and less tailor-made configs](https://github.com/emacs-tw/awesome-emacs#starter-kit) out there.

The idea of this config is to provide a jumping-off-point for your own custom config.
Nothing in this config should be considered <span class="underline">good practice</span>,
it's mostly just how I (think I) like things to be.


<a id="org513d627"></a>

### Try-out

If you're interested in trying out Emacs using my config anyway,
here are the necessary steps:

-   Install Emacs if you haven't<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>
-   Clone this repository into your `user-emacs-directory`<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup>
-   Copy the config file included in this repo by running `cp ~/.emacs.d/emacs-config/.emacs.example ~/.emacs`
-   Close and re-run Emacs which should download and install (almost<sup><a id="fnr.5" class="footref" href="#fn.5">5</a></sup>) all packages

If you did not init this repo in your `user-emacs-directory` using the default name,
you will need to adapt the variable `walheimat-emacs-config-default-path` in the example config you just copied.


<a id="orga80542f"></a>

## Config

The init script will evaluate <span class="underline">everything</span><sup><a id="fnr.6" class="footref" href="#fn.6">6</a></sup> that follows.


<a id="org440552b"></a>

### Personal

Set some personal info.<sup><a id="fnr.7" class="footref" href="#fn.7">7</a></sup>

    (setq user-full-name    "Krister Schuchardt"
          user-mail-address "krister.schuchardt@theventury.com")
    
    ;; warn Mac/Windows users
    (unless (eq system-type 'gnu/linux)
      (warn "\
        Warning: Config only tested on linux.
        While I did get in running on Windows 10,
        it was quite tricky and involved setting unsafe options."))


<a id="org6742fe8"></a>

### Initialization

Set up Emacs, package manager and packages.

1.  Folders

    Make sure the cache folder exists.
    We store bookmarks and perspectives here.
    
        (unless (file-directory-p (expand-file-name ".cache" user-emacs-directory))
          (make-directory (expand-file-name ".cache" user-emacs-directory)))

2.  Start-Up

    Customize start-up.
    
        ;; if you're using a tiling wm, comment the following two lines
        (add-to-list 'default-frame-alist '(fullscreen . maximized))
        ;; (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
        
        ;; no splash
        (setq inhibit-startup-message t)
        
        ;; use a distinct file for customization
        (setq custom-file (expand-file-name "custom.el" walheimat-emacs-config-default-path))
        
        ;; we'll create that file if it doesn't yet exist
        (unless (file-exists-p custom-file)
          (write-region "" nil custom-file))
        
        (load custom-file)
    
    1.  Persistent **scratch**
    
        Let's keep the scratch contents.
        
        This (and some other things) was cribbed from [john2x's config](https://www.john2x.com/emacs.html) and only slightly adapted.
        
            ;; empty scratch message
            (setq initial-scratch-message ";; Howdy, stranger ...")
            
            (setq scratch-persist-file (expand-file-name ".cache/scratch-persist" user-emacs-directory))
            
            (defun persist-scratch ()
              (interactive)
              "Persist contents of *scratch* buffer"
              (with-current-buffer (get-buffer-create "*scratch*")
                (write-region (point-min) (point-max) scratch-persist-file)))
            
            (defun rehydrate-scratch ()
              "Re-hydrate scratch buffer (if persisted)"
              (if (file-exists-p scratch-persist-file)
                  (with-current-buffer (get-buffer "*scratch*")
            	(delete-region (point-min) (point-max))
            	(insert-file-contents scratch-persist-file))))
            
            (add-hook 'after-init-hook #'rehydrate-scratch)
            (add-hook 'kill-emacs-hook #'persist-scratch)

3.  Package Archives

    Add MELPA and org-mode to our package archives.
    We'll be getting most (if not all) packages from the prior.
    
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
          (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
          ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
          ;; and `package-pinned-packages`. Most users will not need or want to do this.
          ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
        )
        (setq package-pinned-packages '())
        (package-initialize)

4.  Lisp extensions

    We need dash.
    
        ;; we need dash for the upcoming loop
        (unless (package-installed-p 'dash)
          (condition-case nil
            (package-install 'dash)
            (error (package-refresh-contents)
        	   (package-install 'dash))))
        (require 'dash)

5.  Dependencies

    We manage our packages with `use-package`, before we can use it,
    we have to install it and a few other packages the hard way.
    
    1.  Other packages
    
        Now we can install the other dependencies.
        
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

6.  Site-Lisp

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

7.  Package Management

    We also always want to ensure the package, i.e. if it's not there, get it.
    
    I used key-chords for a few, common actions but prefer using hyper key now.
    
        (require 'use-package-ensure)
        (setq use-package-always-ensure t)
        
        (require 'diminish)
        (require 'delight)
        (require 'bind-key)
        
        ;; has to come here to be useable
        (use-package use-package-chords
          :disabled
          :config
          (key-chord-mode 1))


<a id="org5bec9c8"></a>

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
        ;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
        (global-prettify-symbols-mode +1)
        ;; (global-whitespace-mode)
        (save-place-mode 1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        (global-font-lock-mode 1)
        (delete-selection-mode 1)
        
        ;; trying to not touch the mouse as much
        ;; (mouse-avoidance-mode 'banish)
        
        ;; simple y/n is enough
        (defalias 'yes-or-no-p 'y-or-n-p)
        
        ;; I want my comments slanted and my keywords bold
        ;; the FiraCode font does not support this
        (defun my-font-lock-hook ()
          "Slanted and enchanted."
          (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
          (set-face-attribute 'font-lock-keyword-face nil :weight 'bold))
        
        (add-hook 'font-lock-mode-hook 'my-font-lock-hook)
        
        ;; huge cursor
        ;; (setq x-stretch-cursor t)

3.  Reasonable Settings

    Make things snappier.
    
        (setq mouse-yank-at-point       t
              show-paren-delay          0.0
              read-process-output-max   (* 1024 1024) ;; 1mb
              sentence-end-double-space nil
              echo-keystrokes           0.1)

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
        
        (setq-default python-indent-offset    custom-tab-width ;; Python
        	      js-indent-level         custom-tab-width ;; Javascript
        	      electric-indent-inhibit t)
        
        (setq backward-delete-char-untabify-method 'hungry)

5.  Key Bindings

    Change up the key bindings a bit.
    
    1.  Personal
    
        I try to have most actions use user-reserved `C-c <key>` combinations,
        but some `C-x <key>` mappings snuck in.
        
        If you want to see all personal keybindings, execute `describe-personal-keybindings`.
        
        -   `C-c a <key>` opens today's (`t`), this week's (`w`) or my personal (`p`) agenda.
        -   `C-c c <key>` to duplicate (`d`) the current line, kill (`k`) other buffers and (`o`) open with outside program.
        -   `C-c c #` (un-)comments lines.
        -   `C-c f <key>` runs ag (`a` for generic, `p` for in-project search). <span class="underline">Requires ag</span>!
        -   `C-c g` opens magit status.
        -   `C-c i <key>` interacts with perspectives.
        -   `C-c j` dumb-jumps.
        -   `C-c k` for docker actions.
        -   `C-c m <key>` for multiple cursors.
        -   `C-c o <key>` toggle (`o`), go to next (`n`) or show (`s`) bookmarks.
        -   `C-c p <key>` interacts with projects.
        -   `C-c q <key>` interacts with fly-checking.
        -   `C-c s` uses swiper to search.
        -   `C-c t <key>` opens eshell (`e`), ansi-term (`a`) or vterm (`v`).
        -   `C-c u <key>` to debug (`u`) or edit template (`t`).
        -   `C-c v <key>` jumps to char (`c`) or line (`v`) with avy.
        -   `C-c w <key>` swaps (`s`) or deletes other windows (`d`).
        -   `C-+` expands region.
        -   `C-x C-c` opens this config org file.
        -   `C-x r q` (really) quits.
        -   `C-x r s` restarts.
        -   `C-z=/=C-S-z` undos/redos.
        -   `<f5>` to enter writeroom mode.
        -   `<f6>` to google this.
        -   `<f7>` to present org file.
        -   `<f8>` to spell-check.
        -   `M-o` goes to the "other" window or the last buffer.
        -   `M-x` opens smex.
        -   `s-k` kills the whole line.
        -   `s-(S)-RET` will open an indented line above (below).
    
    2.  Hyper
    
        The following bindings are either quick-access of already bound
        actions or those that aren't essential (since having a hyper key is not
        guaranteed).
        
        -   `H-b` switches buffer.
        -   `H-d` opens dired relative to open file.
        -   `H-f` to find with ag.
        -   `H-i` switches prespective.
        -   `H-k` to interact with docker.
        -   `H-m` mark all like this (multiple cursors).
        -   `H-<number>`
            -   closes (`0`),
            -   closes others (`1`),
            -   splits horizontal (`2`),
            -   splits vertical (`3`),
            -   kills (`4`).
        -   `H-o` to toggle bookmarks.
        -   `H-p` switches project.
        -   `H-s` searches with swiper.
        -   `H-<TAB>` expands snippets (in `yas-minor-mode`).
        -   `H-#` (un-)comments lines.
        -   `H-u` to debug.
        -   `H-v` jumps to line with avy.
        -   `H-w` swaps windows.
        
        1.  Caps to Hyper
        
            I re-bound my `<CAPS>` (caps-lock) key to `Hyper_L` to use these
            hyper bindings.
            
            If you use Xorg Display Server,
            you can do this by editing your `/usr/share/X11/xkb/symbols/pc` file like so:
            
                ...
                // key <CAPS> {    [ Caps_Lock     ]   };
                key <CAPS> {    [ Hyper_L       ]   };
                ...
                // modifier_map Lock   { Caps_Lock };
                modifier_map Mod3   { Hyper_L, Hyper_R };
                ...
                // modifier_map Mod4   { <HYPR> };   
                modifier_map Mod3   { <HYPR> };   
    
    3.  Non-Use-Package Bindings
    
        Most bindings are declared in the [packages](#org79ffd6d) section.
        
            ;; windows
            (global-set-key (kbd "H-0")     'delete-window)
            (global-set-key (kbd "H-1")     'delete-other-windows)
            (global-set-key (kbd "H-2")     'split-window-below)
            (global-set-key (kbd "H-3")     'split-window-right)
            (global-set-key (kbd "H-4")     'kill-buffer-and-window)
            
            ;; emacs
            (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
            (global-set-key
              (kbd "C-x C-c")
              (lambda () (interactive)(switch-to-buffer (find-file-noselect (expand-file-name "configuration.org" walheimat-emacs-config-default-path)))))

6.  Theme

    Be sure to check out [Peach Melpa](https://peach-melpa.org/) to find a theme you like.
    
    If you're using the `doom-modeline`, go for a `doom-*` theme.
    Otherwise the colors might clash.
    
        ;; two themes and a switch
        (defcustom my-primary-emacs-theme 'doom-opera-light
          "The quote-unquote default emacs theme.")
        
        (defcustom my-secondary-emacs-theme 'paper
          "The non-default emacs theme.")
        
        (setq my-active-theme my-primary-emacs-theme)
        
        (defun theme-light-switch (&optional selection)
          "Switch from light to dark theme and vice-versa."
          (interactive)
          (disable-theme my-active-theme)
          (cond ((or (equal my-active-theme my-primary-emacs-theme) (equal selection 'secondary))  
        	   (load-theme my-secondary-emacs-theme)
        	   (setq my-active-theme my-secondary-emacs-theme))
        	((or (equal my-active-theme my-secondary-emacs-theme) (equal selection 'secondary))
        	   (load-theme my-primary-emacs-theme)
        	   (setq my-active-theme my-primary-emacs-theme))))
        
        ;; some themes require configuration, so we only load after intialization
        (add-hook 'after-init-hook '(lambda()
          (load-theme my-primary-emacs-theme)
          ;; if you don't mind some transparency
          (transparency 95)))

7.  Font

    Prefer FiraCode (-> mononoki -> Liberation -> DejaVu).
    
    If Emacs runs with the custom argument `--bigger`, the default font size is 14 (instead of 10).
    
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
        
        (if (found-custom-arg "--bigger")
          (set-default-font (font-candidate '"Fira Code 14" "mononoki 14" "Liberation Mono 14" "DejaVu Sans Mono 14")))

8.  Zoning

    Zone out after a couple of minutes.
    
        (require 'zone)
        (zone-when-idle 180)

9.  Time

    I want to see the time sometimes (fullscreen).
    I don't want to see the CPU load though.
    
        (setq display-time-default-load-average nil
              display-time-format               "%k:%M ")
        
        ;; note that turning this on will persist the mode in your custom.el,
        ;; so delete it from there if you want it gone again
        (display-time-mode -1)

10. Additional Functions

    Better (?) garbage collection, transparency,
    and `treemacs` buffer check.
    
        ;; check if buffer is treemacs buffer
        ;; similar to minibufferp
        (defun treemacsbufferp ()
          "Check if this is the treemacs buffer."
          (eq (current-buffer) (treemacs-get-local-buffer)))
        
        ;; trick garbage collection
        (defvar hundred-mb (* 1024 1024 100))
        (defun my-minibuffer-setup-hook ()
          (setq gc-cons-threshold most-positive-fixnum))
        (defun my-minibuffer-exit-hook ()
          (setq gc-cons-threshold hundred-mb))
        
        (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
        (add-hook 'minibuffer-exit-hook  #'my-minibuffer-exit-hook)
        
        ;; transparency
        (defun transparency (value)
          "Sets the transparency of the frame window. 0=transparent/100=opaque"
          (interactive "nTransparency Value 0 - 100 opaque:")
          (set-frame-parameter (selected-frame) 'alpha value))
        
        ;; creating parent dirs
        (defun my-create-non-existent-directory ()
          (let ((parent-directory (file-name-directory buffer-file-name)))
            (when (and (not (file-exists-p parent-directory))
              (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        	(make-directory parent-directory t))))
        (add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)
        
        ;; finding dired buffers
        (defun is-dired-buffer (buffer-or-string)
          "Check if provided buffer is dired-buffer."
          (eq (with-current-buffer (get-buffer-create buffer-or-string) major-mode) 'dired-mode))
        
        ;; finding docker buffers
        (defun is-docker-buffer (buffer-or-string)
          "Check if provided buffer is docker-buffer."
          (string-match "* docker " buffer-or-string))
        
        ;; finding some default emacs buffers I don't need to see
        (defun is-default-emacs-buffer (buffer-or-string)
          "Check if provided buffer is a default emacs buffer."
          (or (string-match "*Messages*" buffer-or-string)
              (string-match "*scratch*" buffer-or-string)
              (eq (with-current-buffer (get-buffer-create buffer-or-string) major-mode) 'help-mode)))
        
        ;; finding ag buffers
        (defun is-ag-buffer (buffer-or-string)
          "Check if provided buffer is an ag buffer."
          (string-match "*ag search " buffer-or-string))


<a id="org79ffd6d"></a>

### Packages

What follows is a list of MELPA packages that make Emacs even more awesome.

If you wish to know more about any of them, check out the list<sup><a id="fnr.8" class="footref" href="#fn.8">8</a></sup> of repositories
at the end of this readme/configuration or the [awesome-emacs](https://github.com/emacs-tw/awesome-emacs) project.

Many packages bind keys.
Check the [key bindings section](#orgcbeb3e6) if you need a list of all of them.

1.  ace

    `ace` allows for some nifty window swapping.
    
        (use-package ace-window
          :bind (("H-w" . ace-swap-window)
        	 ("C-c w s" . ace-swap-window)
        	 ("C-c w d" . ace-delete-other-windows)))

2.  add-node-modules-path

    Allows accessing a project's `node_modules`.
    
        (use-package add-node-modules-path)

3.  ag

    Highlight search results using the **Silver Searcher**.
    
    This <span class="underline">requires</span> the `ag` binary which you can get from [here](https://github.com/ggreer/the_silver_searcher#installation) (we will try
    to download it automatically, but might fail).
    
        (use-package ag
          :ensure-system-package ag
          :init
          (setq ag-highlight-search t)
          :bind (("C-c f a" . ag)
        	 ("C-c f p" . ag-project)
        	 ("H-f"     . ag)))

4.  all-the-icons

    You need to install the icons yourself<sup><a id="fnr.5.100" class="footref" href="#fn.5">5</a></sup>.
    
        (use-package all-the-icons)
        
        ;; use it for dired
        (use-package all-the-icons-dired
          :after all-the-icons
          :diminish
          :hook (dired-mode . all-the-icons-dired-mode))

5.  ansi-term

    Sometimes you need a terminal.
    
        (use-package term
          :bind ("C-c t a" . ansi-term))

6.  auto-package-update

    Keep packages updated (disabled for now).
    
        (use-package auto-package-update
          :disabled
          :init
          (setq auto-package-update-delete-old-versions t)
          (setq auto-package-update-hide-results        t)
          :config
          (auto-package-update-maybe))

7.  avy

    Jumping to (visible) lines and chars is fun if you are too lazy to use your mouse.
    
        (use-package avy
          :bind (("C-c v v" . avy-goto-line)
        	 ("C-c v c" . avy-goto-char)
        	 ("H-v"     . avy-goto-line)))

8.  beacon

    Help me find my cursor!
    
        (use-package beacon
          :config
          (beacon-mode 1)
          (setq beacon-color                             0.4
        	beacon-blink-duration                    0.4
        	beacon-size                              60
        	beacon-blink-when-point-moves-vertically 2))

9.  bm

    Bookmarks are useful. I don't remember where I was. <span class="underline">Who are you?!</span>
    
        (use-package bm
          :init
          (setq-default bm-buffer-persistence t)
          (setq bm-restore-repository-on-load t
        	bm-repository-file            (expand-file-name ".cache/bm-persist" user-emacs-directory)
        	bm-annotate-on-create         t
        	bm-highlight-style            'bm-highlight-only-fringe
        	bm-cycle-all-buffers          t)
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
           (("C-c o s" . bm-show)
            ("C-c o n" . bm-next)
            ("C-c o b" . bm-toggle)
            ("H-o"     . bm-toggle))) ;; ho-ho-ho!

10. company

    Code-completion. In a box.
    
        (use-package company-box
          :diminish
          :hook (company-mode . company-box-mode))
        
        (use-package company
          :delight " cmp"
          :init
          (setq company-prefer-capf           t
        	company-minimum-prefix-length 3
        	company-idle-delay            0.5)
          :hook (prog-mode . company-mode))
        
        (use-package company-restclient
          :after company)
        
        (use-package company-web
          :after company)

11. crux

    Let's use `crux` for some editing magic.
    Check the [key bindings section](#orgcbeb3e6) for descriptions.
    
        (use-package crux
          :bind (("M-o"          . crux-other-window-or-switch-buffer)
        	 ("C-c c k"      . crux-kill-other-buffers)
        	 ;; need to find solution with treemacs open
        	 ("C-c c t"      . crux-transpose-windows)
        	 ("C-c c o"      . crux-open-with)
        	 ("S-s-<return>" . crux-smart-open-line-above)
        	 ("s-<return>"   . crux-smart-open-line)
        	 ("s-k"          . crux-kill-whole-line)
        	 ("C-c c d"      . crux-duplicate-current-line-or-region)))

12. dap

    Debugging using VSCode's DAP.
    
        (use-package dap-mode
          :delight " dap"
          :custom
          (lsp-enable-dap-auto-configure nil)
          :init
          (setq dap-python-executable "python3")
          ;; (setq dap-auto-configure-features '(sessions locals breakpoints))
          :config
          (dap-ui-mode 1)
          (require 'dap-node)
          (require 'dap-python)
          :bind (("C-c u" . dap-mode)
        	 (:map dap-mode-map
        	       ("H-u" . dap-hydra))))
    
    1.  Debug Templates
    
        Here are some examples for Node.js projects using `nodemon`.
        
        Put them in a file in your project root,
        and evaluate them there using `C-x C-e`.
        Adapt paths if necessary.
        
        1.  Node.js
        
            This one is for attaching to a containerized node app.
            
                (when (require 'dap-mode nil 'noerror)
                  (progn
                    (let* ((remote-root "/usr/src/app")
                	   (local-root (file-name-directory buffer-file-name)))
                      (dap-register-debug-template
                       "attach::node"
                       (list :type "node"
                	     :request "attach"
                	     :sourceMaps t
                	     :remoteRoot remote-root
                	     :localRoot local-root
                	     :port 9229)))))
        
        2.  Transpiled Node.js
        
            Still some `babel` projects left.
            
                (when (require 'dap-mode nil 'noerror)
                  (progn
                    (let* ((build-directory "build")
                	   (remote-root (concat "/usr/src/app/" build-directory))
                	   (local-root (concat (file-name-directory buffer-file-name) build-directory)))
                      (dap-register-debug-template
                       "attach::babel"
                       (list :type "node"
                	     :request "attach"
                	     :sourceMaps t
                	     :remoteRoot remote-root
                	     :localRoot local-root
                	     :port 9229)))))
        
        3.  TypeScript
        
            Compile your `src` with `--sourceMap` or set `sourceMap` to `true` in
            your `tsconfig.json`.
            
                (when (require 'dap-mode nil 'noerror)
                  (progn
                    (let* ((build-directory "build")
                	   (remote-root (concat "/usr/src/app/" build-directory))
                	   (local-root (concat (file-name-directory buffer-file-name) build-directory)))
                      (dap-register-debug-template
                       "attach::typescript"
                       (list :type "node"
                	     :request "attach"
                	     :sourceMaps t
                	     :remoteRoot remote-root
                	     :localRoot local-root
                	     :port 9229)))))

13. delight

    Refine a couple of major-mode names.
    
        (use-package delight
          :config
          (delight 'dired-mode "Dired" :major)
          (delight 'js2-mode "JavaScript" :major)
          (delight 'emacs-lisp-mode "Elisp" :major))

14. diff-hl

    Show diffs in the fringe.
    Show diffs in `dired` buffers as well.
    Refresh after `magit` is done.
    
        (use-package diff-hl
          :init
          (global-diff-hl-mode)
          :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
        	 (dired-mode         . diff-hl-dired-mode)))

15. diminish

    See individual `use-package` declarations as well,
    since we `delight` in/diminish them there.
    
        (use-package diminish
          :config
          (diminish 'eldoc-mode))

16. dimmer

    Dim inactive frames.
    Make dimmed frames a bit dimmer.
    
        (use-package dimmer
          :diminish
          :init
          (setq dimmer-fraction       0.3
        	dimmer-adjustmentmode :both)  
          :config
          (dimmer-configure-org)
          (dimmer-configure-magit)
          (dimmer-configure-hydra)
          (dimmer-mode t))

17. dired

    Group directories first in `dired`,
    override some keybindings.
    
        (use-package dired
          :ensure nil
          :init
          (put 'dired-find-alternate-file 'disabled nil)
          :config
          (setq delete-by-moving-to-trash t)
          :commands (dired dired-jump delete-file)
          :custom ((dired-listing-switches "-lah --group-directories-first"))
          :bind (("H-d" . dired-jump)
        	 (:map dired-mode-map
        	 ("V" . dired-display-file)   ;; overrides dired-do-run-mail
        	 ("-" . dired-up-directory)))) ;; overrides negative-argument

18. dired-filter

    This package is awesome.
    Hit `/` to filter in `dired` buffers.
    
        (use-package dired-filter
          :diminish "def")

19. docker

    I use Docker a lot, don't always have to use the command line.
    
        (use-package docker
          :init
          (setq docker-container-default-sort-key '("Names"))
          :bind (("C-c k" . docker)
        	 ("H-k"   . docker)))

20. doom-modeline

    Busier and prettier modeline.
    Note that this packag requires you to install `all-the-icons` fonts<sup><a id="fnr.5.100" class="footref" href="#fn.5">5</a></sup>.
    
        (use-package doom-modeline
          :init
          (setq doom-modeline-project-detection 'projectile
        	doom-modeline-minor-modes       t
        	doom-modeline-buffer-encoding   nil)
          :config
          (doom-modeline-mode 1))

21. drag stuff

    Use the default key bindings.
    
        (use-package drag-stuff
          :delight " drg"
          :hook (prog-mode . drag-stuff-mode)
          :config
          (drag-stuff-define-keys))

22. dumb-jump

    Jump to definitions (in other files).
    Configure it for `ivy`.
    
        (use-package dumb-jump
          :init
          (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
          (setq dumb-jump-selector       'ivy
        	dumb-jump-force-searcher 'ag)
          :config
          (dumb-jump-mode)
          :bind (("C-c j" . xref-find-definitions)
        	 ("H-j"   . xref-find-definitions)))

23. eshell

    Set up `eshell`.
    
        (use-package eshell
          :ensure nil
          :hook (eshell-mode . my-eshell-mode-hook)
          :bind ("C-c t e" . eshell))
        
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
        
        (with-eval-after-load "esh-opt"
          (autoload 'epe-theme-lambda "eshell-prompt-extras")
          (setq eshell-highlight-prompt nil
        	eshell-prompt-function 'epe-theme-lambda))

24. evilnc

    Comment code like in `vim`, evil, evil `vim`.
    
        (use-package evil-nerd-commenter
          :bind (("H-#" . evilnc-comment-or-uncomment-lines)
        	 ("C-c c #"  . evilnc-comment-or-uncomment-lines)))

25. expand-region

    One thing that can be a bit tricky is selecting regions, not anymore.
    
        (use-package expand-region
          :bind ("C-+" . er/expand-region))

26. find-file-in-project

    Finding files by name should be easy.
    
        (use-package find-file-in-project)

27. fira-code

    I use FiraCode, this mode allows us to use ligatures.
    
        (use-package fira-code-mode
          :diminish
          ;; use fira mode if it's the default font and the symbol font is installed
          :if (and (x-list-fonts "Fira Code Symbol") (string= "Fira Code" (face-attribute 'default :family)))
          :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
          :hook prog-mode)                                         ; mode to enable fira-code-mode in

28. flycheck

    `flycheck` is for all of our linting/code quality needs.
    
        (use-package flycheck
          :delight " fly"
          :init
          (setq flycheck-keymap-prefix (kbd "C-c q"))
          :hook ((flycheck-mode . my-use-eslint-from-node-modules)
        	 (flycheck-mode . my-use-tslint-from-node-modules)))

29. flyspell

    My spelling is bad.
    Use American English for `flyspell`.
    
    You can bring up actions (skip, save) with `M-o`.
    
        (use-package flyspell
          :delight " fsp"
          :init
          (setq ispell-dictionary "american")
          (setq flyspell-issue-message-flag nil)
          :bind (("<f9>" . flyspell-mode)
        	 (:map flyspell-mode-map
        	     ("n" . flyspell-correct-next)
        	     ("p" . flyspell-correct-previous))))
        
        (use-package flyspell-correct
          :after flyspell)
        
        (use-package flyspell-correct-ivy
          :after flyspell-correct)

30. gitignore-mode

    Syntax highlighting.
    
    Necessary even for `.gitignore` files.
    
        (use-package gitignore-mode
          :mode "^.gitignore")

31. git-timemachine

    If you want to go back in time and point fingers at the progenitors of doom.
    
        (use-package git-timemachine)

32. golden-ratio

    Use the golden ratio.
    
        (use-package golden-ratio
          :diminish
          :config
          (golden-ratio-mode 1))

33. google-this

    If you're too lazy to copy and paste.
    
        (use-package google-this
          :diminish
          :bind ("<f6>" . 'google-this-mode-submap)
          :config
          (google-this-mode 1))

34. highlight-indent-guides

    Show indentation.
    
        (use-package highlight-indent-guides
          ;; don't need to see this
          :diminish highlight-indent-guides-mode
          :init
          (setq highlight-indent-guides-method 'character)
          :hook (prog-mode . highlight-indent-guides-mode))

35. highlight numbers

    Make numbers stand out.
    
        (use-package highlight-numbers
          :hook (prog-mode . highlight-numbers-mode))

36. hl-todo

    Highlight `TODO`, `FIXME` etc. in `prog` modes.
    
        (use-package hl-todo
          :hook (prog-mode . hl-todo-mode))

37. hydra

    We use `hydra` to trigger grouped actions.
    
        (use-package hydra)

38. ivy

    We use `ivy` for narrowing our options.
    
        ;; change to ivy-switch-buffer if you don't use perspective
        (defalias 'my-switch-buffer 'persp-ivy-switch-buffer)
        
        ;; hide dired, docker, ag and default emacs buffers when switching
        (setq my-ivy-ignore-buffers '(
          is-dired-buffer
          is-docker-buffer
          is-ag-buffer
          is-default-emacs-buffer
          "\\` "
          "\\`\\*tramp/"))
        
        ;; toggle custom ignore on or off
        (defun my-toggle-ivy-ignore ()
          (interactive)
          (if (y-or-n-p "Use custom ivy buffer ignore?")
            (setq ivy-ignore-buffers my-ivy-ignore-buffers)
            (setq ivy-ignore-buffers '("\\` " "\\`\\*tramp/"))))
        
        (use-package ivy
          :init
          (setq ivy-use-virtual-buffers      t
        	enable-recursive-minibuffers t
        	ivy-ignore-buffers           my-ivy-ignore-buffers)
          :bind (("C-x b" . my-switch-buffer)
        	 ("H-b"   . my-switch-buffer))
          :config
          (ivy-mode 1))

39. ivy-rich

    Some nicer candidate view when switching buffers.
    
        (defun ivy-rich-switch-buffer-icon (candidate)
          (with-current-buffer (get-buffer candidate)
            (let ((icon (all-the-icons-icon-for-mode major-mode)))
              (if (symbolp icon)
        	  (all-the-icons-icon-for-mode 'fundamental-mode)
        	   icon))))
        
        (use-package ivy-rich
          :after ivy
          :init
          (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
          (setq ivy-rich-display-transformers-list
              '(my-switch-buffer
        	(:columns
        	 (
        	  (ivy-rich-candidate (:width 30))
        	  (ivy-rich-switch-buffer-size (:width 7))
        	  (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
        	  (ivy-rich-switch-buffer-project (:width 30 :face success))
        	  ;; (ivy-rich-switch-buffer-major-mode (:width 8 :face warning))
        	  (ivy-rich-switch-buffer-icon (:width 2))
        	  (ivy-rich-switch-buffer-path (:width (lambda (x)
        	    (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
        	 )
        	 :predicate
        	 (lambda (cand) (get-buffer cand)))))
          :config
          (ivy-rich-mode 1))

40. kaolin

    This is a themes collection I sometimes pick from.
    
        (use-package kaolin-themes
          :init
          (setq kaolin-ocean-alt-bg                      t
        	kaolin-themes-italic-comments            t
        	kaolin-themes-git-gutter-solid           t
        	;; modeline border
        	kaolin-themes-modeline-border            nil
        	;; distinct background for fringe and line numbers
        	kaolin-themes-distinct-fringe            t
        	;; distinct colors for company popup scrollbar
        	kaolin-themes-distinct-company-scrollbar t)
          :config
          ;; treemacs
          (kaolin-treemacs-theme))

41. kubernetes

    Who doesn't like pods and stuff?
    
        (use-package kubernetes
          :commands (kubernetes-overview))

42. lsp

    Prefer `capf`, bigger delay.
    
        (use-package lsp-mode
          :init
          (setq lsp-completion-provider :capf
        	lsp-prefer-capf         t
        	lsp-idle-delay          1.5)
          ;; (setq lsp-semantic-highlighting t)
          :config
          ;; ignore elixir build and dependency folders
          (add-to-list 'lsp-file-watch-ignored "[/\\\\]_build$")
          (add-to-list 'lsp-file-watch-ignored "[/\\\\]deps$"))
        
        (use-package lsp-ui)
    
    1.  Language Servers
    
        Configure or register language servers.
        
        You will have to install them yourself.<sup><a id="fnr.9" class="footref" href="#fn.9">9</a></sup>
        
            ;;; elixir
            (setq elixir-ls-release-location (expand-file-name "ls/elixir" user-emacs-directory))
            (if (file-exists-p (expand-file-name "language_server.sh" elixir-ls-release-location))
              (add-to-list 'exec-path elixir-ls-release-location)
              (add-hook 'elixir-mode-hook 'lsp))
            
            ;;; prolog
            (lsp-register-client
              (make-lsp-client
               :new-connection
               (lsp-stdio-connection (list "swipl"
            			       "-g" "use_module(library(lsp_server))."
            			       "-g" "lsp_server:main"
            			       "-t" "halt"
            			       "--" "stdio"))
               :major-modes '(prolog-mode)
               :priority 1
               :multi-root t
               :server-id 'prolog-ls))

43. magit

    Version control has never been this easy before.
    
        (use-package magit
          :bind (("C-c g" . magit-status)
        	 ("H-g"   . magit-status)))

44. mode-line-bell

    Make the bell visual.
    
        (use-package mode-line-bell
          :config
          (mode-line-bell-mode))

45. multiple-cursors

    Sometimes a lot of things are similarly wrong.
    It's nice to change everything at once.
    
        (use-package multiple-cursors
          :bind
          (("C-c m n" . mc/mark-next-like-this)
           ("C-c m p" . mc/mark-previous-like-this)
           ("C-c m a" . mc/mark-all-like-this)
           ("H-m"     . mc/mark-all-like-this)))

46. mwim

    Move where I want.
    Useful for comments.
    
        (use-package mwim
          :bind (("C-a" . mwim-beginning)
        	 ("C-e" . mwim-end)))

47. origami

    Code folding.
    Unfortunately has some performance issues.
    This package also uses the deprecated `cl` package,
    leading to warning from emacs version 27 onwards.
    
    Disabled for now.
    
        (use-package origami
          :disabled
          :init
          (setq origami-fold-replacement "⋯")
          :hook (prog-mode . origami-mode)
          :bind (("C-c o" . origami-toggle-node)))

48. perspective

    Have some perspective, man.
    
        ;; default is "main"
        (setq my-default-perspective "walheimat")
        
        (use-package perspective
          :custom-face
          (persp-selected-face ((t (:weight bold :foreground "burlywood"))))
          :bind ("H-i" . persp-switch)
          :init
          (setq persp-modestring-dividers '("(" ")" "/")
        	persp-initial-frame-name  my-default-perspective
        	persp-state-default-file  (expand-file-name ".cache/persp-persist" user-emacs-directory)
        	persp-mode-prefix-key     (kbd "C-c i"))
          :config
          (persp-mode))
        
        ;; not sure there's much benefit to this
        (use-package persp-projectile
          :disabled
          :after perspective
          :bind ("C-c ö" . projectile-persp-switch-project))
        
        ;; no idea why putting this in :hook kills the package
        (add-hook 'kill-emacs-hook #'persp-state-save)

49. prettier-js

    Format code quickly.
    
        (use-package prettier-js
          :init
          ;; you might want to remove/edit this
          (setq prettier-js-args '("--print-width" "91")))

50. projectile

    Projects in Emacs.
    You don't really <span class="underline">need</span> `treemacs`.
    
        (use-package projectile
          :diminish " pjt"
          :bind ("H-p" . projectile-switch-project)
          :init
          (setq projectile-completion-system     'ivy
        	projectile-mode-line-function    '(lambda() (format " {%s}" (projectile-project-name)))
        	projectile-switch-project-action #'projectile-dired
        	projectile-sort-order            'recentf)
          :config
          ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
          ;; (add-to-list 'projectile-globally-ignored-directories "build")
          ;; (add-to-list 'projectile-other-file-alist '("org" "org_archive"))
          ;; (add-to-list 'projectile-other-file-alist '("org_archive" "org"))
          (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
          (projectile-mode +1))

51. rainbow

    Show colors in source code and make delimiters stand out.
    
        (use-package rainbow-delimiters
          :hook (prog-mode . rainbow-delimiters-mode))
        
        (use-package rainbow-mode
          :diminish
          :hook (prog-mode . rainbow-mode))

52. restart-emacs

    Sometimes I restart for fun.
    
        (use-package restart-emacs
          :init
          (setq restart-emacs-restore-frames t)
          :bind ("C-x r s" . restart-emacs))

53. restclient

    Postman is passé.
    I use a `.http` file extension for my request examples.
    
        (use-package restclient
          :mode ("\\.http\\'" . restclient-mode))

54. request

    Not used yet, but will in the future.
    
        (use-package request)

55. s

    String manipulation utility.
    
        (use-package s)

56. smartparens

    Create a pairs automatically.
    
        (use-package smartparens
          :diminish smartparens-mode
          :init
          (require 'smartparens-config)
          :hook (prog-mode . smartparens-mode))

57. smeargle

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

58. smex

    Show completions for `M-x` in a buffer.
    
        (use-package smex
          :bind ("M-x" . smex))

59. smooth-scrolling

    Smooth scrolling at the margins using `C-n` and `C-p`.
    
        (use-package smooth-scrolling
          :init
          (setq smooth-scroll-margin 4)
          :config
          (smooth-scrolling-mode 1))

60. so-long

    For files whose lines are too long (no longer
    needed in Emacs 27+).
    
        (if (version< emacs-version "27")
          (use-package so-long
            :config
            (global-so-long-mode 1)))

61. swiper

    Smart searching with `ivy`.
    
        (use-package swiper
          :after ivy
          :bind (("C-c s" . swiper)
        	 ("H-s"   . swiper)))

62. symon

    Show some system stats when nothing else is going on.
    
        (use-package symon
          :init
          (setq symon-sparkline-type 'bounded
        	symon-delay          10
        	symon-monitors       '(symon-linux-cpu-monitor
        			       symon-linux-memory-monitor
        			       symon-linux-network-rx-monitor
        			       symon-linux-network-tx-monitor))
          :config
          (symon-mode))

63. telephone-line

    A slightly nicer mode-line (disabled in favor of `doom-modeline` for now).
    
        (use-package telephone-line
          :disabled
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

64. treemacs

    I'm now a fan of `dired`, but sometimes the "ineluctable modality of the 
    visible" is nice, so let's show some <span class="underline">dirs</span>.
    
        (use-package treemacs
          :disabled
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
        	("C-c n 1"   . treemacs-delete-other-windows)
        	("C-c n n"   . treemacs)
        	("C-c n b"   . treemacs-bookmark)
        	("C-c n M-t" . treemacs-find-tag)))
    
    1.  Treemacs Packages
    
        Some treemacs integration packages.
        
            (use-package treemacs-evil
              :disabled
              :after treemacs evil)
            
            (use-package treemacs-projectile
              :disabled
              :after treemacs projectile)
            
            (use-package treemacs-icons-dired
              :disabled
              :after treemacs dired
              :config (treemacs-icons-dired-mode))
            
            (use-package treemacs-magit
              :disabled
              :after treemacs magit)
            
            ;; this supposedly works with perspective but it fails
            (use-package treemacs-persp
              :disabled
              :after treemacs persp-mode
              :config (treemacs-set-scope-type 'Perspectives))
            
            ;; start with treemacs open (or not)
            ;; (treemacs)

65. undo-fu

    Undoing un-undoing is weird in Emacs.
    
        (use-package undo-fu
          :init
          (global-unset-key (kbd "C-z"))
          :bind ("C-z"   . undo-fu-only-undo)
        	("C-S-z" . undo-fu-only-redo))

66. use-package-ensure-system-package

    Ensure binaries.
    
        (use-package use-package-ensure-system-package)

67. vterm

    `vterm` can be an alternative to included shells.
    We also install `vterm-toggle`.
    
    Also, if you're on an older Ubuntu version (like my work PC),
    the `libvterm` package might be too old, but you could
    always try to build from source &#x2026;
    
        (unless (version< emacs-version "27.0")
          (use-package vterm
            :config
            (when (file-exists-p "/bin/fish")
              (setq vterm-shell "/bin/fish"))
            (setq vterm-kill-buffer-on-exit t))
          (use-package vterm-toggle
            :bind (("H-t"   . vterm-toggle)
        	   ("C-c t v" . vterm-toggle))
            :init
            (setq vterm-toggle-fullscreen-p nil)
            (add-to-list 'display-buffer-alist
        	     '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
        		(display-buffer-reuse-window display-buffer-in-side-window)
        		(side . bottom)
        		(dedicated . t)
        		(reusable-frames . visible)
        		(window-height . 0.3)))))

68. which-key

    Show the next possible key presses towards an action.
    
        (use-package which-key
          :diminish
          :init
          ;; big enough to not mess up avy line search
          (setq which-key-idle-delay 1.5)
          :config
          (which-key-mode))

69. writeroom-mode

    Create a room of one's own.
    I use a different (light) theme here.
    
        (use-package writeroom-mode
          :hook ((writeroom-mode-enable  . (lambda() (theme-light-switch 'secondary)))
        	 (writeroom-mode-disable . (lambda() (theme-light-switch 'primary))))
          :bind ("<f5>" . writeroom-mode))

70. yasnippet

    Use snippets in `prog` mode buffers.
    Because I also use company, `yas-expand` is mapped to `H-e`,
    if you don't have a hyper key, bind it to a personal binding.
    
        (use-package yasnippet-snippets
          :after yasnippet
          :config
          (yas-reload-all))
        
        (use-package yasnippet
          :delight " yas"
          :bind ((:map yas-minor-mode-map
        	      ("<tab>"    . nil)
        	      ("TAB"      . nil)
        	      ("H-<tab>"  . #'yas-expand)))
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

71. zoom

    Use the golden ratio between (in-)active buffers.
    
    This is buggy sometimes, so I prefer `golden-ratio`.
    
        (use-package zoom
         :disabled
         :diminish
         :init 
         (custom-set-variables
           '(zoom-size '(0.618 . 0.618)))
         :config
         (zoom-mode 1))


<a id="org2c1fd97"></a>

### Mode Configs

Configure major modes.

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

2.  crontab mode

    It's time to deal with this.
    
        (use-package crontab-mode)

3.  lisp mode

    Enable `flycheck`.
    
        (defun my-elisp-mode-hook ()
          "Hooks for lisp interaction mode."
          (flycheck-mode 1))
        
        (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

4.  css mode

    Just activate `flycheck` and tabs for now.
    
        (defun my-css-mode-hook ()
          "Hooks for css mode."
          (add-node-modules-path)
          (enable-tabs)
          (flycheck-mode))
        
        (add-hook 'css-mode-hook 'my-css-mode-hook)

5.  dockerfile mode

    Make `Dockerfiles` look nice.
    
        (use-package dockerfile-mode
          :mode "^Dockerfile")

6.  elixir mode

    Enable `flycheck`.
    
        (use-package elixir-mode
          :hook (elixir-mode . my-elixir-mode-hook))
        
        (defun my-elixir-mode-hook ()
          "Hooks for elixir mode."
          (lsp)
          (flycheck-mode))

7.  haskell mode

    Don't use haskell much yet.
    
        (use-package haskell-mode)

8.  json mode

    Enable tabs and `flycheck`.
    
        (defun my-json-mode-hook ()
          "Hooks for json mode."
          (when (y-or-n-p "Do you want to enables tabs?")
            (enable-tabs))
          (flycheck-mode 1)
          (rainbow-delimiters-mode))
        
        (use-package json-mode
          :hook (json-mode . my-json-mode-hook))

9.  js2 mode

    Enable `flycheck` and disable internal checker.
    
        (use-package js2-mode
          :mode "\\.js\\'"
          :init
          (setq-default js2-show-parse-errors nil
        		js2-strict-missing-semi-warning nil)
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
        	nil)))

10. lua mode

    Why not. It can be *awesome*.
    
        (use-package lua-mode)

11. markdown mode

    Markdown. Sometimes you need it.
    
        (use-package markdown-mode)

12. org mode

    Org mode is the best thing about Emacs. Check out the [manual](https://orgmode.org/manual/).
    
    1.  The Mode Itself
    
        Use bullets mode and make the ellipses bendy arrows. When a `TODO` is `DONE`, log time.
        We also make the sequence from `TODO` to `DONE` more granular and add another `DONE`-like
        state `CANCELLED`.
        
            (use-package org-bullets
              :hook (org-mode . (lambda() (org-bullets-mode t))))
            
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
              (setq org-ellipsis                   "↷"
            	org-log-done                   t
            	org-startup-truncated          nil
            	org-startup-folded             'overview
            	org-directory                  my-org-directory
            	org-default-notes-file         (concat org-directory "/notes.org")
            	org-startup-with-inline-images t
            	;; be sure to add archive tag with org-toggle-archive-tag
            	org-archive-location           "::* Archived"
            	org-todo-keywords
            	  '((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
              (add-to-list 'org-global-properties
            	       '("Effort_ALL". "30m 1h 2h 4h 6h 1d 2d")))
            
            (setq org-log-done                           'time
                  org-clock-idle-time                    nil
                  org-clock-continuously                 nil
                  org-clock-persist                      t
                  org-clock-in-switch-to-state           "IN PROGRESS"
                  org-clock-in-resume                    nil
                  org-clock-report-include-clocking-task t
                  org-clock-out-remove-zero-time-clocks  t
                  ;; Too many clock entries clutter up a heading
                  org-log-into-drawer                    t
                  org-clock-into-drawer                  1)
            
            (require 'org-install)
            (setq org-modules                     '(org-habit org-info)
                  org-habit-graph-column          105
                  ;; this doesn't seem to affect anything
                  org-archive-subtree-save-file-p t)
            
            (org-load-modules-maybe t)
            
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
            
            ;; tags differentiated by # and @
            (setq org-tag-alist '(
              ;; depth
              ("#immersive" . ?i)
              ("#process"   . ?p)
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
        
        This is mostly based on [mwfogleman](https://github.com/mwfogleman/.emacs.d/blob/master/michael.org)'s Emacs config.
        
            (use-package org-super-agenda
              :init
              (setq org-super-agenda-groups
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
            	      ))
              ;; not sure why this can't be in config
              (org-super-agenda-mode)
              :bind ("C-c a" . org-agenda))
            
            ;; we hide all @-tags
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
    
    3.  Presentations
    
        Use `org-tree-slide` for presentations.
        
            (use-package org-tree-slide
              :hook ((org-tree-slide-play . (lambda () (beacon-mode -1)))
            	 (org-tree-slide-stop . (lambda () (beacon-mode 1))))
              :bind (("<f7>" . org-tree-slide-mode)
            	 (:map org-tree-slide-mode-map
            	  ("n" . org-tree-slide-move-next-tree)
            	  ("p" . org-tree-slide-move-previous-tree))))

13. python mode

    Enable `flycheck`.
    This mode is built-in.
    
        (use-package python
          :hook (python-mode . my-python-mode-hook)
          :init
          ;; use python3 as default python command
          (setq py-python-command        "python3"
        	python-shell-interpreter "python3"))
        
        (defun my-python-mode-hook ()
          "Hooks for python mode."
          (message "Sssnake_case!")
          (flycheck-mode 1)
          (lsp)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

14. rjsx mode

    Pretty much like `js2`.
    
        (use-package rjsx-mode
          :mode "\\.jsx\\'"
          :hook (rjsx-mode . my-rjsx-mode-hook))
        
        (defun rjsx-indent ()
          (interactive)
          (setq-local indent-line-function 'js-jsx-indent-line))
        
        (defun my-rjsx-mode-hook ()
          "Hooks for rjsx mode."
          (add-node-modules-path)
          (enable-tabs)
          (flycheck-mode)
          (rjsx-indent)
          (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
        	nil)))

15. typescript mode

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

16. web mode

    Web mode uses `flycheck`, prompts user if `lsp` should be enabled.
    
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

17. yam mode

    Sometimes you need YAMLs.
    
        (use-package yaml-mode)


<a id="orgf028dca"></a>

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

<sup><a id="fn.1" href="#fnr.1">1</a></sup> I jumped ship from `26.3`. Most of the things will work there.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> What you're reading is likely a markdown version exported from it.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> If you're feeling adventurous, [build from source](https://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL).

<sup><a id="fn.4" href="#fnr.4">4</a></sup> If you're not sure where your `user-emacs-directory` might be,
you can do the following:

-   run Emacs
-   hit `M-x` (that is your Alt/Option key followed by the letter `x`)
-   type `describe-variable` and hit return
-   type `user-emacs-directory` and hit return again

A window (or is it a frame?) should pop up telling you the path

Finally run `git clone git@gitlab.com:Walheimat/emacs-config.git ~/.emacs.d`
(replace `~/.emacs.d` with your actual path if it differs)

<sup><a id="fn.5" href="#fnr.5">5</a></sup> This config uses the `all-the-icons` package
whose icons need to be downloaded manually
by running `M-x all-the-icons-install-fonts` and selecting `yes`.

This config uses `dash`.

We will try to install it before installing the other packages
but this might fail.

If that is the case do the following:

-   hit `M-x`, type `package-install` and hit return
-   type `dash` and hit return again
-   once the installation is complete, re-run Emacs

<sup><a id="fn.6" href="#fnr.6">6</a></sup> 107 code blocks, to be exact.

<sup><a id="fn.7" href="#fnr.7">7</a></sup> Send me an email, why don't you?

<sup><a id="fn.8" href="#fnr.8">8</a></sup> Repositories (almost complete):

-   [ace-window](https://github.com/abo-abo/ace-window)
-   [add-node-modules-path](https://github.com/codesuki/add-node-modules-path)
-   [ag](https://github.com/Wilfred/ag.el)
-   [all-the-icons](https://github.com/domtronn/all-the-icons.el)
-   [auto-package-update](https://github.com/rranelli/auto-package-update.el)
-   [avy](https://github.com/abo-abo/avy)
-   [beacon](https://github.com/Malabarba/beacon)
-   [bm](https://github.com/joodland/bm)
-   [company](https://company-mode.github.io/)
-   [crux](https://github.com/bbatsov/crux)
-   [dap-mode](https://github.com/emacs-lsp/dap-mode)
-   [dash](https://github.com/magnars/dash.el)
-   [diff-hl](https://github.com/dgutov/diff-hl)
-   [dimmer](https://github.com/gonewest818/dimmer.el)
-   [dired-filter](https://github.com/Fuco1/dired-hacks/)
-   [docker](https://github.com/Silex/docker.el)
-   [doom-modeline](https://github.com/seagle0128/doom-modeline)
-   [drag-stuff](https://github.com/rejeep/drag-stuff.el)
-   [dumb-jump](https://github.com/jacktasia/dumb-jump)
-   [elixir-mode](https://github.com/elixir-editors/emacs-elixir)
-   [esh-autosuggest](https://github.com/dieggsy/esh-autosuggest/)
-   [evil-nerd-commenter](https://github.com/redguardtoo/evil-nerd-commenter)
-   [expand-region](https://github.com/magnars/expand-region.el)
-   [find-file-in-project](https://github.com/technomancy/find-file-in-project)
-   [fira-code](https://github.com/jming422/fira-code-mode)
-   [flycheck](https://github.com/flycheck/flycheck)
-   [flyspell-correct](https://github.com/d12frosted/flyspell-correct)
-   [golden-ratio](https://github.com/roman/golden-ratio.el)
-   [google-this](https://github.com/Malabarba/emacs-google-this)
-   [haskell-mode](https://github.com/haskell/haskell-mode)
-   [highlight-indent-guide](https://github.com/zk-phi/indent-guide)
-   [highlight-numbers](https://github.com/Fanael/highlight-numbers)
-   [hydra](https://github.com/abo-abo/hydra)
-   [ivy-rich](https://github.com/Yevgnen/ivy-rich)
-   [ivy/swiper](https://github.com/abo-abo/swiper)
-   [js2-mode](https://github.com/mooz/js2-mode/)
-   [kaolin](https://github.com/ogdenwebb/emacs-kaolin-themes)
-   [kubernetes](https://github.com/chrisbarrett/kubernetes-el)
-   [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
-   [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
-   [lua-mode](https://github.com/immerrr/lua-mode/tree/345ebfc1e236d9676e7e9f7364493785e7756348)
-   [magit](https://magit.vc/)
-   [markdown-mode](https://jblevins.org/projects/markdown-mode/)
-   [mode-line-bell](https://github.com/purcell/mode-line-bell)
-   [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
-   [mwim](https://github.com/alezost/mwim.el)
-   [org-mode](https://orgmode.org/)
-   [org-present](https://github.com/rlister/org-present)
-   [org-super-agenda](https://github.com/alphapapa/org-super-agenda)
-   [origami](https://github.com/gregsexton/origami.el)
-   [perspective](https://github.com/nex3/perspective-el)
-   [prettier-js](https://github.com/prettier/prettier-emacs/tree/e9b73e81d3e1642aec682195f127a42dfb0b5774)
-   [projectile](https://github.com/bbatsov/projectile)
-   [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
-   [rainbow-mode](https://github.com/emacsmirror/rainbow-mode)
-   [request](https://github.com/tkf/emacs-request)
-   [restart-emacs](https://github.com/iqbalansari/restart-emacs)
-   [rjsx-mode](https://github.com/felipeochoa/rjsx-mode)
-   [s](https://github.com/magnars/s.el)
-   [smartparens](https://github.com/Fuco1/smartparens)
-   [smex](https://github.com/nonsequitur/smex/)
-   [smooth-scrolling](https://github.com/aspiers/smooth-scrolling/tree/2462c13640aa4c75ab3ddad443fedc29acf68f84)
-   [symon](https://github.com/zk-phi/symon)
-   [telephone-line](https://github.com/dbordak/telephone-line)
-   [treemacs](https://github.com/Alexander-Miller/treemacs)
-   [undo-fu](https://gitlab.com/ideasman42/emacs-undo-fu)
-   [use-package](https://github.com/jwiegley/use-package)
-   [vterm](https://github.com/akermu/emacs-libvterm)
-   [vterm-toggle](https://github.com/jixiuf/vterm-toggle)
-   [web-mode](http://web-mode.org/)
-   [which-key](https://github.com/justbur/emacs-which-key)
-   [writeroom-mode](https://github.com/joostkremers/writeroom-mode)
-   [yasnippet](https://github.com/joaotavora/yasnippet)
-   [zone](https://www.emacswiki.org/emacs/ZoneMode)
-   [zoom](https://github.com/cyrus-and/zoom)

<sup><a id="fn.9" href="#fnr.9">9</a></sup> All languages listed [here](https://emacs-lsp.github.io/lsp-mode/page/languages/).

Currently I only need three:

-   [Elixir](https://github.com/elixir-lsp/elixir-ls)
-   [Prolog](https://emacs-lsp.github.io/lsp-mode/page/lsp-prolog/)
-   [Python](https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/)
