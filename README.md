

# emacs config

The file can be loaded using `(org-babel-load-file "~/.emacs.d/configuration.org")` (assuming that's the file's location).

Check out [my init file](https://gitlab.com/Walheimat/emacs-config/-/blob/master/.emacs) for reference.

**Note** that this config uses the \`all-the-icons\` package whose icons need to be downloaded manually
by running `M-x all-the-icons-install-fonts` and selecting `yes`.


# Table of Contents

1.  [emacs config](#org51cff88)
    1.  [before init](#org24c193d)
    2.  [global](#org8dc2917)
    3.  [specific](#org054c3fa)
    4.  [modes](#org31f3b9a)


<a id="org24c193d"></a>

## before init

Set up emacs, package manager and packages.


### general

No splash. Use separate file for customizations (so we don't clutter up our init file). Use python3.

    (package-initialize)
    (setq inhibit-startup-message t)
    ;; (setq initial-scratch-message nil)
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (setq py-python-command "python3")
    (load custom-file)


### MELPA

Initialize MELPA.

    (require 'package)
    (require 'dash)
    
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


### packages

Install packages (if they're missing).

    (defun packages-install (packages)
      (--each packages
        (when (not (package-installed-p it))
          (package-install it)))
      (delete-other-windows))
    
    (defun init--install-packages ()
      (packages-install
       '(
         ace-window
         ack
         add-node-modules-path
         all-the-icons
         ample-theme
         angular-mode
         beacon
         company
         company-box
         company-lsp
         company-restclient
         company-web
         dap-mode
         dash
         diff-hl
         diminish
         dimmer
         docker
         doom-themes
         drag-stuff
         dumb-jump
         elixir-mode
         esh-autosuggest
         eshell-prompt-extras
         evil
         evil-magit
         evil-nerd-commenter
         evil-vimish-fold
         find-file-in-project
         flycheck
         focus
         git-timemachine
         highlight-numbers
         highlight-indent-guides
         hydra
         ivy
         json-mode
         js2-mode
         kaolin-themes
         lsp-mode
         magit
         markdown-mode
         nodejs-repl
         org-bullets
         perspective
         posframe
         prettier-js
         projectile
         rainbow-delimiters
         restclient
         rjsx-mode
         s
         shell-pop
         smex
         treemacs
         telephone-line
         treemacs-evil
         typescript-mode
         use-package
         vimish-fold
         web-mode
         which-key
         yaml-mode
         yasnippet
         zoom
         )))
    
    (condition-case nil
        (init--install-packages)
      (error
       (package-refresh-contents)
       (init--install-packages)))


### site-lisp

Add side lisp directory and subdirs to load path.

    (setq site-lisp-dir
          (expand-file-name "site-lisp" user-emacs-directory))
    (add-to-list 'load-path site-lisp-dir)
    (dolist (project (directory-files site-lisp-dir t "\\w+"))
      (when (file-directory-p project)
        (add-to-list 'load-path project)))


<a id="org8dc2917"></a>

## global

Configure global settings.


### save place

Save places, and do so in a file.

    (setq save-place-file (expand-file-name ".places" user-emacs-directory))


### autosave and backups

Store backups in backups folder. Store autosaves in temp folder. We don't want this to mess with git.

    (setq backup-directory-alist
      `(("." . ,(expand-file-name
        (concat user-emacs-directory "backups")))))
    (setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


### global modes

Turn on a lot of useful (and prettifying) modes.

    (show-paren-mode 1)
    (global-auto-revert-mode t)
    (global-hl-line-mode)
    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (global-display-line-numbers-mode)
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


### reasonable settings

Insertion of text should delete region. Bracket pairs should be highlighted. Window (or frame &#x2026;) should start maximized. Garbage collection and memory.

    ;; show right away please
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
      "Slantend and enchanted."
      (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
      (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
    )
    (add-hook 'font-lock-mode-hook 'my-font-lock-hook)


### tabs all the way

Tabs are 4 spaces wide. No electric indent. Pipe char to show indentation. Commands to enable/disable sane tabs.

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
    
    ;; (setq whitespace-style '(face tabs tab-mark trailing))
    ;; (custom-set-faces
    ;;   '(whitespace-tab ((t (:foreground "#636363")))))
    ;; (setq whitespace-display-mappings
    ;;   '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'


### key bindings

Change up the key bindings a bit.

-   `C-x g` opens magit status.
-   `M-x` opens smex.
-   `s-,` to comment.
-   `s-a` to use ack. <span class="underline">Requires ack</span>!
-   `C-x r q` to (really) quit.
-   `C-x C-c` to open this config file.
-   `M-o` to go to "other" window.
-   `C-x j` to dumb-jump.
-   `C-x t m` to open timemachine.
-   `s-s` turn on flyspell prog mode.
-   `C-x p f` find file in project.

Do we really need a line here?

    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "s-,") 'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-a") 'ack)
    (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
    (global-set-key
      (kbd "C-x C-c")
      (lambda () (interactive)(switch-to-buffer (find-file-noselect "~/.emacs.d/configuration.org"))))
    (global-set-key (kbd "M-o") 'ace-window)
    (global-set-key (kbd "C-x j") 'dumb-jump-go)
    (global-set-key (kbd "C-x t m") 'git-timemachine-toggle)
    (global-set-key (kbd "s-s") 'flyspell-prog-mode)
    (global-set-key (kbd "C-x p f") 'find-file-in-project)


### theme

Just pick a theme. This one is based on Jon Blow's and pretty cool.

    (load-theme 'naysayer t)
    ;; (load-theme 'monokai t)


### font size

Prefer mononoki (-> FiraCode -> Liberation -> DejaVu). If emacs runs with the custom arg `-bigger`, the default font size is 14 (instead of 10).

    (require 'dash)
    (defun font-candidate (&rest fonts)
      "Return the first available font from a list of fonts."
      (--first (find-font (font-spec :name it)) fonts))
    
    (set-face-attribute 'default nil :font (font-candidate '"mononoki 12" "Fira Code 14" "Liberation Mono 12" "DejaVu Sans Mono 12"))
    
    (defun found-custom-arg (switch)
      "Check for custom arg and delete it right away so emacs doesn't complain."
      (let ((found-switch (member switch command-line-args)))
        (setq command-line-args (delete switch command-line-args))
        found-switch))
    
    (if (found-custom-arg "-bigger")
      (set-default-font (font-candidate '"mononoki 14" "Fira Code 14" "Liberation Mono 14" "DejaVu Sans Mono 14"))
    )


### fun stuff

Zone out after a minute.

    (require 'zone)
    (zone-when-idle 180)


### func stuff

Add some functions.

    ;; kill all other buffers
    (defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
    ;; check if buffer is treemacs buffer
    ;; similar to minibufferp
    (defun treemacsbufferp ()
      "Check if this is the treemacs buffer."
      (eq (current-buffer) (treemacs-get-local-buffer)))


<a id="org054c3fa"></a>

## specific

Configure specific packages/aspects.


### company

Set up company-box

    (require 'company-box)
    (add-hook 'company-mode-hook 'company-box-mode)
    (setq company-minimum-prefix-length 3)
    (setq company-idle-delay 0.5)


### docker

Key binding.

    (use-package docker
      :ensure t
      :bind ("C-c d" . docker))


### dap

Require stuff.

    (require 'dap-node)
    (dap-auto-configure-mode 1)
    (dap-register-debug-template
      "Node::Attach"
      (list :type "node"
    	:request "attach"
    	:remoteRoot "/usr/src/app"
    	:localRoot "/home/krister/theventury"
    	:name "Node::Attach"))


### diff-hl

Refresh post magit.

    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


### diminish

    (require 'diminish)
    (diminish 'company-mode)
    (diminish 'ivy-mode)
    (diminish 'company-box-mode)
    (diminish 'beacon-mode)
    (diminish 'zoom-mode)
    (diminish 'which-key-mode)
    (diminish 'eldoc-mode)
    (diminish 'highlight-indent-guides-mode)


### dimmer

Make dimmed frames a bit dimmer.

    (require 'dimmer)
    (setq dimmer-fraction 0.3)
    (dimmer-configure-org)
    (dimmer-configure-magit)
    (dimmer-configure-hydra)
    (setq dimmer-adjustmentmode :both)


### drag stuff

Use the default key bindings.

    (require 'drag-stuff)
    (drag-stuff-define-keys)


### dumb-jump

Use ivy. We have ivy.

    (setq dumb-jump-selector 'ivy)


### eshell

Set up eshell.

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


### flycheck

Only check on save. Configure threshold and (unused) idle-change delay.

    (defun my-flycheck-hook()
      (setq flycheck-check-syntax-automatically '(save idle-change))
      (setq flycheck-checker-error-threshold 100)
      (setq flycheck-idle-change-delay 2.5))
    ;; (add-hook 'flycheck-mode-hook 'my-flycheck-hook)

1.  override finding eslint

    Eslint configs can be found using a file, not a directory.
    
        (require 'flycheck)
        (defun flycheck-eslint-config-exists-p ()
          "Whether there is a valid eslint config for the current buffer."
          (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
        	 (exitcode (and executable (call-process executable nil nil nil
        						 "--print-config" ".eslintrc"))))
            (eq exitcode 0)))

2.  load eslint/tslint from local node modules

    Use the locally installed eslint/tslint.
    
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
        
        (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
        (add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)

3.  function to switch between tslint and lsp

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


### flyspell

There could be too many messages.

    (setq flyspell-issue-message-flag nil)


### highlight-indent-guides

    (setq highlight-indent-guides-method 'character)


### kaolin

Apply kaolin theme to treemacs.

    (require 'kaolin-themes)
    (kaolin-treemacs-theme)
    (setq kaolin-ocean-alt-bg t)
    ;; Enable distinct background for fringe and line numbers.
    ;; (setq kaolin-themes-distinct-fringe t)  
    
    ;; Enable distinct colors for company popup scrollbar.
    ;; (setq kaolin-themes-distinct-company-scrollbar t)


### lsp

Prefer capf, bigger delay, configure for angular.

    ;; (setq lsp-prefer-capf t)
    ;; (setq lsp-idle-delay 0.500)
    ;; (setq lsp-semantic-highlighting t)
    (setq lsp-clients-angular-language-server-command
      '("node"
        "/home/krister/.config/nvm/12.16.1/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/home/krister/.config/nvm/12.16.1/lib/node_modules"
        "--tsProbeLocations"
        "/home/krister/.config/nvm/12.16.1/lib/node_modules"
        "--stdio"))


### mode mappings

Set up mode mappings.

    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))
    (add-to-list 'auto-mode-alist '("\\.component.html" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.component.css" . css-mode))
    (add-to-list 'auto-mode-alist '("\\.json" . json-mode))


### prettier-js

Require so it can be used outside of minor mode.

    (require 'prettier-js)


### treemacs

Less indentation. Never other window.

    (use-package treemacs
      :ensure t
      :defer t
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (progn
        (setq treemacs-indentation                   1
    	  treemacs-indentation-string            " ⁝ "
    	  treemacs-is-never-other-window         t
    	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
    	  treemacs-show-hidden-files             t)
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
    	("C-x t 1"   . treemacs-delete-other-windows)
    	("C-x t t"   . treemacs)
    	("C-x t B"   . treemacs-bookmark)
    	("C-x t C-t" . treemacs-find-file)
    	("C-x t M-t" . treemacs-find-tag)))
    
    ;; (use-package treemacs-evil
    ;;   :after treemacs evil
    ;;   :ensure t)
    
    (use-package treemacs-projectile
      :after treemacs projectile
      :ensure t)
    
    (use-package treemacs-icons-dired
      :after treemacs dired
      :ensure t
      :config (treemacs-icons-dired-mode))
    
    (use-package treemacs-magit
      :after treemacs magit
      :ensure t)
    
    (use-package treemacs-persp
      :after treemacs persp-mode
      :ensure t
      :config (treemacs-set-scope-type 'Perspectives))
    (treemacs)


### telephone-line

A slightly nicer modeline.

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
          telephone-line-secondary-left-separator 'telephone-line-identity-hollow-right)
    (telephone-line-mode t)


### vimish

Trying out evil.

    ;; (require 'evil)
    ;; (require 'vimish-fold)
    ;; (require 'evil-vimish-fold)
    ;; (setq evil-magit-state 'emacs)
    ;; (setq evil-magit-emacs-to-default-state-modes nil)
    ;; (require 'evil-magit)
    ;; change mode-line color by evil state
    ;; (require 'cl)
    ;; (lexical-let ((default-color (cons (face-background 'mode-line)
    ;;                                    (face-foreground 'mode-line))))
    
    ;; (defun color-mode-line()
    ;;   (let ((color (cond ((minibufferp) default-color)
    ;;                      ((treemacsbufferp) default-color)
    ;;                      ((evil-insert-state-p) '("#9932CC" . "#ffffff"))
    ;;                      ;; ((evil-emacs-state-p)  '("#ff6347" . "#ffffff"))
    ;;                      ((buffer-modified-p)   '("#db7093" . "#ffffff"))
    ;;                      (t default-color))))
    ;;     (set-face-background 'mode-line (car color))
    ;;     (set-face-foreground 'mode-line (cdr color))))
    
    ;; (add-hook 'post-command-hook 'color-mode-line)
    ;; (defun all-evil()
    ;;   (message "going all evil")
    ;;   (evil-mode 1))
    ;;   (evil-vimish-fold-mode))
    ;; (setq evil-default-state 'emacs)
    
    ;; (add-hook 'evil-normal-state-entry-hook
    ;;   (lambda ()
    ;;     (message "Switching to normal state")
    ;;     (setq evil-magit-emacs-to-default-state-modes '(git-commit-mode))
    ;;     (setq evil-magit-state 'normal)))
    ;; (add-hook 'evil-normal-state-exit-hook
    ;;   (lambda ()
    ;;      (message "Switching to emacs state")
    ;;      (setq evil-magit-emacs-to-default-state-modes nil)
    ;;      (setq evil-magit-state 'emacs)))
    ;; (all-evil)


<a id="org31f3b9a"></a>

## modes

Configure modes.


### css mode

Just activate flycheck and tabs for now.

    (defun my-css-mode-hook ()
      "Hooks for css mode."
      (add-node-modules-path)
      (enable-tabs)
      (flycheck-mode))
    
    (add-hook 'css-mode-hook 'my-css-mode-hook)


### js2 mode

Enable Flycheck and disable internal checker.

    (setq-default js2-show-parse-errors nil)
    (setq-default js2-strict-missing-semi-warning nil)
    
    (defun my-js2-mode-hook ()
      "Hooks for js2 mode."
      (enable-tabs)
      (add-node-modules-path)
      (flycheck-mode 1)
      (rainbow-delimiters-mode)
      (drag-stuff-mode)
      (add-hook 'local-write-file-hooks
        (lambda ()
          (delete-trailing-whitespace)
    	nil))
    )
    (add-hook 'js2-mode-hook 'my-js2-mode-hook)


### org mode

1.  Make org-mode look nicer

    Use bullets mode and make the ellipses bendy arrows.
    
        (add-hook 'org-mode-hook (lambda() (org-bullets-mode t)))
        (setq org-ellipsis "↷")

2.  Make org-mode log with notes

    When a `TODO` is `DONE` log a note.
    
        (setq org-log-done 'note)


### python mode

Enable flycheck.

    (defun my-python-mode-hook ()
      "Hooks for python mode."
      (flycheck-mode 1)
      (add-hook 'local-write-file-hooks
        (lambda ()
          (delete-trailing-whitespace)
    	nil))
    )
    (add-hook 'python-mode-hook 'my-python-mode-hook)


### rjsx mode

Pretty much like js2.

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
    (add-hook 'rjsx-mode-hook 'my-rjsx-mode-hook)


### typescript mode

Enable lsp, flycheck and sane tabs. And some other stuff.

    (defun my-typescript-mode-hook ()
      "Hooks for typescript mode."
      (enable-tabs)
      (add-node-modules-path)
      (flycheck-mode 1)
      (lsp)
      (rainbow-delimiters-mode)
      (add-hook 'local-write-file-hooks
        (lambda ()
          (delete-trailing-whitespace)
    	nil)))
    
    (add-hook 'typescript-mode-hook 'my-typescript-mode-hook)


### web mode

Web mode uses flycheck with lsp enabled.

    (require 'web-mode)
    (setq web-mode-comment-style 2)
    (add-to-list 'web-mode-comment-formats '("vue" . "//"))
    (defun my-web-mode-hook ()
      "Hooks for web mode."
      (enable-tabs)
      (web-mode-use-tabs)
      (add-node-modules-path)
      (lsp)
      (flycheck-mode)
      (add-hook 'local-write-file-hooks
        (lambda ()
          (delete-trailing-whitespace)
    	nil)))
    
    (add-hook 'web-mode-hook 'my-web-mode-hook)


### zoom mode

Use the golden ratio.

    (custom-set-variables
     '(zoom-size '(0.618 . 0.618)))

