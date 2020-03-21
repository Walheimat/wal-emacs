

# emacs config

The file can be loaded using `(org-babel-load-file "~/.emacs.d/configuration.org")` (assuming that's the file's location).

Check out [my init file](https://gitlab.com/Walheimat/emacs-config/-/blob/master/.emacs) for reference.


# Table of Contents

1.  [emacs config](#orgd391a44)
    1.  [before init](#org30b5154)
    2.  [global](#org3163ef2)
    3.  [specific](#org2f42aeb)
    4.  [modes](#org80fe783)


<a id="org30b5154"></a>

## before init

Set up emacs, package manager and packages.


### general

No splash. Use separate file for customizations.

    (package-initialize)
    (setq inhibit-startup-message t)
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
         ack
         add-node-modules-path
         ample-theme
         ;; angular-mode
         ;; angular-snippets
         auto-complete
         browse-kill-ring
         company
         company-lsp
         company-restclient
         company-web
         diff-hl
         dimmer
         dumb-jump
         evil-nerd-commenter
         find-file-in-project
         flycheck
         focus
         hydra
         ivy
         js2-mode
         lsp-mode
         magit
         markdown-mode
         nodejs-repl
         org-bullets
         origami
         perspective
         prettier-js
         projectile
         rainbow-delimiters
         restclient
         rjsx-mode
         s
         smex
         treemacs
         typescript-mode
         use-package
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


<a id="org3163ef2"></a>

## global

Configure global settings.


### save place

Save places, and do so in a file.

    (setq save-place-file (expand-file-name ".places" user-emacs-directory))


### autosave and backups

Store backups in backups folder. Store autosaves in temp folder.

    (setq backup-directory-alist
      `(("." . ,(expand-file-name
        (concat user-emacs-directory "backups")))))
    (setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


### reasonable settings

Insertion of text should delete region. Bracket pairs should be highlighted. Window (or frame &#x2026;) should start maximized.

    (delete-selection-mode 1)
    (show-paren-mode 1)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))


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
    
    (setq whitespace-style '(face tabs tab-mark trailing))
    (custom-set-faces
      '(whitespace-tab ((t (:foreground "#636363")))))
    (setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'


### key bindings

Change up the key bindings a bit.

-   `C-x g` opens magit status.
-   `M-x` opens smex.
-   `C-x C-y` to browse kill ring.
-   `s-,` to comment.
-   `s-a` to use ack. <span class="underline">Requires ack</span>!
-   `C-x r q` to (really) quit.
-   `C-x C-c` to open this config file.

    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "C-x C-y") 'browse-kill-ring)
    (global-set-key (kbd "s-,") 'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-a") 'ack)
    (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
    (global-set-key
      (kbd "C-x C-c")
      (lambda () (interactive)(switch-to-buffer (find-file-noselect "~/.emacs.d/configuration.org"))))


### theme

Use ample flat.

    (load-theme 'doom-spacegrey t)


### global modes

Turn on a lot of useful (and prettifying) modes.

    (global-hl-line-mode)
    (add-hook 'after-init-hook 'global-company-mode)
    (global-display-line-numbers-mode)
    (ivy-mode 1)
    (global-prettify-symbols-mode +1)
    (global-diff-hl-mode)
    (dimmer-mode t)
    (global-whitespace-mode)
    (save-place-mode 1)
    (dumb-jump-mode)
    (which-key-mode)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (zoom-mode 1)


### font size

Prefer mononoki (-> FiraCode -> Liberation -> DejaVu). If emacs runs with the custom arg `-bigger`, the default font size is 14 (instead of 10).

    (require 'dash)
    (defun font-candidate (&rest fonts)
      "Return the first available font from a list of fonts."
      (--first (find-font (font-spec :name it)) fonts))
      (set-face-attribute 'default nil :font (font-candidate '"mononoki 14" "Fira Code 14" "Liberation Mono 14" "DejaVu Sans Mono 14"))
    
    (defun found-custom-arg (switch)
      "Check for custom arg and delete it right away so emacs doesn't complain."
      (let ((found-switch (member switch command-line-args)))
        (setq command-line-args (delete switch command-line-args))
        found-switch))
    
    (unless (found-custom-arg "-bigger")
      (set-default-font (font-candidate '"mononoki 10" "Fira Code 10" "Liberation Mono 10" "DejaVu Sans Mono 10"))
    )


<a id="org2f42aeb"></a>

## specific

Configure packages.


### autocomplete

Use default configuration.

    (ac-config-default)


### dimmer

Make dimmed frames a bit dimmer.

    (require 'dimmer)
    (setq dimmer-fraction 0.2)
    (dimmer-configure-org)
    (dimmer-configure-magit)
    (dimmer-configure-hydra)


### find file in project

Bind `C-x p f` to `find-file-in-project`.

    (use-package find-file-in-project
      :bind
      (:map global-map
        ("C-x p f" . find-file-in-project)))


### flycheck

Make flycheck understand newer eslint.

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


### mode mappings

Set up mode mappings.

    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))


### origami

Define keys.

    (require 'origami)
    (define-key origami-mode-map (kbd "C-x #") 'origami-toggle-node)


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
    	  treemacs-no-delete-other-windows       nil
    	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
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


<a id="org80fe783"></a>

## modes

Configure modes.


### js2 mode

Enable Flycheck and disable internal checker. I use this mode to test some minor modes like origami.

    (setq-default js2-show-parse-errors nil)
    (setq-default js2-strict-missing-semi-warning nil)
    (add-hook 'js2-mode-hook (lambda () (add-node-modules-path)))
    (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
    (add-hook 'js2-mode-hook 'enable-tabs)
    (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'js2-mode-hook 'focus-mode)
    (add-hook 'js2-mode-hook 'origami-mode)


### lsp

Use lsp in web mode (for vetur).

    (add-hook 'web-mode-hook 'lsp)
    (add-hook 'web-mode-hook 'flycheck-mode)


### org mode

1.  Make org-mode look nicer

    Use bullets mode and make the ellipses bendy arrows.
    
        (add-hook 'org-mode-hook (lambda() (org-bullets-mode t)))
        (setq org-ellipsis "↷")

2.  Make org-mode log with notes

    When a `TODO` is `DONE` log a note.
    
        (setq org-log-done 'note)


### rjsx mode

Enable Flycheck.

    (add-hook 'rjsx-mode-hook (lambda () (add-node-modules-path)))
    (add-hook 'rjsx-mode-hook (lambda () (flycheck-mode 1)))
    (add-hook 'rjsx-mode-hook 'enable-tabs)
    (add-hook 'rjsx-mode-hook (lambda () (setq-local indent-line-function 'js-jsx-indent-line)))


### typescript mode

Enable flycheck and sane tabs.

    (add-hook 'typescript-mode-hook (lambda () (add-node-modules-path)))
    (add-hook 'typescript-mode-hook (lambda () (flycheck-mode 1)))
    (add-hook 'typescript-mode-hook 'enable-tabs)
    (add-hook 'typescript-mode-hook (lambda () (focus-mode)))


### web mode

Web mode uses flycheck with tslint enabled.

    (add-hook 'web-mode-hook 'enable-tabs)


### zoom mode

Use the golden ratio.

    (custom-set-variables
     '(zoom-size '(0.618 . 0.618)))

