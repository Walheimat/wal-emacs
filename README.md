

# emacs config

The file can be loaded using `(org-babel-load-file "~/.emacs.d/configuration.org")` (assuming that's the file's location).

Check out [my init file](https://gitlab.com/Walheimat/emacs-config/-/blob/master/.emacs) for reference.


# Table of Contents

1.  [emacs config](#org600bb60)
    1.  [before init](#orgbb4a64c)


<a id="orgbb4a64c"></a>

## before init

Set up emacs, package manager and packages.


### general

No splash. Use separate file for customizations (so we don't clutter up our init file).

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
         ace-window
         ack
         add-node-modules-path
         ample-theme
         angular-mode
         beacon
         browse-kill-ring
         color-identifiers-mode
         company
         company-lsp
         company-restclient
         company-web
         diff-hl
         dimmer
         doom-themes
         drag-stuff
         dumb-jump
         evil-nerd-commenter
         find-file-in-project
         flycheck
         focus
         git-timemachine
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
         posframe
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


### site-lisp

Keeping this empty for now &#x2026;

