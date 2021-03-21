;;; .emacs --- the init file

;;; Commentary:

;; Simplified init file.
;;
;; This uses `org-babel-load-file' to create a Lisp script.

;;; Code:

(defvar wal/emacs-config-default-path
  (expand-file-name "emacs-config" user-emacs-directory)
  "The default path to Walheimat's Emacs org config.")

(let ((gc-cons-threshold most-positive-fixnum)
	  (gc-cons-percentage 0.6))
  (org-babel-load-file
    (expand-file-name "README.org" wal/emacs-config-default-path)))

;; just an example: changing primary and secondary themes
;; (setq wal/primary-emacs-theme   'doom-dracula
;;       wal/secondary-emacs-theme 'doom-opera-light)

;;; .emacs ends here
