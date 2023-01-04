;;; wal-prelude.el --- Config setup functionality. -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps Walheimat's literate configuration by tangling
;; the base Org file's source blocks and subsequently loading package
;; `wal'.
;;
;; Refer to the provided init.el file in templates for an example of
;; its usage.

;;; Code:

;;;; Variables:

(declare-function org-babel-tangle-file "ob-tangle")

(defconst wal/packages '(wal
                         wal-func
                         wal-external
                         wal-key-bindings
                         wal-settings
                         wal-look
                         wal-fonts
                         ;; The following packages are optional.
                         wal-emacs
                         wal-edit
                         wal-movement
                         wal-find
                         wal-complete
                         wal-workspace
                         wal-windows
                         wal-org
                         wal-dired
                         wal-terminal
                         wal-vc
                         wal-visuals
                         wal-lang
                         wal-fix
                         wal-lsp
                         wal-devops
                         wal-web
                         wal-writing
                         wal-fluff)
  "List of sub-packages that will be loaded.

The order determines the load order as well.")

(defvar wal/booting nil
  "Set to t during bootstrapping.")

(defvar wal/init-error nil
  "Set to the error message if initialization failed.")

(defvar wal/emacs-config-default-path nil
  "The root path of the configuration.

This variable will be set when calling `wal/bootstrap-config'.")

(defvar wal/emacs-config-package-path nil
  "The path to the config's packages.

This variable will be set when calling `wal/bootstrap-config'.")

;;;; Utility:

(defun wal/find-or-create-directory (dir)
  "Find (or create) directory DIR.

Returns the path to the directory or nil (if created)."
  (if (file-directory-p dir)
      dir
    (make-directory dir)))

(defun wal/directory-files (directory)
  "Get all non-dot-directory files in DIRECTORY."
  (nthcdr 2 (directory-files directory t)))

;;;; Entry-points:

(defun wal/tangle-config ()
  "Tangle the config."
  (interactive)
  (require 'org)
  (require 'ob-tangle)
  (defvar org-confirm-babel-evaluate)
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle-file (expand-file-name "README.org" wal/emacs-config-default-path))))

(defun wal/bootstrap-config (source-dir &optional no-load)
  "Bootstrap the configuration in SOURCE-DIR.

This will tangle the config if it hasn't been yet.

Unless NO-LOAD is t, this will load the `wal' package."
  (let* ((package-dir (expand-file-name "wal" source-dir))
         (created-dir (wal/find-or-create-directory package-dir)))

    ;; These variables are also used in `wal' package.
    (setq wal/emacs-config-default-path source-dir)
    (setq wal/emacs-config-package-path package-dir)

    (unless created-dir
      (wal/tangle-config))

    (unless no-load
      (condition-case err
          (progn
            (setq wal/booting t)

            (add-to-list 'load-path wal/emacs-config-package-path)
            (dolist (it wal/packages)
              (require it nil t))

            (setq wal/booting nil))
        (error
         (setq wal/init-error (error-message-string err))
         (delay-warning
          'wal
          (format "Initializing the config failed.\n\nReview the following message:\n\n%s\n\nThen tangle again." wal/init-error)
          :error))))))

(provide 'wal-prelude)

;;; wal-prelude.el ends here
