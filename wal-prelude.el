;;; wal-prelude.el --- Bootstrap the configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; This package bootstraps Walheimat's literate configuration by
;; tangling the libraries and setting up the init file to load the
;; packages.

;;; Code:

;;;; Variables:

(declare-function org-babel-tangle-file "ob-tangle")

(defconst wal-packages '(wal
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
                         wal-web)
  "List of sub-packages that will be loaded.

The order determines the load order as well.")

(defvar wal-booting nil
  "Set to t during bootstrapping.")

(defvar wal-emacs-config-default-path nil
  "The root path of the configuration.

This variable will be set when calling `wal-prelude-bootstrap'.")

(defvar wal-emacs-config-lib-path nil
  "The path to the config's library.

This variable will be set when calling `wal-prelude-bootstrap'")

(defvar wal-emacs-config-package-path nil
  "The path to the config's packages.

This variable will be set when calling `wal-prelude-bootstrap'.")

;;;; Utility:

(defun wal-find-or-create-directory (dir)
  "Find (or create) directory DIR.

Returns the path to the directory or nil (if created)."
  (if (file-directory-p dir)
      dir
    (make-directory dir)))

(defun wal-directory-files (directory)
  "Get all non-dot-directory files in DIRECTORY."
  (nthcdr 2 (directory-files directory t)))

;;;; Init file setup:

(defconst wal-prelude--init-marker ";; wal-prelude-bootstrap"
  "The marker used to insert and delete in the user's init file.")

(defun wal-prelude--ensure-init (init-file source-dir)
  "Ensure that the INIT-FILE knows how to bootstrap.

This verifies the bootstrapping block was created by the current
version. It is otherwise (re-)created.

Files are looked up relative to SOURCE-DIR."
  (unless (file-exists-p init-file)
    (user-error "Init file %s doesn't exist" init-file))

  (let* ((cmd (format "cd %s && git describe --abbrev=0" source-dir))
         (description (string-trim (shell-command-to-string cmd)))
         (hashed (base64-encode-string description))
         (init-buffer (find-file-noselect init-file))
         (marker (concat "\n" wal-prelude--init-marker ":" hashed "\n"))
         (template (expand-file-name "templates/init.eld" source-dir))
         (template-buffer (find-file-noselect template))
         (ready nil))

    (with-current-buffer init-buffer
      (if (string-search hashed (buffer-string))
          (progn
            (setq ready t)
            (message "Bootstrap in '%s' is up-to-date" init-file))
        (when-let* ((start (string-search wal-prelude--init-marker (buffer-string))))
          (message "Deleting existing bootstrap in '%s'" init-file)
          (delete-region start (point-max))
          (save-buffer))))

    (unless ready
      (message "Setting up bootstrap in '%s'" init-file)
      (with-current-buffer template-buffer
        (append-to-file marker nil init-file)
        (append-to-file (buffer-string) nil init-file)))))

(defvar wal-prelude-init-error nil
  "Set to the error message if initialization failed.")

(defun wal-prelude--load-config (&optional package-dir)
  "Load the config from PACKAGE-DIR."
  (interactive)

  (let ((dir (or package-dir
                 wal-emacs-config-package-path
                 default-directory))
        (current nil))

    (setq wal-booting t)

    (add-to-list 'load-path dir)

    (condition-case err
        (dolist (it wal-packages)
          (setq current it)
          (require it))
      (error
       (message "Failed to load package '%s': %s"
                current
                (error-message-string err))
       (setq wal-prelude-init-error err
             wal-booting nil)))

    (setq wal-booting nil)))

;;;; Entry-points:

(defun wal-tangle-config ()
  "Tangle the configuration's libraries.

Note that `message' is silenced during tangling."
  (interactive)

  (require 'org)
  (require 'ob-tangle)
  (defvar org-confirm-babel-evaluate)

  (let ((org-confirm-babel-evaluate nil)
        (sources (wal-directory-files wal-emacs-config-lib-path)))

    (advice-add #'message :override #'ignore)

    (dolist (it sources)
      (org-babel-tangle-file (expand-file-name it wal-emacs-config-default-path)))

    (advice-remove #'message #'ignore)

    (message "All library files tangled")))

(defun wal-prelude-bootstrap (source-dir &optional no-load cold-boot)
  "Bootstrap the configuration in SOURCE-DIR.

This will tangle the config if it hasn't been yet.

Unless NO-LOAD is t, this will load the `wal' package.

If COLD-BOOT is t, a temp folder will be used as a
`package-user-dir' to test the behavior of a cold boot."
  (let* ((package-dir (expand-file-name "wal" source-dir))
         (lib-dir (expand-file-name "lib" source-dir))
         (created-dir (wal-find-or-create-directory package-dir)))

    (message "Boostrapping config from '%s'" source-dir)

    ;; These variables are also used in `wal' package.
    (setq wal-emacs-config-default-path source-dir)
    (setq wal-emacs-config-lib-path lib-dir)
    (setq wal-emacs-config-package-path package-dir)

    (unless created-dir
      (wal-tangle-config))

    (when cold-boot
      (require 'cl-macs)
      (require 'seq)
      (require 'scroll-bar)

      (setq package-user-dir (make-temp-file nil t))

      (message "Cold-boot using '%s'" package-user-dir))

    (unless no-load
      (wal-prelude--load-config)

      (when wal-prelude-init-error
        (if cold-boot
            (kill-emacs 1)
          (delay-warning
           'wal
           (format "Initializing the config failed.\n\nReview the following message:\n\n%s\n\nThen tangle again." wal-prelude-init-error)
           :error))))))

(provide 'wal-prelude)

;;; wal-prelude.el ends here
