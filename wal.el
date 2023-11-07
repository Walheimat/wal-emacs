;;; wal.el --- Bootstrap the configuration -*- lexical-binding: t -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-emacs
;; Version: 2.1.14
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This package bootstraps Walheimat's literate configuration by
;; tangling the libraries and setting up the init file to load the
;; packages.

;;; Code:

(declare-function org-babel-tangle-file "ob-tangle")

(defvar wal-packages '(wal-useful
                       wal-package
                       wal-key-bindings
                       wal-bridge
                       ;; The following packages are optional.
                       wal-settings
                       wal-config
                       wal-emacs
                       wal-edit
                       wal-visuals
                       wal-movement
                       wal-find
                       wal-complete
                       wal-workspace
                       wal-windows
                       wal-org
                       wal-dired
                       wal-terminal
                       wal-vc
                       wal-lang
                       wal-fix
                       wal-lsp
                       wal-devops
                       wal-web)
  "List of sub-packages that will be loaded.

The order determines the load order as well.")

(defvar wal-booting nil
  "Set to t during bootstrapping.")

(defvar wal-loaded nil
  "Set to t after loading.")

(defvar wal-ensure nil
  "Ensure packages after bootstrapping.")

(defvar wal-emacs-config-default-path nil
  "The root path of the configuration.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal-emacs-config-lib-path nil
  "The path to the config's library.

This variable will be set when calling `wal-bootstrap'")

(defvar wal-emacs-config-build-path nil
  "The path to the config's built packages.

This variable will be set when calling `wal-bootstrap'.")

(defgroup wal nil
  "Walheimat's configuration."
  :group 'convenience
  :prefix "wal-")

(defun wal-package-files ()
  "Get the package files."
  (let* ((package-files (nthcdr 2 (directory-files wal-emacs-config-build-path t)))
         (el-files (seq-filter
                    (lambda (it)
                      (string-equal (file-name-extension it)
                                    "el"))
                    package-files)))

    el-files))

(defconst wal--init-marker "wal-prelude-bootstrap")
(defconst wal--init-end-marker "wal-prelude-bootstrap--end")
(defconst wal--init-marker-fs (concat wal--init-marker ":%s")
  "String to format new markers.")

(defun wal-init (init-file source-dir &optional clear)
  "Ensure that the INIT-FILE knows how to bootstrap.

This verifies the bootstrapping block was created by the current
version. It is otherwise (re-)created.

Files are looked up relative to SOURCE-DIR.

If CLEAR is t, make sure the INIT-FILE no longer knows."
  (unless (file-exists-p init-file)
    (user-error "Init file '%s' doesn't exist" init-file))

  (let* ((cmd (format "cd %s && git describe --abbrev=0" source-dir))
         (description (string-trim (shell-command-to-string cmd)))
         (hashed (base64-encode-string description))
         (init-buffer (find-file-noselect init-file))
         (marker (format wal--init-marker-fs hashed))
         (template (expand-file-name "data/init.eld" source-dir))
         (template-buffer (find-file-noselect template))
         (template-contents (with-current-buffer template-buffer
                              (buffer-string)))
         (bootstrap (format template-contents marker source-dir wal--init-end-marker))
         (ready nil))

    (with-current-buffer init-buffer
      (if (and (not clear)
               (string-search hashed (buffer-string)))
          (progn
            (message "Bootstrap in '%s' is up-to-date" init-file)
            (setq ready t))
        (when-let* ((start (string-search wal--init-marker (buffer-string)))
                    (end (or (string-search wal--init-end-marker (buffer-string))
                             (point-max))))
          (message "Deleting existing bootstrap in '%s'" init-file)

          (save-excursion
            (goto-char start)
            (beginning-of-line)
            (setq start (point))
            (goto-char end)
            (end-of-line)
            (setq end (point)))

          (delete-region start end)
          (save-buffer))))
    (kill-buffer init-buffer)

    (unless (or ready clear)
      (message "Setting up bootstrap in '%s'" init-file)
      (append-to-file bootstrap nil init-file))))

(defvar wal-init-error nil
  "Set to the error message if initialization failed.")

(defun wal--configure-customization ()
  "Configure custom file and load it."
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (message "Setting up custom file '%s" custom-file)

  (unless (file-exists-p custom-file)
    (make-empty-file custom-file t))

  (when (file-exists-p custom-file)
    (load custom-file)))

(defun wal--load-config (&optional build-dir)
  "Load the config from BUILD-DIR."
  (interactive)

  (let ((dir (or build-dir
                 wal-emacs-config-build-path
                 default-directory))
        (current nil))

    (setq wal-booting t)

    (wal--configure-customization)

    (add-to-list 'load-path dir)

    (condition-case err
        (dolist (it wal-packages)
          (setq current it)
          (require it))
      (error
       (message "Failed to load package '%s': %s"
                current
                (error-message-string err))
       (setq wal-init-error err
             wal-booting nil)))

    (setq wal-booting nil
          wal-loaded t)))

(defun wal--set-paths (source-dir)
  "Set all directories based on SOURCE-DIR."
   (let* ((lib-dir (expand-file-name "lib" source-dir))
          (build-dir (expand-file-name "build" source-dir)))

     ;; These variables are also used in `wal' package.
     (setq wal-emacs-config-default-path source-dir
           wal-emacs-config-lib-path lib-dir
           wal-emacs-config-build-path build-dir)))

(defun wal--configure-cold-boot ()
  "Configure cold-booting."
  (require 'cl-lib)
  (require 'seq)
  (require 'scroll-bar)

  (let ((temp-dir (make-temp-file nil t)))

    (setq package-user-dir temp-dir)
    (message "Cold-boot using '%s'" temp-dir)))

(defconst wal--phony-build-dependencies '(".cask")
  "Files or directories that are phony dependencies.

These files will be touched after tangling.")

(defun wal--touch ()
  "Touch directories to make sure they aren't considered outdated."
  (dolist (it wal--phony-build-dependencies)

    (let ((expanded (expand-file-name it wal-emacs-config-default-path)))

      (when (file-exists-p expanded)
        (shell-command (format "touch %s" expanded))))))

(defvar wal--ignore-during-tangle '(message partial-recall--schedule-buffer)
  "Functions that should be advised using `ignore' during tangling.")

(defun wal--tangle-target ()
  "Get the target file during tangling."
  (let* ((name buffer-file-name)
         (nodir (file-name-nondirectory name))
         (sans (file-name-sans-extension nodir)))

    (expand-file-name (format "%s.el" sans) wal-emacs-config-build-path)))

(defun wal-tangle-config ()
  "Tangle the configuration's libraries.

Note that `message' is silenced during tangling."
  (interactive)

  (require 'org)
  (require 'ob-tangle)
  (defvar org-confirm-babel-evaluate)

  (message "Tangling files in '%s'" wal-emacs-config-lib-path)

  (let ((org-confirm-babel-evaluate nil)
        (sources (nthcdr 2 (directory-files wal-emacs-config-lib-path t))))

    (dolist (it wal--ignore-during-tangle)
      (advice-add it :override #'ignore))

    (dolist (it sources)
      (org-babel-tangle-file (expand-file-name it wal-emacs-config-default-path)))

    (dolist (it wal--ignore-during-tangle)
      (advice-remove it #'ignore))

    (wal--touch)

    (message "All library files in '%s' tangled" wal-emacs-config-lib-path)))

(defun wal--handle-error (exit)
  "Handle the error that occurred during initialization.

If EXIT is t, exit on error."
  (when wal-init-error
    (if exit
        (kill-emacs 1)
      (delay-warning
       'wal
       (format "Initializing the config failed.\n\nReview the following message:\n\n%s\n\nThen tangle again." wal-init-error)
       :error))))

(defun wal--maybe-tangle ()
  "Maybe tangle the configuration."
  (if (and (file-directory-p wal-emacs-config-build-path)
           (not (directory-empty-p wal-emacs-config-build-path)))
      (message "Found non-empty build directory '%s', will not tangle" wal-emacs-config-build-path)
    (make-directory wal-emacs-config-build-path t)
    (wal-tangle-config)))

(defun wal-bootstrap (source-dir &optional mode)
  "Bootstrap the configuration in SOURCE-DIR.

This will tangle the config if it hasn't been yet.

Optional MODE can be one of symbols `plain', `cold' and `ensure'.

Plain means the configuration will not be loaded.

Cold means a temp folder will be used as `package-user-dir' to
test the behavior of a cold boot.

Ensure means that packages will be installed after loading."
  (let ((mode (or mode 'normal))
        (load t)
        (exit nil))

    (message "Bootstrapping config from '%s' in '%s' mode" source-dir mode)

    (wal--set-paths source-dir)
    (wal--maybe-tangle)

    (pcase mode
      ('ensure
       (package-initialize)
       (setq wal-ensure t))

      ('cold
       (wal--configure-cold-boot)
       (setq exit t))

      ('plain
       (setq load nil)))

    (when load
      (wal--load-config)
      (wal--handle-error exit))))

(provide 'wal)

;;; wal.el ends here
