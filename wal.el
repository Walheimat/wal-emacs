;;; wal.el --- Bootstrap the configuration -*- lexical-binding: t -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-emacs
;; Version: 2.2.7
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package bootstraps Walheimat's literate configuration by
;; tangling the libraries and setting up the init file to load the
;; packages.

;;; Code:

(declare-function org-babel-tangle-file "ob-tangle")

;;; -- Customization

(defgroup wal nil
  "Walheimat's configuration."
  :group 'convenience
  :prefix "wal-")

(defcustom wal-additional-packages '(wal-settings
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
                                     wal-lang
                                     wal-fix
                                     wal-lsp
                                     wal-devops
                                     wal-web)
  "Additional packages of the configuration.

These are optional packages that can be left out. If a minimal
configuration is preferred, you can optionally set `wal-minimal'
or run Emacs with flag `--minimal'."
  :group 'wal
  :type '(repeat symbol))

;;; -- Variables

(defvar wal-packages '(wal-useful
                       wal-package
                       wal-key-bindings
                       wal-bridge
                       wal-visuals
                       wal-emacs
                       wal-config)
  "List of sub-packages that will be loaded.

The order determines the load order as well.")

(defvar wal-booting nil
  "Set to t during bootstrapping.")

(defvar wal-loaded nil
  "Set to t after loading.")

(defvar wal-ensure nil
  "Ensure packages after bootstrapping.")

(defvar wal--default-path nil
  "The root path of the configuration.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal--lib-path nil
  "The path to the config's library.

This variable will be set when calling `wal-bootstrap'")

(defvar wal--build-path nil
  "The path to the config's built packages.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal--dinghy-path nil
  "The path to dinghy submodule.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal--custom-file "custom.el"
  "Name of the custom file.")

(defconst wal--phony-build-dependencies '(".cask")
  "Files or directories that are phony dependencies.

These files will be touched after tangling.")

(defvar wal-init-error nil
  "Set to the error message if initialization failed.")

(defconst wal--init-marker "wal-prelude-bootstrap")

(defconst wal--init-end-marker "wal-prelude-bootstrap--end")

(defconst wal--init-marker-fs (concat wal--init-marker ":%s")
  "String to format new markers.")

(defvar wal--compile-buffer nil)

(defvar wal--ignore-during-tangle '(message partial-recall--schedule-buffer)
  "Functions that should be advised using `ignore' during tangling.")

;;; -- Creating bootstrapper

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

;;; -- Tangling

(defun wal-tangle-library ()
  "Tangle library files.

This tangles all library files. Whether they are loaded or not
depends on `wal-additional-packages'.

Note that `message' is silenced during tangling."
  (require 'org)
  (require 'ob-tangle)
  (defvar org-confirm-babel-evaluate)

  (message "Tangling files in '%s'" wal--lib-path)

  (let ((org-confirm-babel-evaluate nil)
        (sources (nthcdr 2 (directory-files wal--lib-path t))))

    (dolist (it wal--ignore-during-tangle)
      (advice-add it :override #'ignore))

    (dolist (it sources)
      (org-babel-tangle-file (expand-file-name it wal--default-path)))

    (dolist (it wal--ignore-during-tangle)
      (advice-remove it #'ignore))

    (wal--touch)

    (message "All %d library files in `%s' tangled" (length sources) wal--lib-path)))

(defun wal--touch ()
  "Touch directories to make sure they aren't considered outdated."
  (dolist (it wal--phony-build-dependencies)

    (message "Touching `%s'" it)

    (and-let* ((expanded (expand-file-name it wal--default-path))
               ((file-exists-p expanded))
               (output (shell-command-to-string (format "touch %s" expanded))))

      (unless (string-empty-p output)
        (message "Output from touching: %s" output)))))

;;; -- Loading

(defun wal-load (&optional build-dir)
  "Load the config from BUILD-DIR."
  (interactive)

  (let ((dir (or build-dir
                 wal--build-path
                 default-directory))
        (current nil))

    (setq wal-booting t)

    (wal-load--configure-customization)

    (add-to-list 'load-path dir)

    (let ((all-packages (append wal-packages wal-additional-packages)))

      (condition-case err
          (dolist (it all-packages)
            (setq current it)
            (require it))
        (error
         (message "Failed to load package '%s': %s"
                  current
                  (error-message-string err))
         (setq wal-init-error err
               wal-booting nil))
        (:success
         (message "Successfully loaded all %d packages" (length all-packages)))))

    (setq wal-booting nil
          wal-loaded t)))

(defun wal-load--configure-customization ()
  "Configure custom file and load it."
  (let ((file (expand-file-name wal--custom-file user-emacs-directory)))

    (if (file-exists-p file)

        (progn
          (message "Loading custom file `%s'" file)
          (load file))

      (message "Didn't find file, setting up `%s'" file)
      (make-empty-file file t))

    (setq custom-file file)))

;;; Helpers

(defun wal--compile (cmd &optional comint)
  "Compile CMD.

Optionally use `comint-mode' if COMINT is t."
  (defvar compilation-save-buffers-predicate)

  (let ((default-directory wal--default-path)
        (display-buffer-alist '(("\\*compilation" (display-buffer-no-window))))
        (compilation-save-buffers-predicate #'ignore))

    (setq wal--compile-buffer (compile cmd comint))))

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

;;; -- Goals

(defun wal-update ()
  "Update the configuration.

This updates the repository, tangles it and finally upgrades
packages in `wal-bridge'."
  (interactive)

  (message "Updating repository")

  (wal--compile "make update && make upgrade"))

(defvar wal-upgrade--wait-time 1)

(defun wal-upgrade ()
  "Upgrade bridge packages.

This waits for a maximum of 5 seconds to upgrade all packages."
  (defvar package-alist)
  (declare-function package-vc-p "ext:package.el")

  (let* ((package-count 0)
         (counter 0)
         (after-execution (lambda (&rest _args)
                            (setq counter (1+ counter))))
         (repeats 0)
         (max-repeats 5))

    (dolist (package package-alist)
      (dolist (pkg-desc (cdr package))
        (when (package-vc-p pkg-desc)
          (setq package-count (1+ package-count)))))

    (add-hook 'vc-post-command-functions after-execution)

    (package-vc-upgrade-all)

    (while (and (not (= counter package-count))
                (not (> repeats max-repeats)))

      (setq repeats (1+ repeats))

      (sit-for wal-upgrade--wait-time))

    (remove-hook 'vc-post-command-functions after-execution)

    (if (< repeats max-repeats)
        (message "Upgraded packages in %d seconds" repeats)
      (message "Upgrades did not finish, waited for a maximum of %d seconds" max-repeats))))

(defun wal-tangle ()
  "Tangle the config in a separate process."
  (interactive)

  (message "Tangling configuration")

  (wal--compile "make tangle"))

;;; -- Bootstrapping

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

    (wal-bootstrap--set-paths source-dir)

    (wal-bootstrap--maybe-tangle)

    (pcase mode
      ('ensure
       (package-initialize)
       (wal-bootstrap--ensure-dinghy)
       (setq wal-ensure t))

      ('upgrade
       (package-initialize))

      ('cold
       (wal-bootstrap--configure-cold-boot)
       (setq exit t))

      ('plain
       (setq load nil)))

    (when load
      (wal-load)
      (wal--handle-error exit))))

(defun wal-bootstrap--set-paths (source-dir)
  "Set all paths from SOURCE-DIR."
  (let* ((lib-dir (expand-file-name "lib" source-dir))
         (build-dir (expand-file-name "build" source-dir))
         (dinghy-dir (expand-file-name "dinghy" source-dir)))


    (setq wal--default-path source-dir
          wal--lib-path lib-dir
          wal--build-path build-dir
          wal--dinghy-path dinghy-dir)))

(defun wal-bootstrap--maybe-tangle ()
  "Maybe tangle the configuration."
  (if (and (file-directory-p wal--build-path)
           (not (directory-empty-p wal--build-path)))

      (message "Found non-empty build directory '%s', will not tangle" wal--build-path)

    (make-directory wal--build-path t)

    (wal-tangle-library)))

(defun wal-bootstrap--ensure-dinghy ()
  "Prep dinghy packages.

This installs `dinghy-rope'."
  (let ((src (expand-file-name "src" wal--dinghy-path)))

    (when (file-exists-p src)
      (package-install-file (expand-file-name "dinghy-rope.el" src)))))

(defun wal-bootstrap--configure-cold-boot ()
  "Configure cold-booting."
  (require 'cl-lib)
  (require 'seq)
  (require 'scroll-bar)

  (let ((temp-dir (make-temp-file nil t)))

    (setq package-user-dir temp-dir)
    (message "Cold-boot using '%s'" temp-dir)))

;;; -- API

(defun wal-tangle-target ()
  "Get the target file during tangling."
  (let* ((name buffer-file-name)
         (nodir (file-name-nondirectory name))
         (sans (file-name-sans-extension nodir)))

    (expand-file-name (format "%s.el" sans) wal--build-path)))

(defun wal-show-compilation-result ()
  "Pop to the compilation buffer."
  (interactive)

  (unless wal--compile-buffer
    (user-error "No compilation buffer"))

  (pop-to-buffer wal--compile-buffer))

(defun wal-package-files ()
  "Get the package files."
  (let* ((package-files (nthcdr 2 (directory-files wal--build-path t)))
         (el-files (seq-filter
                    (lambda (it)
                      (string-equal (file-name-extension it)
                                    "el"))
                    package-files)))

    el-files))

(provide 'wal)

;;; wal.el ends here
