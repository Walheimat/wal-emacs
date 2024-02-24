;;; wal.el --- Bootstrap the configuration -*- lexical-binding: t -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/wal-emacs
;; Version: 2.2.11
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package bootstraps Walheimat's literate configuration by
;; tangling the libraries and setting up the init file to load the
;; packages.

;;; Code:

(declare-function org-babel-tangle-file "ob-tangle")

;;;; Customization

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

;;;; Public variables

(defvar wal-booting nil
  "Set to t during bootstrapping.")

(defvar wal-loaded nil
  "Set to t after loading.")

(defvar wal-ensure nil
  "Ensure packages after bootstrapping.")

(defvar wal-default-path nil
  "The root path of the configuration.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal-lib-path nil
  "The path to the config's library.

This variable will be set when calling `wal-bootstrap'")

(defvar wal-build-path nil
  "The path to the config's built packages.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal-dinghy-path nil
  "The path to dinghy submodule.

This variable will be set when calling `wal-bootstrap'.")

(defvar wal-error nil
  "Set to the error message if initialization failed.")

;;;; Init bootstrap

(defconst wal-init--marker "wal-prelude-bootstrap")

(defconst wal-init--end-marker "wal-prelude-bootstrap--end")

(defconst wal-init--marker-fs (concat wal-init--marker ":%s")
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
         (marker (format wal-init--marker-fs hashed))
         (template (expand-file-name "data/init.eld" source-dir))
         (template-buffer (find-file-noselect template))
         (template-contents (with-current-buffer template-buffer
                              (buffer-string)))
         (bootstrap (format template-contents marker source-dir wal-init--end-marker))
         (ready nil))

    (with-current-buffer init-buffer
      (if (and (not clear)
               (string-search hashed (buffer-string)))
          (progn
            (message "Bootstrap in '%s' is up-to-date" init-file)
            (setq ready t))
        (when-let* ((start (string-search wal-init--marker (buffer-string)))
                    (end (or (string-search wal-init--end-marker (buffer-string))
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

;;;; Tangling

(defvar wal-tangle--ignore '(message partial-recall--schedule-buffer)
  "Functions that should be advised using `ignore' during tangling.")

(defvar wal-tangle--phony-build-dependencies '(".cask")
  "Files or directories that are phony dependencies.

These files will be touched after tangling.")

(defun wal-tangle-library ()
  "Tangle library files.

This tangles all library files. Whether they are loaded or not
depends on `wal-additional-packages'.

Note that `message' is silenced during tangling."
  (require 'org)
  (require 'ob-tangle)
  (defvar org-confirm-babel-evaluate)

  (message "Tangling files in '%s'" wal-lib-path)

  (let ((org-confirm-babel-evaluate nil)
        (sources (nthcdr 2 (directory-files wal-lib-path t))))

    (dolist (it wal-tangle--ignore)
      (advice-add it :override #'ignore))

    (dolist (it sources)
      (org-babel-tangle-file (expand-file-name it wal-default-path)))

    (dolist (it wal-tangle--ignore)
      (advice-remove it #'ignore))

    (wal-tangle--touch)

    (message "All %d library files in `%s' tangled" (length sources) wal-lib-path)))

(defun wal-tangle--touch ()
  "Touch directories to make sure they aren't considered outdated."
  (dolist (it wal-tangle--phony-build-dependencies)

    (message "Touching `%s'" it)

    (and-let* ((expanded (expand-file-name it wal-default-path))
               ((file-exists-p expanded))
               (output (shell-command-to-string (format "touch %s" expanded))))

      (unless (string-empty-p output)
        (message "Output from touching: %s" output)))))

;;;; Loading

(defvar wal-load--core-packages '(wal-useful
                                  wal-package
                                  wal-key-bindings
                                  wal-bridge
                                  wal-visuals
                                  wal-emacs
                                  wal-config)
  "List of core packages that need to be loaded.

The order determines the load order as well.")

(defvar wal-load--custom-file "custom.el"
  "Name of the custom file.")

(defun wal-load (&optional build-dir)
  "Load the config from BUILD-DIR."
  (interactive)

  (let ((dir (or build-dir
                 wal-build-path
                 default-directory))
        (current nil))

    (setq wal-booting t)

    (wal-load--configure-customization)

    (add-to-list 'load-path dir)

    (let ((all-packages (append wal-load--core-packages wal-additional-packages)))

      (condition-case err
          (dolist (it all-packages)
            (setq current it)
            (require it))
        (error
         (message "Failed to load package '%s': %s"
                  current
                  (error-message-string err))
         (setq wal-error err
               wal-booting nil))
        (:success
         (message "Successfully loaded all %d packages" (length all-packages)))))

    (setq wal-booting nil
          wal-loaded t)))

(defun wal-load--configure-customization ()
  "Configure custom file and load it."
  (let ((file (expand-file-name wal-load--custom-file user-emacs-directory)))

    (if (file-exists-p file)

        (progn
          (message "Loading custom file `%s'" file)
          (load file))

      (message "Didn't find file, setting up `%s'" file)
      (make-empty-file file t))

    (setq custom-file file)))

;;;; Compilation

(defvar wal-compile--buffer nil)
(defvar wal-compile--timer nil)
(defvar wal-compile--process nil)

(defun wal-compile--check (on-completion)
  "Check the status of the compilation.

If the process is no longer live, call ON-COMPLETION with the
process status."
  (when wal-compile--timer

    (let ((process wal-compile--process))

      (unless (process-live-p process)

        (cancel-timer wal-compile--timer)

        (funcall on-completion (process-exit-status process))

        (setq wal-compile--timer nil
              wal-compile--process nil)))))

(defun wal-compile (cmd &optional hidden on-completion)
  "Compile CMD.

If HIDDEN is t, hide the compilation. A handler may be passed as
ON-COMPLETION that will be called with the exit code on
completion."
  (defvar compilation-save-buffers-predicate)

  (let ((default-directory wal-default-path)
        (display-buffer-alist (if hidden
                                  '(("\\*compilation" (display-buffer-no-window)))
                                display-buffer-alist))
        (compilation-save-buffers-predicate #'ignore))

    (let ((buffer (compile cmd)))

      (setq wal-compile--buffer buffer
            wal-compile--process (get-buffer-process buffer))

      (when on-completion
        (setq wal-compile--timer
              (run-with-timer 0 0.2 'wal-compile--check on-completion)))

      buffer)))

;;;; Goals

(defun wal-update ()
  "Update the configuration.

This updates the repository, tangles it and finally upgrades
packages in `wal-bridge'."
  (interactive)

  (message "Updating repository")

  (wal-compile "make update && make upgrade" nil 'wal-update--on-completion))

(defun wal-update--on-completion (exit-status)
  "Handle EXIT-STATUS."
  (when (and (eq 0 exit-status)
             (yes-or-no-p "Upgrade succeeded. Want to restart Emacs?"))
    (restart-emacs)))

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

  (wal-compile "make tangle" t))

;;;; Bootstrapping

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
      (wal-load nil)
      (wal-bootstrap--handle-error exit))))

(defun wal-bootstrap--handle-error (exit-on-error)
  "Handle the error that occurred during initialization.

If EXIT-ON-ERROR is t, exit on error."
  (when wal-error
    (if exit-on-error
        (kill-emacs 1)
      (delay-warning
       'wal
       (format "Initializing the config failed.\n\nReview the following message:\n\n%s\n\nThen tangle again." wal-error)
       :error))))

(defun wal-bootstrap--set-paths (source-dir)
  "Set all paths from SOURCE-DIR."
  (let* ((lib-dir (expand-file-name "lib" source-dir))
         (build-dir (expand-file-name "build" source-dir))
         (dinghy-dir (expand-file-name "dinghy" source-dir)))


    (setq wal-default-path source-dir
          wal-lib-path lib-dir
          wal-build-path build-dir
          wal-dinghy-path dinghy-dir)))

(defun wal-bootstrap--maybe-tangle ()
  "Maybe tangle the configuration."
  (if (and (file-directory-p wal-build-path)
           (not (directory-empty-p wal-build-path)))

      (message "Found non-empty build directory '%s', will not tangle" wal-build-path)

    (make-directory wal-build-path t)

    (wal-tangle-library)))

(defun wal-bootstrap--ensure-dinghy ()
  "Prep dinghy packages.

This installs `dinghy-rope'."
  (let ((src (expand-file-name "src" wal-dinghy-path)))

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

;;;; API

(defun wal-tangle-target ()
  "Get the target file during tangling."
  (let* ((name buffer-file-name)
         (nodir (file-name-nondirectory name))
         (sans (file-name-sans-extension nodir)))

    (expand-file-name (format "%s.el" sans) wal-build-path)))

(defun wal-show-compilation-result ()
  "Pop to the compilation buffer."
  (interactive)

  (unless wal-compile--buffer
    (user-error "No compilation buffer"))

  (pop-to-buffer wal-compile--buffer))

(defun wal-package-files ()
  "Get the package files."
  (let* ((package-files (nthcdr 2 (directory-files wal-build-path t)))
         (el-files (seq-filter
                    (lambda (it)
                      (string-equal (file-name-extension it)
                                    "el"))
                    package-files)))

    el-files))

(provide 'wal)

;;; wal.el ends here
