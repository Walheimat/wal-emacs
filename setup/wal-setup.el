;; wal-setup.el --- Install the init file.

;;; Commentary:

;; Load this file using `load-file', then run `wal/setup'.

;;; Code:

(require 'shell)
(require 'find-func)

;;;; Variables:

(defvar wal/setup--buffer-name "*wal-results*")
(defvar wal/setup--dependencies '("bash"))
(defvar wal/setup--optional-dependencies '("npm" "npx"))
(defvar wal/setup--command-fmt "./%s.sh %s")
(defvar wal/setup--scripts '(("init file" . wal/setup-init-file)
                             ("commit hooks" . wal/setup-commit-hooks)
                             ("daemon" . wal/setup-daemon)))

;;;; Utility:

(defun wal/setup--locate-config-root()
  "Locate the config root."
  (locate-dominating-file (find-library-name "wal-setup") "README.org"))

(defun wal/setup--relative-file (file)
  "Locate FILE relative to setup root."
  (expand-file-name file (wal/setup--locate-config-root)))

(defun wal/setup--executables-exist-p (&optional optional)
  "Check if (OPTIONAL) executables needed for setting up commits exist."
  (let ((execs (if optional
                   (append wal/setup--optional-dependencies wal/setup--dependencies)
                 wal/setup--dependencies)))
    (eval `(and ,@(mapcar #'executable-find execs)))))

(defun wal/setup--get-buffer ()
  "Get the results buffer."
  (get-buffer-create wal/setup--buffer-name))

(defun wal/setup--show-results ()
  "Show the results buffer."
  (with-current-buffer (wal/setup--get-buffer)
    (erase-buffer)
    (display-buffer-in-side-window (current-buffer) '((side . bottom)))))

(defun wal/setup--command (name args)
  "Return the command for the script NAME with ARGS."
  (format wal/setup--command-fmt name (string-join args " ")))

(defun wal/setup--async-run-script (script &optional args)
  "Run SCRIPT, passing ARGS, asynchronously."
  (let ((default-directory (wal/setup--relative-file "setup")))
    (async-shell-command
     (wal/setup--command script args)
     (wal/setup--get-buffer))))

;; Setup functions:

(defun wal/setup-init-file ()
  "Set up the init file using METHOD."
  (interactive)
  (unless (wal/setup--executables-exist-p)
    (user-error "Setup script requires bash to be in path"))
  (let ((method (completing-read "Select setup method: " '(link copy))))
    (wal/setup--show-results)
    (wal/setup--async-run-script "setup-init" (list method user-emacs-directory))))

(defun wal/setup-commit-hooks ()
  "Set up commit hooks."
  (interactive)
  (unless (wal/setup--executables-exist-p t)
    (user-error "Setup script requires bash, npm and npx to be in path"))
  (wal/setup--show-results)
  (wal/setup--async-run-script "setup-commits"))

(defun wal/setup-daemon ()
  "Setup Emacs daemon."
  (interactive)
  (unless (wal/setup--executables-exist-p)
    (user-error "Setup script requires bash to be in path"))
  (wal/setup--show-results)
  (wal/setup--async-run-script "setup-daemon"))

(defun wal/setup (script)
  "Run a setup SCRIPT."
  (interactive (list (completing-read
                      "What do you want to set up? "
                      wal/setup--scripts)))
  (let ((func (cdr (assoc script wal/setup--scripts))))
    (if func
        (funcall func)
      (user-error "No setup of that name exists"))))

(provide 'wal-setup)

;;; wal-setup.el ends here
