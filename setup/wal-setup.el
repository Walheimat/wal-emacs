;; wal-setup.el --- Install the init file.

;;; Commentary:

;; Load this file using `load-file' and pick a setup.
;; hooks.

;;; Code:

(require 'shell nil t)

;; Internal

(defvar wal/setup--results-buffer-name "*wal-results*"
  "The name of the results buffer.")

(defun wal/setup--get-config-path ()
  "Get the path to the config."
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun wal/setup--show-results-buffer ()
  "Show the results buffer.
Optionally ERASE-FIRST."
  (with-current-buffer (get-buffer-create "*wal-results*")
    (erase-buffer)
    (display-buffer-in-side-window (current-buffer) '((side . bottom)))))

(defun wal/setup--executables-exist-p (&optional optional)
  "Check if (OPTIONAL) executables needed for setting up commits exist."
  (let ((execs (if optional '("bash" "npm" "npx") '("bash"))))
    (eval `(and ,@(mapcar #'executable-find execs)))))

(defun wal/setup--async-run-script (script &optional args)
  "Run SCRIPT asynchronously using results buffer as output (and ARGS)."
  (let ((default-directory (expand-file-name "setup" (wal/setup--get-config-path)))
        (scr (string-trim (concat "./" script ".sh" " " (string-join args " ")))))
    (async-shell-command scr (get-buffer-create wal/setup--results-buffer-name))))

;; Setup functions

(defun wal/setup-init-file ()
  "Set up the init file."
  (unless (wal/setup--executables-exist-p)
    (user-error "Setup script requires bash to be in path"))
  (let ((method (completing-read "Select setup method: " '(link copy))))
    (wal/setup--show-results-buffer)
    (wal/setup--async-run-script "setup-init" (list method user-emacs-directory))))

(defun wal/setup-commit-hooks ()
  "Set up commit hooks."
  (unless (wal/setup--executables-exist-p t)
    (user-error "Setup script requires bash, npm and npx to be in path"))
  (wal/setup--show-results-buffer)
  (wal/setup--async-run-script "setup-commits"))

(defun wal/setup ()
  "Run a setup function."
  (let ((setup-name (completing-read
                     "What do you want to set up? "
                     '("init file" "commit hooks"))))
    (cond
     ((string= setup-name "init file")
      (wal/setup-init-file))
     ((string= setup-name "commit hooks")
      (wal/setup-commit-hooks))
     (t
      (user-error "No setup of that name exists")))))

(wal/setup)

;;; wal-setup.el ends here
