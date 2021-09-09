;; wal-setup.el --- Install the init file.

;;; Commentary:

;; Load this file using `load-file' then call `wal/setup-init-file' to
;; set up the init file `wal-setup-commit-hooks' to set up commit
;; hooks.

;;; Code:

;;; Buffer

(defvar wal/setup--results-buffer-name "*wal-results*"
  "The name of the results buffer.")

(defvar wal/setup--npm-output-buffer (get-buffer-create "*wal-results: npm*")
  "Output buffer for npm.")

(defvar wal/setup--npx-output-buffer (get-buffer-create "*wal-results: npx*")
  "Output buffer for npx.")

(defun wal/setup--append-results-message (mes)
  "Append MES to the results buffer.

Unless this is the first message, a newline is prepended."
  (with-current-buffer (get-buffer-create wal/setup--results-buffer-name)
    (goto-char (point-max))
    (if (eq (point) (point-min))
        (insert mes)
      (insert (format "\n%s" mes)))))

(defun wal/setup--erase-results-buffer ()
  "Erase the results buffer."
  (with-current-buffer (get-buffer-create wal/setup--results-buffer-name)
    (erase-buffer)))


(defun wal/setup--show-results-buffer ()
  "Show the results buffer."
  (with-current-buffer (get-buffer-create wal/setup--results-buffer-name)
    (wal/setup--results-mode)
    (display-buffer (current-buffer))))

;;; Checks

(defun wal/setup--init-file-exists-p ()
  "Check if an init file already exists."
  (file-exists-p (expand-file-name "init.el" user-emacs-directory)))

(defun wal/setup--dot-emacs-file-exists-p ()
  "Check if a .emacs file already exists."
  (file-exists-p (expand-file-name ".emacs" "~")))

;;; Shell

(defun wal/setup--soft-link-template ()
  "Create a (soft) symbolic link to the template.

The soft link is created in the `user-emacs-directory'."
  (let ((templ (expand-file-name "init.el" "./templates"))
        (targ (expand-file-name "init.el" user-emacs-directory)))
    (shell-command (format "ln -s %s %s" templ targ))))

(defun wal/setup--copy-template ()
  "Create a copy of the template.

The copy is created in the `user-emacs-directory'."
  (let ((templ (expand-file-name "init.el" "./templates"))
        (targ (expand-file-name "init.el" user-emacs-directory)))
    (shell-command (format "cp %s %s" templ targ))))

;;; Mode

(defvar wal/setup--results-mode-map (make-sparse-keymap)
  "Keymap used in `wal/setup--results-mode'.")

(define-key wal/setup--results-mode-map (kbd "q") #'kill-buffer-and-window)

(define-derived-mode wal/setup--results-mode text-mode "Wal Results"
  "Major mode to show setup results.")

(defun wal/setup-init-file ()
  "Set up the init file."
  (interactive)
  (when (or (wal/setup--init-file-exists-p) (wal/setup--dot-emacs-file-exists-p))
    (user-error "Remove existing init file first"))
  (wal/setup--erase-results-buffer)
  (wal/setup--append-results-message "Setup started")
  (let ((method (completing-read "Select setup method: " '(link copy))))
    (wal/setup--append-results-message (format "Selected method: %s" method))
    (cond
     ((string= method "link")
      (wal/setup--soft-link-template)
      (wal/setup--append-results-message "Created symbolic link to template"))
     ((string= method "copy")
      (wal/setup--copy-template)
      (wal/setup--append-results-message "Created copy of template"))
     (nil
      (user-error "Unknown method"))))
  (wal/setup--append-results-message "Setup finished, reload Emacs")
  (wal/setup--show-results-buffer))

(defun wal/setup-commit-hooks ()
  "Set up commit hooks."
  (interactive)
  (unless (and (executable-find "npm") (executable-find "npx"))
    (user-error "Binaries 'npm' and 'npx' need to be in path"))
  (wal/setup--erase-results-buffer)
  (wal/setup--append-results-message "Setting up commit hooks")
  (let ((default-directory wal/emacs-config-default-path)
        (output-buffer (get-buffer-create wal/setup--results-buffer-name)))
    (wal/setup--append-results-message "Installing packages")
    (shell-command "npm install" wal/setup--npm-output-buffer)
    (wal/setup--append-results-message "Installing hooks")
    (shell-command "npx husky install" wal/setup--npx-output-buffer))
  (wal/setup--append-results-message "Commit hooks are set up")
  (wal/setup--show-results-buffer))

(provide 'wal-setup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; wal-setup.el ends here
