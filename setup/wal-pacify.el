;;; wal-pacify.el --- Set up checking package files.

;;; Commentary:

;; This will just check the packages.

;;; Code:

(require 'wal-prelude (expand-file-name
                       "setup/wal-prelude.el"
                       (getenv "EMACS_SOURCE_DIR"))
         t)

(let ((source-dir (getenv "EMACS_SOURCE_DIR")))

  (when (fboundp 'wal/bootstrap-config)
    (wal/bootstrap-config source-dir t)))

(defvar wal/emacs-config-package-path)
(declare-function wal/package-files "ext:wal.el")
(declare-function wal/directory-files "ext:wal-prelude.el")

(add-to-list 'load-path wal/emacs-config-package-path)

(require 'wal nil t)

(package-initialize)
(require 'flycheck)

;;; Functionality:

(defvar wal-pacify-errors nil)

(defun wal-pacify--callback ()
  "Exit with errors."
  (lambda (_status &optional errors)
    (when errors
      (seq-do
       (lambda (err)
         (when-let ((file (flycheck-error-filename err))
                    (line (flycheck-error-line err))
                    (col (flycheck-error-column err))
                    (mess (flycheck-error-message err)))

           (add-to-list 'wal-pacify-errors (format "%s %d:%d %s\n" file line col mess))))
       errors))))

(defun wal-pacify--check-file (file)
  "Check FILE with flycheck."
  (with-current-buffer (find-file-noselect file)
    (let ((checker (flycheck-get-checker-for-buffer)))

      (let ((check (flycheck-syntax-check-new
                    :buffer (current-buffer)
                    :checker checker
                    :context nil
                    :working-directory (flycheck-compute-working-directory checker)))
            (callback (wal-pacify--callback)))

        (flycheck-syntax-check-start check callback)))))

(message "Checking package files")

(dolist (it (wal/package-files))
  (wal-pacify--check-file it))

(sit-for 5)

(flycheck-safe-delete-temporaries)

(when wal-pacify-errors
  (message "Check failed!")

  (dolist (it wal-pacify-errors)
    (message it))

  (kill-emacs 1))

;;; wal-pacify.el ends here
