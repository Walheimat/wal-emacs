;;; wal-setup-ci.el --- Set up the config in a CI environment.

;;; Commentary:

;; This will tangle the config without loading it.

;;; Code:


(declare-function org-babel-tangle-file "ext:ob-tangle")

(defvar wal/emacs-config-package-path nil)

(defun wal/tangle-config ()
  "Tangle the config in a CI environment."
  (let* ((root-path (getenv "GITHUB_WORKSPACE"))
         (source-file (expand-file-name "README.org" root-path)))
    (setq wal/emacs-config-package-path (expand-file-name "wal" root-path))
    (make-directory wal/emacs-config-package-path)
    (require 'org)
    (require 'ob-tangle)
    (org-babel-tangle-file source-file)))

(wal/tangle-config)

;;; wal-setup-ci.el ends here
