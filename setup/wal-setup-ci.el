;;; wal-setup-ci.el --- Set up the config in a CI environment.

;;; Commentary:

;; This will tangle the config without loading it.

;;; Code:

(require 'wal-prelude (expand-file-name
                       "setup/wal-prelude.el"
                       (getenv "GITHUB_WORKSPACE")) t)

(let ((source-dir (getenv "GITHUB_WORKSPACE")))

  (when (fboundp 'wal/bootstrap-config)
    (wal/bootstrap-config source-dir t)))

;;; wal-setup-ci.el ends here
