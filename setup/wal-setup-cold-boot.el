;;; wal-setup-cold-boot.el --- Set up the config in a cold environment.

;;; Commentary:

;; This will tangle the config and load it like it was a cold boot by
;; setting the `package-user-dir' to a temporary directory.

;;; Code:

(require 'cl-macs)
(require 'seq)
(require 'scroll-bar)
(require 'wal-prelude (expand-file-name
                       "setup/wal-prelude.el"
                       (getenv "EMACS_SOURCE_DIR")) t)

(let ((source-dir (getenv "EMACS_SOURCE_DIR")))

  (when (fboundp 'wal/bootstrap-config)
    (wal/bootstrap-config source-dir nil t)))

;;; wal-setup-cold-boot.el ends here
