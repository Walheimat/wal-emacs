;;; .emacs --- the init file

;;; Commentary:

;; Simplified init file using `org-babel' to tangle
;; source blocks from literate config.

;;; Code:

(declare-function org-babel-tangle-file "ob-tangle")

(defvar wal/emacs-config-default-path
  (expand-file-name "emacs-config" user-emacs-directory)
  "The default path to Walheimat's Emacs org config.")

(defun wal/tangle-config (&optional maybe load)
  "(MAYBE) tangle the config (and LOAD it).

The default `init.el' will call this to only tangle
src blocks if that hasn't already happened.

If called interactively by the user, this will just
tangle the blocks without loading the created file."
  (interactive)
  (let ((untangled (expand-file-name "README.org" wal/emacs-config-default-path))
        (tangled (expand-file-name "README.el" wal/emacs-config-default-path)))
    (unless (and maybe (file-exists-p tangled))
      (require 'org)
      (require 'ob-tangle)
      (org-babel-tangle-file untangled tangled))
    (when load
      (load-file tangled))))

;; Uncomment to test start-up time.
;; (setq use-package-minimum-reported-time 0.05
;;       use-package-verbose t
;;       use-package-compute-statistics t)

;; Turns on native-compile and log warnings silently.
(setq package-native-compile t
      native-comp-async-report-warnings-errors 'silent)

;; Disable `org-roam' v2 warning.
(setq org-roam-2-ack t)

;; This will tangle source blocks on first load and
;; afterwards just load the tangled file `README.el'.
(let ((gc-cons-threshold most-positive-fixnum)
	  (gc-cons-percentage 0.8)
      (file-name-handler-alist nil))
  ;; Maybe tangle config and then load the file.
  (wal/tangle-config t t))

;; Just an example: Setting a theme.
(setq wal/theme 'doom-dracula)

;;; .emacs ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
