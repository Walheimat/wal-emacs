;;; .emacs --- the init file

;;; Commentary:

;; Simplified init file using `org-babel' to tangle
;; source blocks from literate config.
;;
;; Copy this file to your HOME directory.

;;; Code:

(declare-function org-babel-tangle-file "ob-tangle")

(defvar wal/emacs-config-default-path
  (expand-file-name "emacs-config" user-emacs-directory)
  "The default path to Walheimat's Emacs org config.")

(defvar wal/emacs-config-package-path
  (expand-file-name "wal" wal/emacs-config-default-path)
  "The path to the tangled Lisp files.")

(defun wal/find-or-create-package-directory ()
  "Find (or create) package directory.
Returns the path to the directory or nil (if created)."
  (if (file-directory-p wal/emacs-config-package-path)
      wal/emacs-config-package-path
    (make-directory wal/emacs-config-package-path)))

(defun wal/directory-files (directory)
  "Get all directory files in DIRECTORY except for current and parent directories."
  (nthcdr 2 (directory-files directory t)))

(defun wal/tangle-config (&optional maybe load)
  "(MAYBE) tangle the config (and LOAD it).

The init file will call this function to tangle source
blocks only if that hasn't already happened and then
load the package.

If called interactively this will tangle the blocks
without loading the package."
  (interactive)
  (let ((source-file (expand-file-name "README.org" wal/emacs-config-default-path))
        (found-target-dir (wal/find-or-create-package-directory)))
    (unless (and maybe found-target-dir)
      (require 'org)
      (require 'ob-tangle)
      (org-babel-tangle-file source-file))
    (when load
      (load-file (expand-file-name "wal.el" (wal/find-or-create-package-directory))))))

;; Uncomment to test start-up time.
;; (setq use-package-minimum-reported-time 0.05
;;       use-package-verbose t
;;       use-package-compute-statistics t)

;; Turns on native-compile and log warnings silently.
(setq package-native-compile t
      native-comp-async-report-warnings-errors 'silent)

;; Disable `org-roam' v2 warning.
(setq org-roam-2-ack t)

;; This will tangle all source blocks on first load and
;; afterwards load the package.
(let ((gc-cons-threshold most-positive-fixnum)
	  (gc-cons-percentage 0.8)
      (file-name-handler-alist nil))
  ;; Maybe tangle config and then load the package.
  (wal/tangle-config t t))

;; Customization should start here:
;; (setq wal/theme 'doom-dracula)

;;; .emacs ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
