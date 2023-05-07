;;; wal-pacify.el --- Check files with `flymake'. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Checks Lisp files for warnings and errors.

;;; Code:

(declare-function wal-prelude-package-files "ext:wal-prelude.el")

(require 'warnings)
(require 'flymake)

(defun wp--get-state (state)
  "Get the STATE."
  (when (hash-table-p flymake--state)
    (gethash state flymake--state)))

(defun wp--ready-p ()
  "Check if the report concluded."
  (when-let ((b-state (wp--get-state 'elisp-flymake-byte-compile))
             (d-state (wp--get-state 'elisp-flymake-checkdoc)))

    (and (flymake--state-reported-p b-state)
         (flymake--state-reported-p d-state))))

(defun wp--get-diags ()
  "Get all all state dialogs."
  (append (flymake--state-diags (wp--get-state 'elisp-flymake-byte-compile))
          (flymake--state-diags (wp--get-state 'elisp-flymake-checkdoc))))

(defun wp--get-severity-diags (severity)
  "Get all dialogs of SEVERITY."
  (cl-loop for diag in (wp--get-diags)
           if (eq (flymake--severity (flymake-diagnostic-type diag)) severity)
           collect (wp--get-info diag)))

(defvar wp--infos nil)
(defvar wp--debugs nil)
(defvar wp--warnings nil)
(defvar wp--errors nil)

(defvar wp--info 0)
(defvar wp--debug (warning-numeric-level :debug))
(defvar wp--warning (warning-numeric-level :warning))
(defvar wp--error (warning-numeric-level :error))

(defun wp--get-info (diag)
  "Get the relevant info from DIAG."
  (let ((text (flymake--diag-text diag))
        (beg (flymake--diag-beg diag))
        (locus (flymake--diag-locus diag)))

    (list :file (buffer-file-name locus) :line (line-number-at-pos beg) :text text)))

(defconst wp--checkers '(elisp-flymake-byte-compile elisp-flymake-checkdoc))

(defun wp--collect (file)
  "Collect reports for FILE."
  (let ((counter 0))

    (with-current-buffer (find-file-noselect file)
      (setq sentence-end-double-space nil)
      (setq flymake-diagnostic-functions wp--checkers)

      (flymake-mode)
      (flymake-start nil)

      (while (and (not (wp--ready-p)) (< counter 100))
        (setq counter (1+ counter))
        (sit-for 0.05))

      (unless (wp--ready-p)
        (error "Waited for five seconds for check to complete"))

      (let ((warnings (wp--get-severity-diags wp--warning))
            (errors (wp--get-severity-diags wp--error))
            (debugs (wp--get-severity-diags wp--debug))
            (infos (wp--get-severity-diags wp--info)))

        (setq wp--warnings (append wp--warnings warnings)
              wp--errors (append wp--errors errors)
              wp--debugs (append wp--debugs debugs)
              wp--infos (append wp--infos infos))))))

(defun wp--format (info)
  "Format INFO for output."
  (let ((file (plist-get info :file))
        (line (plist-get info :line))
        (text (plist-get info :text)))

    (format "%s:%s: %s" file line text)))

(defun wp-check--get-package-files ()
  "Get all testable package files."
  (seq-filter
   (lambda (it) (not (string-match "movement\\|fix\\|settings" it)))
   (wal-prelude-package-files)))

(defun wp-check ()
  "Check all package files."
  (message "Checking package files with `flymake'")

  (condition-case err
      (dolist (it (wp-check--get-package-files))
        (wp--collect it))
    (err
     (message "Failed to check all of the packages: %s" (error-message-string err))
     (kill-emacs 0)))

  (let* ((severe (append wp--errors wp--warnings))
         (other (append wp--debugs wp--infos)))

    (when other
      (message "Found %s note(s)" (length other))
      (dolist (it other)
        (message (wp--format it))))

    (when severe
      (message "Found %s error(s)" (length severe))

      (dolist (it severe)
        (message (wp--format it)))
      (kill-emacs 1))))

(provide 'wal-pacify)

;;; wal-pacify.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("wp-" . "wal-pacify-"))
;; End:
