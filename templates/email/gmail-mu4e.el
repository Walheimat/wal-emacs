;;; gmail-mu4e.el --- Template for setting up `mu4e' with `smtpmail' for Gmail.

;;; Commentary:

;; This file is a minimal Gmail setup. Be sure to replace the folders
;; and the email address. You will then need to copy the file to any
;; folder in the `load-path' using name `site-start.el'.

;;; Code:

;; Rename folders.
(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder "/[Gmail].Sent"
      mu4e-trash-folder "/[Gmail].Trash"
      mu4e-maildir-shortcuts
      '((:maildir "/INBOX" :key ?i)
        (:maildir "/[Gmail].All" :key ?a)
        (:maildir "/[Gmail].Trash" :key ?t :hide t)))

;; Replace with your user email.
(setq smtpmail-smtp-user "email@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; gmail-mu4e.el ends here.

;; Local Variables:
;; no-byte-compile: t
;; End:
