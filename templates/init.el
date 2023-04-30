;;; init.el --- Walheimat's init file

;;; Commentary:

;; Either copy this file to any location Emacs will load from (cf.
;; 49.4 The Emacs Initialization File in the manual) or load the
;; included setup file to do this for you.

;;; Code:

(require 'wal-prelude (expand-file-name
                       "emacs-config/setup/wal-prelude.el"
                       user-emacs-directory))

;; Maybe tangle, load.
(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8)
      (file-name-handler-alist nil)
      (source-dir (expand-file-name "emacs-config" user-emacs-directory)))
  (wal-bootstrap-config source-dir))

;;; init.el ends here
