;; %s
;;
;; This snippet bootstraps `wal-emacs'.
;;
;; You may freely move and edit it.
;;
;; Note however that on version updates, the snippet will get
;; re-created by deleting the existing snippet and appending a new one
;; to the end of the file.
;;
(let ((base-dir "%s")
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8)
      (file-name-handler-alist nil))

  (add-to-list 'load-path base-dir)
  (require 'wal-prelude)

  (when (fboundp 'wal-prelude-bootstrap)
    (wal-prelude-bootstrap base-dir))) ;; %s
