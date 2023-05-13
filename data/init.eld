;;
;; Do not add custom code below this line. It will get deleted when the bootstrap
;; is re-created.
(let ((base-dir "%s")
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8)
      (file-name-handler-alist nil))

  (add-to-list 'load-path base-dir)
  (require 'wal-prelude)

  (when (fboundp 'wal-prelude-bootstrap)
    (wal-prelude-bootstrap base-dir)))
