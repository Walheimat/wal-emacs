;;; wal-workspace-test.el --- Tests for workspace package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-workspace nil t)

(ert-deftest test-wal/maybe-use-projectile ()
  (match-expansion
   (wal/maybe-use-projectile find-file)
   `(defun wal/maybe-use-projectile-find-file ()
      "Use `projectile-find-file' instead of `find-file' if in a project."
      (interactive)
      (with-demoted-errors "%s"
        (require 'projectile)
        (if (projectile-project-p)
            (call-interactively (symbol-function 'projectile-find-file))
          (call-interactively (symbol-function 'find-file)))))))

(ert-deftest test-wal/with-projectile-switch ()
  (with-mock-all ((projectile-project-p . #'always))
    (let ((func (lambda (&rest _r) 'projectile)))
      (should (equal 'projectile (wal/with-projectile-switch func nil)))))
  (with-mock-all ((projectile-project-p . #'ignore)
                  (call-interactively . #'wal/ra))
    (should (equal '(projectile-switch-project) (wal/with-projectile-switch nil nil)))))

;;; wal-workspace-test.el ends here
