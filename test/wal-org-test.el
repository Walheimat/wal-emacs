;;; wal-org-test.el --- Tests for Org package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-org nil t)

(ert-deftest test-wal/first-require-ox-md ()
  (with-mock-all ((featurep . #'ignore)
                  (require . #'wal/rf))
    (should (equal 'ox-md (wal/first-require-ox-md)))))

(ert-deftest test-wal/org-content ()
  (with-rf-mock org-content
    (should (equal 8 (wal/org-content 8)))))

(ert-deftest test-wal/then-find-tasks-and-store-window-configuration ()
  (defvar org-agenda-files)
  (with-mock-all ((display-buffer . #'wal/ra)
                  (window-configuration-to-register . #'wal/rf))
    (let ((org-agenda-files '("~"))
          (wal/org-agenda-register-char ?t))
      (should (equal ?t (wal/then-find-tasks-and-store-window-configuration))))))

(ert-deftest test-wal/maybe-org-roam-refile ()
  (with-mock-all ((org-roam-buffer-p . #'always)
                  (wal/univ-p . #'ignore)
                  (org-roam-refile . (lambda () 'roam))
                  (org-refile . (lambda (&rest _r) 'org)))
    (should (equal 'roam (wal/maybe-org-roam-refile))))
  (with-mock-all ((org-roam-buffer-p . #'ignore)
                  (org-refile . (lambda (&rest _r) 'org)))
    (should (equal 'org (wal/maybe-org-roam-refile)))))

(ert-deftest test-wal/relative-column-width ()
  (defvar text-scale-mode-amount nil)
  (defvar text-scale-mode-step nil)
  (let ((text-scale-mode-amount 1)
        (text-scale-mode-step 2))
    (should (eq 80 (wal/relative-column-width)))))

(ert-deftest test-wal/org-tree-slide-toggle-visibility ()
  (let ((cursor-type 'box))
    (wal/org-tree-slide-toggle-visibility)
    (should-not cursor-type)
    (wal/org-tree-slide-toggle-visibility)
    (should cursor-type)))

(ert-deftest test-wal/org-tree-slide ()
  (defvar visual-fill-column-width nil)
  (defvar visual-fill-column-center-text nil)
  (let ((out nil))
    (with-mock-all ((visual-fill-column-mode . (lambda (_) (add-to-list 'out 'visual)))
                    (mixed-pitch-mode . (lambda (_) (push 'mixed out)))
                    (wal/relative-column-width . #'wal/rf)
                    (outline-show-all . (lambda () (push 'outline out))))
      (wal/org-tree-slide-play)
      (should (eq visual-fill-column-width 160))
      (should visual-fill-column-center-text)
      (should-not cursor-type)
      (should (equal '(mixed visual) out))
      (setq out nil)
      (wal/org-tree-slide-stop)
      (should-not visual-fill-column-width)
      (should-not visual-fill-column-center-text)
      (should cursor-type)
      (should (equal '(mixed outline visual) out)))))

(ert-deftest test-wal/org-tree-slide-text-scale ()
  (defvar org-tree-slide-mode)
  (let ((org-tree-slide-mode t))
    (with-mock-all ((wal/org-tree-slide-play . #'wal/rt))
      (should (equal 'testing (wal/org-tree-slide-text-scale))))))

(ert-deftest test-wal/org-capture-find-project-tasks ()
  (defvar wal/use-projectile)
  (with-mock-all ((projectile-project-root . (lambda () "/tmp"))
                  (project-current . #'always)
                  (project-root . (lambda (_) "/tmp")))
    (let ((wal/use-projectile t))
      (should (string-equal "/tmp/tasks.org" (wal/org-capture-find-project-tasks))))
    (let ((wal/use-projectile nil))
      (should (string-equal "/tmp/tasks.org" (wal/org-capture-find-project-tasks)))))
  (with-mock-all ((project-root . #'ignore)
                  (project-current . #'always))
    (defvar org-directory)
    (let ((org-directory "/org")
          (wal/use-projectile nil))
      (should (string-equal "/org/tasks.org" (wal/org-capture-find-project-tasks))))))

(ert-deftest test-wal/org-clock-in-switch-to-state ()
  (should (string-equal "IN PROGRESS" (wal/org-clock-in-switch-to-state "OTHER STATE"))))

(ert-deftest test-wal/org-clock-heading ()
  (with-mock-all ((org-link-display-format . #'wal/rf)
                  (org-get-heading . (lambda (&rest _r) "test heading"))
                  (org-no-properties . #'wal/rf)
                  (wal/truncate . (lambda (h &optional _) (substring h 0 4))))
    (should (string-equal "test" (wal/org-clock-heading)))))

(ert-deftest test-wal/org-clock-in-from-now ()
  (with-mock org-clock-in #'wal/rt
    (should (equal 'testing (wal/org-clock-in-from-now)))))

(ert-deftest test-wal/org-clock-take-note ()
  (let ((out nil))
    (with-mock-all ((org-clock-goto . (lambda () (add-to-list 'out 'goto)))
                    (org-add-note . (lambda () (add-to-list 'out 'note))))
      (wal/org-clock-take-note)
      (should (equal '(note goto) out)))))

;;; wal-org-test.el ends here
