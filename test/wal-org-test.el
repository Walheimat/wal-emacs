;;; wal-org-test.el --- Tests for Org package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-org nil t)

(ert-deftest test-wal/first-require-ox-md ()
  (with-mock ((featurep . #'ignore)
              require)

    (wal/first-require-ox-md)

    (was-called-with featurep (list 'ox-md))
    (was-called-with require (list 'ox-md nil t))))

(ert-deftest test-wal/org-content ()
  (with-mock org-content

    (wal/org-content 8)

    (was-called-with org-content (list 8))))

(ert-deftest test-wal/org-refile ()
  (defvar org-roam-directory)
  (defvar org-agenda-files)

  (let ((org-agenda-files '("/tmp/agenda"))
        (org-roam-directory "/tmp/roam"))

    (with-mock ((org-roam-buffer-p . #'always)
                (org-refile . (lambda (&rest _) org-agenda-files)))

      (should (equal '("/tmp/roam") (wal/org-refile)))
      (was-called org-refile)

      (wal/clear-mocks)
      (should (equal '("/tmp/agenda") (wal/org-refile 5)))
      (was-called org-refile))

    (with-mock ((org-roam-buffer-p . #'ignore)
                (org-refile . (lambda (&rest _) org-agenda-files)))

      (should (equal '("/tmp/agenda") (wal/org-refile)))
      (was-called org-refile))))

(ert-deftest test-wal/agenda-buffer-p ()
  (defvar org-agenda-contributing-files)
  (wal/with-temp-file "test-agenda.org"
    (let ((org-agenda-contributing-files `(,wal/tmp-file))
          (buf (find-file-noselect wal/tmp-file)))

      (should (wal/agenda-buffer-p buf)))))

(ert-deftest test-wal/consult-agenda-buffer--query ()
  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal/consult-agenda-buffer--query) '(visibility buffer-name wal/agenda-buffer-p))))

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
  (with-mock (visual-fill-column-mode
              mixed-pitch-mode
              (wal/relative-column-width . #'wal/rf)
              outline-show-all
              text-scale-adjust)

    (wal/org-tree-slide-play)

    (should (eq visual-fill-column-width 160))
    (should visual-fill-column-center-text)
    (should-not cursor-type)
    (was-called mixed-pitch-mode)
    (was-called visual-fill-column-mode)

    (wal/clear-mocks)

    (wal/org-tree-slide-stop)

    (should-not visual-fill-column-width)
    (should-not visual-fill-column-center-text)
    (should cursor-type)

    (was-called outline-show-all)
    (was-called-with mixed-pitch-mode -1)
    (was-called-with visual-fill-column-mode -1)
    (was-called-with text-scale-adjust 0)))

(ert-deftest test-wal/org-tree-slide-text-scale ()
  (defvar org-tree-slide-mode)
  (let ((org-tree-slide-mode t))

    (with-mock wal/org-tree-slide-play

      (wal/org-tree-slide-text-scale)

      (was-called wal/org-tree-slide-play))))

(ert-deftest test-wal/org-capture-find-project-task-heading ()
  (let ((heading nil))

    (with-mock ((org-find-exact-heading-in-directory . (lambda (&rest _) heading))
                set-buffer
                switch-to-buffer
                goto-char
                (project-current . #'always)
                (project-root . #'always)
                (marker-buffer . (lambda (_) 'buffer))
                (marker-position . (lambda (_) 'position)))

      (should-error (wal/org-capture-locate-project-tasks) :type 'user-error)
      (should-error (wal/org-capture-switch-to-project-tasks) :type 'user-error)

      (setq heading 'heading)

      (wal/org-capture-locate-project-tasks)
      (was-called-with goto-char (list 'position))


      (wal/org-capture-switch-to-project-tasks)
      (was-called-with switch-to-buffer (list 'buffer)))))

(ert-deftest test-wal/org-clock-in-switch-to-state ()
  (should (string-equal "IN PROGRESS" (wal/org-clock-in-switch-to-state "OTHER STATE"))))

(ert-deftest test-wal/org-clock-heading ()
  (with-mock ((org-link-display-format . #'wal/rf)
              (org-get-heading . (lambda (&rest _r) "test heading"))
              (org-no-properties . #'wal/rf)
              wal/truncate)

    (wal/org-clock-heading)

    (was-called-with wal/truncate (list "test heading" 12))))

(ert-deftest test-wal/org-clock-in-from-now ()
  (with-mock org-clock-in

    (wal/org-clock-in-from-now)

    (was-called org-clock-in)))

(ert-deftest test-wal/org-clock-take-note ()
  (with-mock (org-clock-goto
              org-add-note)

    (wal/org-clock-take-note)

    (was-called org-clock-goto)
    (was-called org-add-note)))

;;; wal-org-test.el ends here
