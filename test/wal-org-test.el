;;; wal-org-test.el --- Tests for Org package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-org nil t)

(ert-deftest wal-first-require-ox-md ()
  :tags '(org)

  (bydi ((:ignore featurep)
         require)

    (wal-first-require-ox-md)

    (bydi-was-called-with featurep (list 'ox-md))
    (bydi-was-called-with require (list 'ox-md nil t))))

(ert-deftest wal-org-content ()
  :tags '(org user-facing)

  (bydi org-content

    (wal-org-content 8)

    (bydi-was-called-with org-content (list 8))))

(ert-deftest wal-org-hide-emphasis-makers ()
  :tags '(org user-facing)

  (defvar org-hide-emphasis-markers)

  (let ((org-hide-emphasis-markers nil))

    (bydi (font-lock-flush)

      (wal-org-hide-emphasis-markers)
      (should org-hide-emphasis-markers)

      (wal-org-hide-emphasis-markers t)
      (should-not org-hide-emphasis-markers)

      (bydi-was-called-n-times font-lock-flush 2))))

(ert-deftest wal-org-refile ()
  :tags '(org user-facing)

  (defvar org-roam-directory nil)
  (defvar org-agenda-files nil)

  (let ((org-agenda-files '("/tmp/agenda"))
        (org-roam-directory "/tmp/roam"))

    (bydi ((:always org-roam-buffer-p)
           (:mock org-refile :return org-agenda-files))

      (should (equal '("/tmp/roam") (wal-org-refile)))
      (bydi-was-called org-refile)

      (bydi-clear-mocks)
      (should (equal '("/tmp/agenda") (wal-org-refile 5)))
      (bydi-was-called org-refile))

    (bydi ((:ignore org-roam-buffer-p)
           (:mock org-refile :return org-agenda-files))

      (should (equal '("/tmp/agenda") (wal-org-refile)))
      (bydi-was-called org-refile))))

(ert-deftest wal-agenda-buffer-p ()
  :tags '(org)

  (with-temp-buffer
    (bydi ((:always org-agenda-file-p))
      (should (wal-agenda-buffer-p (current-buffer))))))

(ert-deftest wal-consult-agenda-buffer--query ()
  :tags '(org)

  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-agenda-buffer--query) '(visibility buffer-name wal-agenda-buffer-p))))

(ert-deftest wal-relative-column-width ()
  :tags '(org)

  (defvar text-scale-mode-amount nil)
  (defvar text-scale-mode-step nil)
  (let ((text-scale-mode-amount 1)
        (text-scale-mode-step 2))

    (should (eq 80 (wal-relative-column-width)))))

(ert-deftest wal-org-tree-slide-toggle-visibility ()
  :tags '(org user-facing)

  (let ((cursor-type 'box))

    (wal-org-tree-slide-toggle-visibility)

    (should-not cursor-type)

    (wal-org-tree-slide-toggle-visibility)

    (should cursor-type)))

(ert-deftest wal-org-tree-slide ()
  :tags '(org)

  (defvar visual-fill-column-width nil)
  (defvar visual-fill-column-center-text nil)
  (bydi (visual-fill-column-mode
         mixed-pitch-mode
         (:mock wal-relative-column-width :with bydi-rf)
         outline-show-all
         text-scale-adjust
         wal-org-hide-emphasis-markers)

    (wal-org-tree-slide-play)

    (should (eq visual-fill-column-width 160))
    (should visual-fill-column-center-text)
    (should-not cursor-type)
    (bydi-was-called mixed-pitch-mode)
    (bydi-was-called visual-fill-column-mode)
    (bydi-was-called wal-org-hide-emphasis-markers)

    (bydi-clear-mocks)

    (wal-org-tree-slide-stop)

    (should-not visual-fill-column-width)
    (should-not visual-fill-column-center-text)
    (should cursor-type)

    (bydi-was-called outline-show-all)
    (bydi-was-called-with mixed-pitch-mode -1)
    (bydi-was-called-with visual-fill-column-mode -1)
    (bydi-was-called-with text-scale-adjust 0)
    (bydi-was-called-with wal-org-hide-emphasis-markers t)))

(ert-deftest wal-org-tree-slide-text-scale ()
  :tags '(org)

  (defvar org-tree-slide-mode)
  (let ((org-tree-slide-mode t))

    (bydi wal-org-tree-slide-play

      (wal-org-tree-slide-text-scale)

      (bydi-was-called wal-org-tree-slide-play))))

(ert-deftest wal-org-capture-find-project-task-heading ()
  :tags '(org)

  (defvar org-directory)

  (let ((marker nil)
        (existing-file nil)
        (org-directory "/tmp"))

    (when (get-buffer "*wal-async*")
      (kill-buffer "*wal-async*"))

    (bydi ((:mock org-find-exact-heading-in-directory :return marker)
           (:mock org-find-exact-headline-in-buffer :return marker)
           switch-to-buffer
           (:mock wal-project-local-value :with (lambda (it &optional _)
                                                  (pcase it
                                                    ('wal-org-capture-tasks-heading marker)
                                                    ('wal-org-capture-tasks-file existing-file)
                                                    ('wal-project-parent-project nil)
                                                    (_ nil))))
           (:always project-current)
           (:always project-root))

      (should-error (wal-org-capture-locate-project-tasks) :type 'user-error)

      (with-temp-buffer
        (insert "Test")
        (goto-char (point-max))

        (setq marker (point-marker))

        (goto-char (point-min))

        (wal-org-capture-locate-project-tasks)

        (should (eq (marker-position marker) 5)))

      (ert-with-temp-file file
        (setq existing-file file)

        (with-current-buffer (find-file-noselect file)
          (insert "Testing")
          (goto-char (point-max))

          (setq marker (point-marker)))

        (wal-org-capture-locate-project-tasks)

        (should (eq (marker-position marker) 8))

        (setq existing-file (file-name-nondirectory file))

        (with-current-buffer (find-file-noselect file)
          (insert "?")
          (goto-char (point-max))

          (setq marker (point-marker)))

        (wal-org-capture-locate-project-tasks)

        (should (eq (marker-position marker) 9))))))

(ert-deftest wal-org-capture-project-tasks ()
  :tags '(org user-facing)

  (bydi org-capture
    (wal-org-capture-project-tasks)

    (bydi-was-called-with org-capture '(nil "p"))))

(ert-deftest wal-org-clock-in-switch-to-state ()
  :tags '(org)

  (should (string-equal "IN PROGRESS" (wal-org-clock-in-switch-to-state "OTHER STATE"))))

(ert-deftest wal-org-clock-heading ()
  :tags '(org)

  (bydi ((:mock org-link-display-format :with bydi-rf)
         (:mock org-get-heading :return "test heading")
         (:mock org-no-properties :with bydi-rf)
         wal-truncate)

    (wal-org-clock-heading)

    (bydi-was-called-with wal-truncate (list "test heading" 12))))

(ert-deftest wal-org-clock-in-from-now ()
  :tags '(org)

  (bydi org-clock-in

    (wal-org-clock-in-from-now)

    (bydi-was-called org-clock-in)))

(ert-deftest wal-org-clock-take-note ()
  :tags '(org user-facing)

  (bydi (org-clock-goto
         org-add-note)

    (wal-org-clock-take-note)

    (bydi-was-called org-clock-goto)
    (bydi-was-called org-add-note)))

(ert-deftest wal-org-clock-insert-current-task ()
  :tags '(org user-facing)

  (defvar org-clock-current-task)

  (with-temp-buffer
    (let ((org-clock-current-task nil)
          (kill-ring nil))

      (should-error (wal-org-clock-kill-current-task))
      (should (string= "" (buffer-string)))

      (setq org-clock-current-task (propertize "Testing" :face 'success))

      (shut-up
        (wal-org-clock-kill-current-task)
        (yank))

      (should (string= "Testing" (buffer-string))))))

(ert-deftest wal-org-agenda-take-note ()
  (bydi (consult-org-agenda
         org-add-note)

    (wal-org-agenda-take-note)

    (bydi-was-called consult-org-agenda)
    (bydi-was-called org-add-note)))

(ert-deftest wal-org-clock--first-record-clocking-buffer ()
  (bydi ((:watch wal-org-clock--unsaved)
         (:mock org-clocking-buffer :return 'buffer))

    (wal-org-clock--first-record-clocking-buffer)

    (bydi-was-set-to wal-org-clock--unsaved 'buffer)))

(ert-deftest wal-org-clock--then-save-unsaved-buffer ()
  (let ((wal-org-clock--unsaved (current-buffer))
        (org-log-note-this-command 'org-clock-out))

    (bydi ((:spy save-buffer)
           (:watch wal-org-clock--unsaved))
      (wal-org-clock--then-save-unsaved-buffer)

      (bydi-was-called save-buffer)
      (bydi-was-set-to wal-org-clock--unsaved nil))))

;;; wal-org-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
