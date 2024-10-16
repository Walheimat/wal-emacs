;;; wal-org-test.el --- Tests for Org package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-org nil t)

(ert-deftest wal-first-require-ox-md ()
  :tags '(org)

  (bydi ((:always require))

    (wal-first-require-ox-md)

    (bydi-was-called-with require (list 'ox-md nil t))))

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

(ert-deftest wal-org-refile--maybe-use-default-directory ()
  :tags '(org user-facing)

  (defvar org-roam-directory nil)
  (defvar org-agenda-files nil)

  (let ((org-agenda-files '("/tmp/agenda"))
        (org-roam-directory "/tmp/roam"))

    (bydi ((:watch org-agenda-files)
           org-refile)

      (wal-org-refile--maybe-use-default-directory)
      (bydi-was-not-set org-agenda-files)
      (bydi-was-not-called org-refile)

      (wal-org-refile--maybe-use-default-directory '(5))
      (bydi-was-set-to org-agenda-files (list default-directory))
      (bydi-was-called org-refile))))

(ert-deftest wal-agenda-buffer-p ()
  :tags '(org)

  (with-temp-buffer
    (bydi ((:always org-agenda-file-p))
      (should (wal-agenda-buffer-p (current-buffer))))))

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
           (:ignore project-prompt-project-dir)
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

        (wal-org-capture-locate-project-tasks t)

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

(ert-deftest wal-org-clock-in-and-out-switch-to-state ()
  :tags '(org)

  (defvar org-todo-keywords-1)
  (defvar org-clock-current-task)

  (let ((wal-org-clock-in-progress-state "IN PROGRESS")
        (org-todo-keywords-1 '("BAITING" "WORK" "WAITING")))

    (should-not (wal-org-clock-in-switch-to-state "WORK")))

  (let ((wal-org-clock-in-progress-state "IN PROGRESS")
        (org-todo-keywords-1 '("BAITING" "IN PROGRESS" "WAITING")))

    (should (string-equal "IN PROGRESS" (wal-org-clock-in-switch-to-state "UNKNOWN STATE")))
    (should-not (wal-org-clock-in-switch-to-state nil))
    (should (string-equal "IN PROGRESS" (wal-org-clock-in-switch-to-state "BAITING")))

    (bydi ((:mock completing-read :return "WAITING"))
      (let ((org-clock-current-task nil))

        (should (string-equal "WAITING" (wal-org-clock-out-switch-to-state "OTHER STATE")))
        (should (string-equal "WAITING" (wal-org-clock-out-switch-to-state "BAITING")))))))

(ert-deftest wal-org-clock-heading ()
  :tags '(org)

  (bydi ((:mock org-link-display-format :with bydi-rf)
         (:mock org-get-heading :return "test heading")
         (:mock org-no-properties :with bydi-rf)
         truncate-string-to-width)

    (wal-org-clock-heading)

    (bydi-was-called-with truncate-string-to-width (list "test heading" 12 0 nil t))))

(ert-deftest wal-org-clock-in-from-now ()
  :tags '(org)

  (bydi org-clock-in

    (wal-org-clock-in-from-now)

    (bydi-was-called org-clock-in)))

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

(ert-deftest wal-org-agenda--then-rename-tab ()
  :tags '(org)

  (defvar org-agenda-window-setup nil)

  (let ((org-agenda-window-setup 'something-else))

    (bydi (tab-bar-rename-tab)

      (wal-org-agenda--then-rename-tab)

      (bydi-was-not-called tab-bar-rename-tab)

      (setq org-agenda-window-setup 'other-tab)

      (wal-org-agenda--then-rename-tab)

      (bydi-was-called-with tab-bar-rename-tab "agenda"))))

(ert-deftest wal-org-super-agenda--with-groups ()
  :tags '(org)

  (defvar org-super-agenda-groups nil)

  (bydi ((:spy bydi-rf)
         (:watch org-super-agenda-groups))

    (wal-org-super-agenda--with-groups #'bydi-rf 'test)

    (bydi-was-set org-super-agenda-groups)
    (bydi-was-called bydi-rf)))

(ert-deftest wal-org-roam-dailies--with-first-template-only ()
  (defvar org-roam-dailies-capture-templates)
  (let ((org-roam-dailies-capture-templates '(a b c)))

    (bydi ((:watch org-roam-dailies-capture-templates)
           (:spy ignore))

      (wal-org-roam-dailies--with-first-template-only #'ignore 'test)

      (bydi-was-called-with ignore '((test)))

      (bydi-was-set-to org-roam-dailies-capture-templates '(a)))))

(ert-deftest wal-org-archive-subtree ()
  :tags '(org)

  (defvar org-roam-directory nil)
  (defvar org-archive-location nil)
  (defvar org-roam-dailies-directory nil)

  (let ((org-roam-dailies-directory "/test/zettel/dailies")
        (org-roam-directory "/test/zettel/default")
        (org-archive-location "archive.org"))

    (bydi ((:mock buffer-file-name :var mock-file-name :initial "/test/zettel/dailies/is.org")
           (:watch org-archive-location)
           org-archive-subtree)

      (wal-org-archive-subtree)

      (bydi-was-called org-archive-subtree)
      (bydi-was-set-to org-archive-location "/test/zettel/default/dailies_archive.org::* From %s" :clear t)

      (setq mock-file-name "/test/other/test.org")

      (wal-org-archive-subtree)

      (bydi-was-set-to org-archive-location "archive.org"))))

(ert-deftest wal-org--first-record-buffer ()
  :tags '(org)

  (bydi ((:watch wal-org--unsaved))

    (ert-with-test-buffer (:name "logging")
      (defvar org-log-note-marker)

      (let ((org-log-note-marker (point-marker)))

        (wal-org--first-record-buffer)

        (bydi-was-set-to wal-org--unsaved (current-buffer))))))

(ert-deftest wal-org--then-save-unsaved-buffer ()
  (let ((wal-org--unsaved (current-buffer)))

    (bydi ((:spy save-buffer)
           (:watch wal-org--unsaved))

      (wal-org--then-save-unsaved-buffer)

      (bydi-was-called save-buffer)
      (bydi-was-set-to wal-org--unsaved nil))))

;;; wal-org-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
