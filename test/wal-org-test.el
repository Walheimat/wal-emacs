;;; wal-org-test.el --- Tests for Org package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'wal-org nil t)

(ert-deftest wal-first-require-ox-md ()
  (bydi ((:ignore featurep)
         require)

    (wal-first-require-ox-md)

    (bydi-was-called-with featurep (list 'ox-md))
    (bydi-was-called-with require (list 'ox-md nil t))))

(ert-deftest wal-org-content ()
  (bydi org-content

    (wal-org-content 8)

    (bydi-was-called-with org-content (list 8))))

(ert-deftest wal-org-hide-emphasis-makers ()
  (defvar org-hide-emphasis-markers)

  (let ((org-hide-emphasis-markers nil))

    (bydi (font-lock-flush)

      (wal-org-hide-emphasis-markers)
      (should org-hide-emphasis-markers)

      (wal-org-hide-emphasis-markers t)
      (should-not org-hide-emphasis-markers)

      (bydi-was-called-n-times font-lock-flush 2))))

(ert-deftest wal-org-refile ()
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
  (with-temp-buffer
    (bydi ((:always org-agenda-file-p))
      (should (wal-agenda-buffer-p (current-buffer))))))

(ert-deftest wal-agenda--non-agenda-buffer-p ()
  (with-temp-buffer
    (bydi ((:always org-agenda-file-p))
      (should-not (wal-agenda--non-agenda-buffer-p (current-buffer))))))

(ert-deftest wal-consult-agenda-buffer--query ()
  (cl-defun consult--buffer-query (&key sort as predicate)
    (list sort as predicate))

  (should (equal (wal-consult-agenda-buffer--query) '(visibility buffer-name wal-agenda-buffer-p))))

(ert-deftest wal-relative-column-width ()
  (defvar text-scale-mode-amount nil)
  (defvar text-scale-mode-step nil)
  (let ((text-scale-mode-amount 1)
        (text-scale-mode-step 2))

    (should (eq 80 (wal-relative-column-width)))))

(ert-deftest wal-org-tree-slide-toggle-visibility ()
  (let ((cursor-type 'box))

    (wal-org-tree-slide-toggle-visibility)

    (should-not cursor-type)

    (wal-org-tree-slide-toggle-visibility)

    (should cursor-type)))

(ert-deftest wal-org-tree-slide ()
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
  (defvar org-tree-slide-mode)
  (let ((org-tree-slide-mode t))

    (bydi wal-org-tree-slide-play

      (wal-org-tree-slide-text-scale)

      (bydi-was-called wal-org-tree-slide-play))))

(ert-deftest wal-org-capture-find-project-task-heading ()
  (let ((marker nil))

    (when (get-buffer "*wal-async*")
      (kill-buffer "*wal-async*"))

    (bydi ((:mock org-find-exact-heading-in-directory :return marker)
           (:mock org-find-exact-headline-in-buffer :return marker)
           switch-to-buffer
           (:mock wal-project-local-value :with (lambda (it &optional _)
                                                  (pcase it
                                                    ('wal-org-capture-tasks-heading marker)
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
        (setq wal-org-capture-tasks-file file)

        (with-current-buffer (find-file-noselect file)
          (insert "Testing")
          (goto-char (point-max))

          (setq marker (point-marker)))

        (wal-org-capture-locate-project-tasks)

        (should (eq (marker-position marker) 8))))))

(ert-deftest wal-org-clock-in-switch-to-state ()
  (should (string-equal "IN PROGRESS" (wal-org-clock-in-switch-to-state "OTHER STATE"))))

(ert-deftest wal-org-clock-heading ()
  (bydi ((:mock org-link-display-format :with bydi-rf)
         (:mock org-get-heading :return "test heading")
         (:mock org-no-properties :with bydi-rf)
         wal-truncate)

    (wal-org-clock-heading)

    (bydi-was-called-with wal-truncate (list "test heading" 12))))

(ert-deftest wal-org-clock-in-from-now ()
  (bydi org-clock-in

    (wal-org-clock-in-from-now)

    (bydi-was-called org-clock-in)))

(ert-deftest wal-org-clock-take-note ()
  (bydi (org-clock-goto
         org-add-note)

    (wal-org-clock-take-note)

    (bydi-was-called org-clock-goto)
    (bydi-was-called org-add-note)))

(ert-deftest wal-org-clock-insert-current-task ()
  (defvar org-clock-current-task)

  (with-temp-buffer
    (let ((org-clock-current-task nil)
          (kill-ring nil))

      (should-error (wal-org-clock-kill-current-task))
      (should (string= "" (buffer-string)))

      (setq org-clock-current-task (propertize "Testing" :face 'success))

      (wal-org-clock-kill-current-task)
      (yank)

      (should (string= "Testing" (buffer-string))))))

;;; wal-org-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
