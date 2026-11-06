;;; task-workflow-tests.el --- Personal task behavior -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'task-workflow)
(require 'consult)

(defmacro task-workflow-test-with-root (&rest body)
  "Run BODY with isolated task files, Org settings, and ID locations."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "task-workflow-test-" t))
          (task-workflow-personal-root root)
          (task-workflow-focused-id nil)
          (task-workflow-modeline-string nil)
          (org-mode-hook nil)
          (org-agenda-files nil)
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-locations-file (expand-file-name "id-locations" root))
          (org-id-files nil)
          (org-id-extra-files nil)
          (org-capture-templates '(("z" "Existing" entry (file "elsewhere.org") "* TODO %?")))
          (org-todo-keywords '((sequence "TODO" "NEXT" "TEST" "WAIT" "|" "DONE" "CANCELLED")))
          (org-log-done nil)
          (org-log-into-drawer nil)
          (org-clock-out-when-done nil)
          (org-element-use-cache nil)
          (make-backup-files nil)
          (auto-save-default nil))
     (save-window-excursion
       (unwind-protect
           (progn (task-workflow-configure) ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (file-in-directory-p (buffer-file-name buffer) root))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (delete-directory root t)))))

(defun task-workflow-test-file (name text)
  "Write TEXT to NAME in the isolated task directory and return the path."
  (let ((file (expand-file-name name (task-workflow--directory))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

(defun task-workflow-test-heading (state id title &optional extra)
  "Make a marked task heading with STATE, ID, TITLE, and optional EXTRA text."
  (format "* %s %s\n:PROPERTIES:\n:TASK: t\n:ID: %s\n:END:\n%s\n"
          state title id (or extra "")))

(ert-deftest task-workflow-capture-persists-identity-without-focusing ()
  (task-workflow-test-with-root
    (let ((task-workflow-focused-id "previous-focus")
          (templates (copy-tree org-capture-templates))
          (keywords (copy-tree org-todo-keywords))
          (clock (and (boundp 'org-clock-marker) org-clock-marker)))
      (should-not (file-exists-p (task-workflow--directory)))
      (let* ((id (task-workflow-capture-manual "Investigate suspend failures"))
             (file buffer-file-name))
        (should (equal (org-entry-get nil "ID") id))
        (should (equal (org-get-todo-state) "INBOX"))
        (should (equal (org-get-heading t t t t) "Investigate suspend failures"))
        (should (equal (task-workflow--id-at-point) id))
        (should-not (buffer-modified-p))
        (should (equal task-workflow-focused-id "previous-focus"))
        (kill-buffer (current-buffer))
        (let ((record (task-workflow--find id)))
          (should (equal (plist-get record :file) file))
          (should (equal (plist-get record :title) "Investigate suspend failures"))
          (should (equal (plist-get record :state) "INBOX"))))
      (should (equal org-capture-templates templates))
      (with-temp-buffer
        (org-mode)
        (should (equal org-todo-keywords keywords)))
      (should (eq (and (boundp 'org-clock-marker) org-clock-marker) clock)))))

(ert-deftest task-workflow-lifecycle-filter-excludes-finished-and-unrelated-headings ()
  (task-workflow-test-with-root
    (task-workflow-test-file
     "tasks.org"
     (concat
      (mapconcat (lambda (state) (task-workflow-test-heading state state state))
                 '("INBOX" "TODO" "NEXT" "TEST" "WAIT" "PAUSED" "DONE" "CANCELLED") "\n")
      (task-workflow-test-heading "TODO" "archived" "Archived :ARCHIVE:")
      "* TODO Unrelated heading\n:PROPERTIES:\n:ID: unrelated\n:END:\n"
      "** TODO Unmarked child\n"))
    (should (equal (mapcar (lambda (record) (plist-get record :id))
                          (task-workflow--collect))
                   '("INBOX" "TODO" "NEXT" "TEST" "WAIT" "PAUSED")))
    (should (equal (plist-get (task-workflow--find "DONE") :state) "DONE"))
    (should (equal (plist-get (task-workflow--find "CANCELLED") :state) "CANCELLED"))))

(ert-deftest task-workflow-local-lifecycle-does-not-change-unrelated-org ()
  (task-workflow-test-with-root
    (let ((outside (expand-file-name "unrelated.org" root)))
      (with-temp-file outside (insert "* TODO Ordinary work\n* PAUSED Not a global state\n"))
      (task-workflow-capture-manual "Local task")
      (should (member "PAUSED" org-todo-keywords-1))
      (with-current-buffer (find-file-noselect outside)
        (should-not (member "PAUSED" org-todo-keywords-1))
        (goto-char (point-min))
        (should (equal (org-get-todo-state) "TODO")))
      (should (equal (mapcar (lambda (record) (plist-get record :title))
                            (task-workflow--collect))
                     '("Local task"))))))

(ert-deftest task-workflow-focus-follows-renames-and-refiling-by-id ()
  (task-workflow-test-with-root
    (let ((id (task-workflow-capture-manual "Original title")))
      (org-end-of-subtree t t)
      (insert "** Working notes\nBody\n")
      (task-workflow-focus)
      (should (equal task-workflow-focused-id id))
      (goto-char (point-min))
      (re-search-forward "^\\* INBOX Original title$")
      (replace-match "* INBOX Renamed title")
      (save-buffer)
      (let ((destination (expand-file-name "refiled.org" (task-workflow--directory))))
        (rename-file buffer-file-name destination)
        (set-visited-file-name destination t)
        (save-buffer))
      (switch-to-buffer (get-buffer-create " *task-test-navigation*"))
      (unwind-protect
          (progn
            (task-workflow-visit-focused)
            (should (equal (org-entry-get nil "ID") id))
            (should (equal (org-get-heading t t t t) "Renamed title"))
            (should (equal (file-name-nondirectory buffer-file-name) "refiled.org")))
        (kill-buffer " *task-test-navigation*")))))

(ert-deftest task-workflow-select-does-not-change-focus-state-or-notes ()
  (task-workflow-test-with-root
    (let ((first (task-workflow-capture-manual "First task"))
          (second (task-workflow-capture-manual "Second task")))
      (setq task-workflow-focused-id first)
      (org-todo "WAIT")
      (org-end-of-meta-data t)
      (insert "\nWaiting for an answer.\n")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'consult--read) (lambda (&rest _) second)))
          (task-workflow-select))
        (should (equal (org-entry-get nil "ID") second))
        (should (equal (org-get-todo-state) "WAIT"))
        (should (equal (buffer-string) before))
        (should (equal task-workflow-focused-id first)))
      (task-workflow-clear-focus)
      (should-not task-workflow-focused-id)
      (should (equal (org-get-todo-state) "WAIT")))))

(ert-deftest task-workflow-cancelled-selection-leaves-focus-and-location-alone ()
  (task-workflow-test-with-root
    (let* ((id (task-workflow-capture-manual "Keep working"))
           (buffer (current-buffer))
           (position (point)))
      (setq task-workflow-focused-id id)
      (cl-letf (((symbol-function 'consult--read) (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil (task-workflow-select) (quit 'cancelled)) 'cancelled)))
      (should (eq (current-buffer) buffer))
      (should (= (point) position))
      (should (equal task-workflow-focused-id id)))))

(ert-deftest task-workflow-focus-from-picker-does-not-visit-or-complete-task ()
  (task-workflow-test-with-root
    (let ((id (task-workflow-capture-manual "Choose focus")))
      (with-temp-buffer
        (let ((buffer (current-buffer)))
          (cl-letf (((symbol-function 'consult--read) (lambda (&rest _) id)))
            (task-workflow-focus))
          (should (eq buffer (current-buffer)))
          (should (equal task-workflow-focused-id id))
          (should (equal (plist-get (task-workflow--find id) :state) "INBOX")))))))

(ert-deftest task-workflow-completion-and-unsaved-edits-preserve-focus ()
  (task-workflow-test-with-root
    (let ((id (task-workflow-capture-manual "Finish this")))
      (task-workflow-focus)
      (org-todo "DONE")
      (should-not (task-workflow--collect))
      (should (equal task-workflow-focused-id id))
      (task-workflow-visit-focused)
      (should (equal (org-get-todo-state) "DONE"))
      (should (buffer-modified-p))
      (save-buffer)
      (kill-buffer (current-buffer))
      (task-workflow-visit-focused)
      (should (equal (org-entry-get nil "ID") id))
      (should (equal (org-get-todo-state) "DONE")))))

(ert-deftest task-workflow-duplicate-titles-remain-distinct-in-completion ()
  (task-workflow-test-with-root
    (let ((first (task-workflow-capture-manual "Same title"))
          (second (task-workflow-capture-manual "Same title")))
      (let ((rows (task-workflow--candidates (task-workflow--collect))))
        (should-not (equal (caar rows) (caadr rows)))
        (should (equal (consult--lookup-cdr (caar rows) rows) first))
        (should (equal (consult--lookup-cdr (caadr rows) rows) second))))))

(ert-deftest task-workflow-invalid-identities-are-not-silently-repaired ()
  (task-workflow-test-with-root
    (let ((file (task-workflow-test-file "tasks.org" "* TODO Broken\n:PROPERTIES:\n:TASK: t\n:END:\n")))
      (should-error (task-workflow--collect) :type 'user-error)
      (with-current-buffer (find-file-noselect file)
        (should-not (org-entry-get nil "ID"))
        (should-not (buffer-modified-p))
        (erase-buffer)
        (insert (task-workflow-test-heading "TODO" "same-id" "First")
                (task-workflow-test-heading "TODO" "same-id" "Second")))
      (should-error (task-workflow--collect) :type 'user-error))))

(ert-deftest task-workflow-unavailable-focus-is-retained-until-explicitly-cleared ()
  (task-workflow-test-with-root
    (let* ((id (task-workflow-capture-manual "Do not lose identity"))
           (file buffer-file-name))
      (task-workflow-focus)
      (kill-buffer (current-buffer))
      (delete-file file)
      (should-error (task-workflow-visit-focused) :type 'user-error)
      (should (equal task-workflow-focused-id id))
      (task-workflow-clear-focus)
      (should-error (task-workflow-visit-focused) :type 'user-error))))

(ert-deftest task-workflow-empty-read-and-invalid-capture-do-not-create-data ()
  (task-workflow-test-with-root
    (should-error (task-workflow-select) :type 'user-error)
    (dolist (title '("" "  " "First\n* Second" "First\rSecond"))
      (should-error (task-workflow-capture-manual title) :type 'user-error))
    (should-not (file-exists-p (task-workflow--directory)))
    (let ((task-workflow-personal-root (expand-file-name "missing" root)))
      (should-error (task-workflow-capture-manual "Missing root") :type 'user-error)
      (should-not (file-exists-p task-workflow-personal-root)))))

(ert-deftest task-workflow-capture-refuses-symlink-escape ()
  (task-workflow-test-with-root
    (let ((outside (expand-file-name "elsewhere.org" root)))
      (with-temp-file outside (insert "Unrelated document\n"))
      (make-directory (task-workflow--directory))
      (make-symbolic-link outside (expand-file-name "tasks.org" (task-workflow--directory)))
      (should-error (task-workflow-capture-manual "Must stay local") :type 'user-error)
      (should-not (task-workflow--collect))
      (should (equal (with-temp-buffer (insert-file-contents outside) (buffer-string))
                     "Unrelated document\n")))))

(ert-deftest task-workflow-modeline-tracks-explicit-focus-not-selection ()
  (task-workflow-test-with-root
    (let ((first (task-workflow-capture-manual "Recover workstation")))
      (should-not task-workflow-modeline-string)
      (task-workflow-focus)
      (should-not (string-match-p "Recover workstation" task-workflow-modeline-string))
      (should (string-match-p "Recover workstation"
                             (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (let* ((before task-workflow-modeline-string)
             (second (task-workflow-capture-manual "Check a backup")))
        (cl-letf (((symbol-function 'consult--read) (lambda (&rest _) second)))
          (task-workflow-select))
        (should (equal task-workflow-focused-id first))
        (should (equal task-workflow-modeline-string before)))
      (task-workflow-clear-focus)
      (should-not task-workflow-modeline-string))))

(ert-deftest task-workflow-modeline-updates-state-and-saved-title ()
  (task-workflow-test-with-root
    (let ((id (task-workflow-capture-manual "Initial title")))
      (task-workflow-focus)
      (org-todo "WAIT")
      (should (string-match-p "WAIT"
                             (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (org-edit-headline "Renamed task")
      (save-buffer)
      (should (string-match-p "Renamed task"
                             (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (should-not (string-match-p "Initial title"
                                 (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (org-todo "DONE")
      (should (string-match-p "DONE"
                             (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (should (equal task-workflow-focused-id id)))))

(ert-deftest task-workflow-modeline-missing-task-keeps-focus-and-explains-error ()
  (task-workflow-test-with-root
    (let* ((id (task-workflow-capture-manual "Keep the reference"))
           (file buffer-file-name))
      (task-workflow-focus)
      (kill-buffer (current-buffer))
      (delete-file file)
      (should-error (task-workflow-visit-focused) :type 'user-error)
      (should (equal task-workflow-focused-id id))
      (should (eq (get-text-property 0 'face task-workflow-modeline-string) 'warning))
      (should (string-match-p (regexp-quote id)
                             (get-text-property 0 'help-echo task-workflow-modeline-string)))
      (task-workflow-clear-focus)
      (should-not task-workflow-modeline-string))))

(ert-deftest task-workflow-entry-save-retries-one-id-and-preserves-manual-notes ()
  (task-workflow-test-with-root
    (let* ((manual-id (task-workflow-capture-manual "Keep manual notes"))
           (file buffer-file-name)
           (entry (task-workflow-entry-create
                   :root root :title "  Reviewed proposal  "
                   :body "** Outcome\n#+begin_example\nSafe reviewed text\n#+end_example\n"))
           (task-workflow-focused-id manual-id)
           (clock (and (boundp 'org-clock-marker) org-clock-marker)))
      (goto-char (point-max))
      (insert "\nUnsaved manual investigation notes.\n")
      (let ((before (buffer-string))
            (buffer (current-buffer))
            (position (point)))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _) (error "Simulated write failure"))))
          (should-error (task-workflow-save-entry entry)))
        (should (stringp (task-workflow-entry-id entry)))
        (should (equal (buffer-string) before))
        (should (buffer-modified-p))
        (should (eq (current-buffer) buffer))
        (should (= (point) position))
        (let* ((allocated (task-workflow-entry-id entry))
               (record (task-workflow-save-entry entry))
               (saved (buffer-string)))
          (should (equal (plist-get record :id) allocated))
          (should (equal (plist-get record :title) "Reviewed proposal"))
          (should (equal (plist-get record :state) "INBOX"))
          (should (equal (plist-get record :file) file))
          (should (equal (task-workflow-save-entry entry) record))
          (should (equal (buffer-string) saved))
          (should (= (length (task-workflow--collect t)) 2))
          (should (string-match-p "Unsaved manual investigation notes" saved))
          (should (string-match-p "Safe reviewed text" saved))
          (should (equal task-workflow-focused-id manual-id))
          (should (eq (and (boundp 'org-clock-marker) org-clock-marker) clock))
          (should (eq (current-buffer) buffer))
          (should (= (point) position)))))))

(ert-deftest task-workflow-first-save-failure-leaves-no-phantom-task ()
  (task-workflow-test-with-root
    (let ((entry (task-workflow-entry-create
                  :root root :title "Retry initial save" :body "Reviewed notes\n")))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "Simulated first write failure"))))
        (should-error (task-workflow-save-entry entry)))
      (let ((allocated (task-workflow-entry-id entry)))
        (should (stringp allocated))
        (should-not (task-workflow--collect t))
        (with-current-buffer
            (find-file-noselect (expand-file-name "tasks/tasks.org" root))
          (should (string-empty-p (buffer-string)))
          (should-not (buffer-modified-p)))
        (should (equal (plist-get (task-workflow-save-entry entry) :id) allocated))
        (should (= (length (task-workflow--collect t)) 1))))))

(ert-deftest task-workflow-save-existing-id-never-rewrites-manual-edits ()
  (task-workflow-test-with-root
    (let* ((id (task-workflow-capture-manual "Original task"))
           (entry (task-workflow-entry-create
                   :root root :id id :title "Replacement title"
                   :body "Replacement notes must not appear\n")))
      (org-todo "DONE")
      (org-edit-headline "Manually revised title")
      (goto-char (point-max))
      (insert "\nUncommitted manual notes must survive.\n")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _) (ert-fail "Existing task must not be saved"))))
          (let ((record (task-workflow-save-entry entry)))
            (should (equal (plist-get record :id) id))
            (should (equal (plist-get record :title) "Manually revised title"))
            (should (equal (plist-get record :state) "DONE"))))
        (should (equal (buffer-string) before))
        (should (buffer-modified-p))
        (should (= (length (task-workflow--collect t)) 1))))))

(ert-deftest task-workflow-post-write-error-retry-does-not-duplicate ()
  (task-workflow-test-with-root
    (let ((entry (task-workflow-entry-create
                  :root root :title "Committed before hook failure" :body "Keep this body\n"))
          (save (symbol-function 'save-buffer)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest arguments)
                   (apply save arguments)
                   (error "Simulated after-save failure"))))
        (should-error (task-workflow-save-entry entry)))
      (let ((allocated (task-workflow-entry-id entry)))
        (should (equal (plist-get (task-workflow-save-entry entry) :id) allocated))
        (should (= (length (task-workflow--collect t)) 1))
        (let ((file (plist-get (task-workflow--find allocated) :file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "Keep this body" nil t))))))))

(ert-deftest task-workflow-save-failure-rolls-back-only-owned-insertion ()
  (task-workflow-test-with-root
    (task-workflow-capture-manual "Existing task")
    (let ((entry (task-workflow-entry-create :root root :title "Failed proposal")))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _)
                   (goto-char (point-max))
                   (insert "\nUnrelated notes added during failed save.\n")
                   (error "Simulated save hook failure"))))
        (should-error (task-workflow-save-entry entry)))
      (should (string-match-p "Unrelated notes added during failed save" (buffer-string)))
      (should-not (string-match-p "Failed proposal" (buffer-string)))
      (should (buffer-modified-p))
      (should (= (length (task-workflow--collect t)) 1))
      (should (equal (plist-get (task-workflow-save-entry entry) :id)
                     (task-workflow-entry-id entry)))
      (should (= (length (task-workflow--collect t)) 2)))))

(ert-deftest task-workflow-frozen-entry-root-does-not-repoint-focused-task-refresh ()
  (task-workflow-test-with-root
    (let* ((focused (task-workflow-capture-manual "Keep current focus"))
           (other (make-temp-file "task-workflow-frozen-root-" t)))
      (unwind-protect
          (progn
            (task-workflow-focus)
            (let ((before task-workflow-modeline-string))
              (task-workflow-save-entry
               (task-workflow-entry-create :root other :title "Previously captured task"))
              (should (equal task-workflow-focused-id focused))
              (should (equal-including-properties task-workflow-modeline-string before))
              (should (equal (plist-get (task-workflow--find focused) :title)
                             "Keep current focus"))))
        (when-let* ((buffer (get-file-buffer (expand-file-name "tasks/tasks.org" other))))
          (kill-buffer buffer))
        (delete-directory other t)))))

(ert-deftest task-workflow-entry-root-is-not-reinferred-from-current-configuration ()
  (task-workflow-test-with-root
    (let* ((other (make-temp-file "task-workflow-other-root-" t))
           (entry (task-workflow-entry-create :root root :title "Original owner"))
           (buffer (current-buffer)))
      (unwind-protect
          (let* ((task-workflow-personal-root other)
                 (record (task-workflow-save-entry entry)))
            (should (file-in-directory-p (plist-get record :file) root))
            (should-not (file-exists-p (expand-file-name "tasks" other)))
            (should (eq (current-buffer) buffer))
            (should-not task-workflow-focused-id))
        (delete-directory other t)))))

(ert-deftest task-workflow-entry-rejects-escaping-task-directory ()
  (task-workflow-test-with-root
    (let ((outside (make-temp-file "task-workflow-outside-" t)))
      (unwind-protect
          (progn
            (make-symbolic-link outside (expand-file-name "tasks" root))
            (should-error
             (task-workflow-save-entry
              (task-workflow-entry-create :root root :title "Do not escape"))
             :type 'user-error)
            (should-not (file-exists-p (expand-file-name "tasks.org" outside))))
        (delete-directory outside t)))))

(ert-deftest task-workflow-plain-title-validation-preserves-meaning ()
  (should (equal (task-workflow-validate-title "  Investigate suspend failures  ")
                 "Investigate suspend failures"))
  (should (equal (task-workflow-validate-title "Question: why does resume fail?")
                 "Question: why does resume fail?"))
  (should-not (text-properties-at
               0 (task-workflow-validate-title (propertize "Plain title" 'face 'bold)))))

(ert-deftest task-workflow-metadata-or-executable-titles-never-create-tasks ()
  (task-workflow-test-with-root
    (dolist (title (list nil "" " " "First\nSecond" "\nFirst" "First\r" "Tab\ttitle"
                    "* Injected heading" "#+TODO: EVIL" ":ID: model-owned"
                    "[#A] Priority" "COMMENT Hidden task" "Task :ARCHIVE:"
                    "Task :tag:other:" "[[elisp:(message \"unsafe\")][title]]"
                    "elisp:(message \"unsafe\")" "{{{eval((message \"unsafe\"))}}}"
                    "src_emacs-lisp{(message \"unsafe\")}" "call_run()"
                    "Due <2026-10-29 Thu>" "Due [2026-10-29 Thu]"
                    "Diary <%%(diary-float t 4 2)>" "@@html:<script>unsafe</script>@@"
                    "Footnote [fn:unsafe]"
                        (concat "Local " "Variables:")))
      (let ((entry (task-workflow-entry-create :root root :title title)))
        (should-error (task-workflow-save-entry entry) :type 'user-error)
        (should-not (task-workflow-entry-id entry))))
    (should-not (file-exists-p (task-workflow--directory)))))

(ert-deftest task-workflow-unwritten-save-result-is-a-retryable-error ()
  (task-workflow-test-with-root
    (let ((entry (task-workflow-entry-create :root root :title "Must reach disk")))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (should-error (task-workflow-save-entry entry) :type 'user-error))
      (let ((allocated (task-workflow-entry-id entry)))
        (should (stringp allocated))
        (should-not (task-workflow--collect t))
        (should (equal (plist-get (task-workflow-save-entry entry) :id) allocated))
        (should (= (length (task-workflow--collect t)) 1))))))

(provide 'task-workflow-tests)
;;; task-workflow-tests.el ends here
