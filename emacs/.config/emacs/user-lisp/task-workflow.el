;;; task-workflow.el --- Personal Org tasks -*- lexical-binding: t -*-

;;; Commentary:
;; Marked Org headings are the records; IDs, not titles or positions, identify
;; them.  Phase 1 is local: no trackers, agents, or workspace provisioning.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'subr-x)

(declare-function consult--read "consult")
(declare-function consult--lookup-cdr "consult")

(defgroup task-workflow nil
  "Personal tasks stored as ordinary Org headings."
  :group 'org)

(defcustom task-workflow-personal-root "~/Projects/personal/"
  "Owning directory for personal task data.
Task files are immediate .org children of its tasks/ directory.  No other
Org files or project directories are searched.  Configure before calling
`task-workflow-configure'; customer roots are not supported in phase 1."
  :type 'directory)

(cl-defstruct (task-workflow-entry (:constructor task-workflow-entry-create))
  "Application-owned task input shared by manual capture and reviewed drafts.
ROOT is an absolute local owning directory.  TITLE is plain text, BODY is
already safely rendered Org, and ID is allocated once by the writer."
  root title body id)

(defvar task-workflow-focused-id nil
  "ID of the explicitly focused personal task, or nil.
This is session-local.  Selecting, capturing, or completing a task does not
change focus, start a clock, or launch a process.")

(defvar task-workflow-modeline-string nil
  "Cached focused-task icon and hover details, or nil when nothing is focused.
Updated by focus commands, task-file saves, and Org lifecycle changes.
Redisplay only reads this string; it never searches files.")
(put 'task-workflow-modeline-string 'risky-local-variable t)

(defvar task-workflow--modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'task-workflow-visit-focused)
    map)
  "Mouse map for the focused-task indicator.")

(defconst task-workflow--keywords
  '((sequence "INBOX(i)" "TODO(t)" "NEXT(n)" "TEST(e)" "WAIT(w)" "PAUSED(p)"
              "|" "DONE(d)" "CANCELLED(c)"))
  "Task-file-local Org lifecycle, independent of the global vocabulary.")

(defun task-workflow--directory ()
  "Return the personal task directory without creating it."
  (when (file-remote-p task-workflow-personal-root)
    (user-error "Personal tasks require a local root"))
  (let ((directory (expand-file-name "tasks/" task-workflow-personal-root)))
    (unless (file-in-directory-p directory
                                 (expand-file-name task-workflow-personal-root))
      (user-error "Task directory is outside the personal root: %s" directory))
    directory))

(defun task-workflow--managed-file-p (file)
  "Return non-nil if FILE is an Org file directly in the task directory."
  (and file
       (not (file-remote-p file))
       (string-equal (file-name-extension file) "org")
       (equal (file-truename (file-name-directory (expand-file-name file)))
              (file-truename (task-workflow--directory)))
       (file-in-directory-p file (task-workflow--directory))))

(defun task-workflow--refresh-focused ()
  "Refresh focused task text after a task-file save or lifecycle change."
  (when task-workflow-focused-id
    (task-workflow--refresh-modeline)))

(defun task-workflow--setup-buffer ()
  "Set up lifecycle parsing and focused-task refresh in personal task files."
  (when (task-workflow--managed-file-p buffer-file-name)
    (setq-local org-todo-keywords task-workflow--keywords)
    (org-set-regexps-and-options)
    (add-hook 'after-save-hook #'task-workflow--refresh-focused nil t)
    (add-hook 'org-after-todo-state-change-hook
              #'task-workflow--refresh-focused nil t)))

(defun task-workflow-configure ()
  "Enable personal task parsing and agenda discovery without creating data.
Existing capture templates and the global Org TODO vocabulary are unchanged."
  (add-hook 'org-mode-hook #'task-workflow--setup-buffer)
  (let ((directory (task-workflow--directory)))
    (when (file-directory-p directory)
      (add-to-list 'org-agenda-files directory)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (task-workflow--setup-buffer))))
  (task-workflow--refresh-modeline))

(defun task-workflow--files ()
  "Return registered personal task files without creating directories."
  (unless (file-directory-p (expand-file-name task-workflow-personal-root))
    (user-error "Personal root is unavailable: %s" task-workflow-personal-root))
  (let ((directory (task-workflow--directory)))
    (when (file-directory-p directory)
      (cl-remove-if-not
       (lambda (file)
         (and (file-regular-p file) (task-workflow--managed-file-p file)))
       (directory-files directory t "\\`[^.].*\\.org\\'")))))

(defun task-workflow--collect (&optional include-finished)
  "Read marked task records, optionally INCLUDE-FINISHED tasks.
Only headings with their own TASK property set to t are tasks.
Headings without an ID and duplicate IDs are errors, not silently repaired
by a read operation.  Unsaved edits in visited task buffers are respected."
  (let ((seen (make-hash-table :test #'equal))
        records)
    (dolist (file (task-workflow--files))
      (with-current-buffer (find-file-noselect file)
        (unless (derived-mode-p 'org-mode)
          (user-error "Task file is not in Org mode: %s" file))
        (save-excursion
          (save-restriction
            (widen)
            (org-map-entries
             (lambda ()
               (when (equal (org-entry-get nil "TASK") "t")
                 (let ((id (org-entry-get nil "ID"))
                       (finished (or (org-entry-is-done-p)
                                     (org-in-archived-heading-p))))
                   (unless (and id (not (string-empty-p id)))
                     (user-error "Task without ID in %s; use org-id-get-create" file))
                   (when (gethash id seen)
                     (user-error "Duplicate task ID %s in %s and %s"
                                 id (gethash id seen) file))
                   (puthash id file seen)
                   (org-id-add-location id file)
                   (when (or include-finished (not finished))
                     (push (list :id id
                                 :title (org-get-heading t t t t)
                                 :state (or (org-get-todo-state) "NO STATE")
                                 :file file :position (point))
                           records)))))
             nil 'file)))))
    (nreverse records)))

(defun task-workflow--find (id)
  "Resolve ID against current task files, independent of old positions."
  (or (cl-find id (task-workflow--collect t)
               :key (lambda (record) (plist-get record :id)) :test #'equal)
      (user-error "Task %s is unavailable in %s" id (task-workflow--directory))))

(defun task-workflow--refresh-modeline (&optional record)
  "Cache the focused task's modeline text, optionally using resolved RECORD.
Expected unavailable-file or task-identity errors are visible in the indicator
and its tooltip.  No file access or error handling is deferred to redisplay."
  (setq task-workflow-modeline-string
        (when task-workflow-focused-id
          (condition-case error
              (let* ((record (or record (task-workflow--find task-workflow-focused-id)))
                     (title (substring-no-properties (plist-get record :title)))
                     (state (substring-no-properties (plist-get record :state))))
                (propertize " ⚑ "
                 'help-echo (format "[%s] %s\nmouse-1: visit focused task" state title)
                 'mouse-face 'mode-line-highlight
                 'local-map task-workflow--modeline-map))
            ((user-error file-error)
             (propertize " ⚑ " 'face 'warning
                         'help-echo (error-message-string error)
                         'mouse-face 'mode-line-highlight
                         'local-map task-workflow--modeline-map)))))
  (force-mode-line-update t))

(defun task-workflow--candidates (records)
  "Make unambiguous completion labels paired with IDs from RECORDS."
  (let ((counts (make-hash-table :test #'equal))
        rows)
    (dolist (record records)
      (let ((label (format "%-10s %s  (%s)"
                           (plist-get record :state)
                           (plist-get record :title)
                           (file-name-nondirectory (plist-get record :file)))))
        (puthash label (1+ (gethash label counts 0)) counts)
        (push (cons label (plist-get record :id)) rows)))
    (mapcar (lambda (row)
              (if (= (gethash (car row) counts) 1)
                  row
                (cons (format "%s  [%s]" (car row) (cdr row)) (cdr row))))
            (nreverse rows))))

(defun task-workflow--read-id (prompt)
  "Select an unfinished task ID using PROMPT, with no preview side effects."
  (let ((records (task-workflow--collect)))
    (unless records
      (user-error "No unfinished personal tasks; use task-workflow-capture-manual"))
    (require 'consult)
    (consult--read (task-workflow--candidates records)
                   :prompt prompt :category 'task-workflow
                   :require-match t :sort nil :history t
                   :lookup #'consult--lookup-cdr)))

(defun task-workflow--visit (record)
  "Visit RECORD without changing focus or lifecycle."
  (pop-to-buffer (find-file-noselect (plist-get record :file)))
  (widen)
  (goto-char (plist-get record :position))
  (org-reveal)
  (org-show-entry))

(defun task-workflow-validate-title (title)
  "Return TITLE trimmed and without text properties, or signal `user-error'.
Task titles must be nonempty single-line plain text, not Org metadata,
bracketed/executable links, timestamps, or file-local variable markers.
Invalid input is rejected rather than silently changing its meaning."
  (unless (and (stringp title)
               (not (string-match-p "[[:cntrl:]]" title)))
    (user-error "Use a nonempty, single-line plain task title"))
  (setq title (string-trim (substring-no-properties title)))
  (let ((case-fold-search nil))
    (when (or (string-empty-p title)
              (string-match-p
               (concat "\\`\\(?:\\*+\\(?: \\|$\\)\\|#\\+\\|"
                       ":[[:alnum:]_@#%]+:\\|COMMENT\\(?: \\|$\\)\\|"
                       "\\[#[[:alnum:]]\\]\\)")
               title)
              (string-match-p " +:[[:alnum:]_@#%:]+:\\'" title)
              (let ((case-fold-search t))
                (string-match-p
                 (concat "\\[\\[\\|{{{\\|<<\\|@@\\|%%(\\|\\[fn:\\|"
                         "\\(?:<\\|\\[\\)[0-9]\\{4\\}-[0-9]\\{2\\}-\\|"
                         "\\_<\\(?:elisp\\|shell\\|eval\\|file\\):\\|"
                         "\\_<\\(?:src_\\|call_\\)\\|Local Variables:")
                 title)))
      (user-error "Use a plain task title without Org metadata or executable syntax")))
  title)

(defun task-workflow--entry-written-p (file id)
  "Return non-nil if FILE contains the marked task ID.
Inspect the persisted Org text without visiting it or processing file-local
variables.  The writer uses precious-file saving, so a failed atomic write
cannot leave half an entry that could be mistaken for a committed task."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (let ((org-mode-hook nil)
               (org-inhibit-startup t)
               found)
           (org-mode)
           (org-map-entries
            (lambda ()
              (when (and (equal (org-entry-get nil "TASK") "t")
                         (equal (org-entry-get nil "ID") id))
                (setq found t))))
           found))))

(defun task-workflow-save-entry (entry)
  "Persist ENTRY once and return its ordinary task record.
Use ENTRY's explicit ROOT, never the root configured at callback time.
Allocate ID before attempting to save and retain it on failure.  Existing
IDs return their current record without replacing titles, state, or notes.
An unsuccessful write rolls back only this call's insertion, preserving
unrelated buffer edits.  If saving wrote the complete entry before a hook
failed, retain that committed entry so retry resolves the same record.
This function does not visit, focus, clock, or start anything."
  (unless (task-workflow-entry-p entry)
    (signal 'wrong-type-argument (list 'task-workflow-entry-p entry)))
  (let* ((root (task-workflow-entry-root entry))
         (configured-root task-workflow-personal-root)
         (task-workflow-personal-root
          (if (and (stringp root) (file-name-absolute-p root)
                   (not (file-remote-p root)))
              root
            (user-error "Task entry requires an absolute local root")))
         (directory (task-workflow--directory)))
    (unless (file-directory-p root)
      (user-error "Personal root is unavailable: %s" root))
    (let* ((id (task-workflow-entry-id entry))
           (existing (and id
                          (cl-find id (task-workflow--collect t)
                                   :key (lambda (record) (plist-get record :id))
                                   :test #'equal))))
      (or existing
          (let* ((title (task-workflow-validate-title
                         (task-workflow-entry-title entry)))
                 (body (or (task-workflow-entry-body entry) ""))
                 (file (expand-file-name "tasks.org" directory))
                 position)
            (unless (stringp body)
              (user-error "Task body must be safely rendered Org text"))
            (unless (task-workflow-entry-id entry)
              (setf (task-workflow-entry-id entry) (org-id-new)))
            (setq id (task-workflow-entry-id entry))
            (unless (and (stringp id)
                         (string-match-p "\\`[[:alnum:]_.:@+-]+\\'" id))
              (user-error "Task entry has an invalid application-owned ID"))
            (make-directory directory t)
            (unless (task-workflow--managed-file-p file)
              (user-error "Task file is outside the personal task directory: %s" file))
            (with-current-buffer (find-file-noselect file)
              (unless (derived-mode-p 'org-mode)
                (user-error "Task file is not in Org mode: %s" file))
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-max))
                  (let ((start (copy-marker (point)))
                        (end (copy-marker (point) t))
                        (modified (buffer-modified-p))
                        inserted-tick completed)
                    (unwind-protect
                        (progn
                          (when (bobp)
                            (insert "#+title: Personal tasks\n"
                                    "#+TODO: INBOX(i) TODO(t) NEXT(n) TEST(e) WAIT(w) PAUSED(p) | DONE(d) CANCELLED(c)\n\n"))
                          (task-workflow--setup-buffer)
                          (unless (bolp) (insert "\n"))
                          (unless (bobp) (insert "\n"))
                          (setq position (point))
                          (insert "* INBOX " title "\n")
                          (goto-char position)
                          (org-entry-put nil "TASK" "t")
                          (org-entry-put nil "ID" id)
                          (org-end-of-meta-data t)
                          (insert "\n" body)
                          (unless (bolp) (insert "\n"))
                          (setq inserted-tick (buffer-chars-modified-tick))
                          (set-marker-insertion-type start t)
                          (set-marker-insertion-type end nil)
                          ;; Save hooks refresh the user's focused task in its
                          ;; configured context, not this entry's frozen root.
                          (let ((file-precious-flag t)
                                (task-workflow-personal-root configured-root))
                            (save-buffer))
                          (unless (task-workflow--entry-written-p file id)
                            (user-error "Task was not saved; preserve this draft and retry"))
                          (setq completed t))
                      (unless completed
                        (let ((written
                               (and inserted-tick
                                    (condition-case nil
                                        (task-workflow--entry-written-p file id)
                                      (error nil)))))
                          (unless written
                            (let ((changed (and inserted-tick
                                                (/= inserted-tick
                                                    (buffer-chars-modified-tick)))))
                              (delete-region start end)
                              (set-buffer-modified-p (or modified changed))))))
                      (set-marker start nil)
                      (set-marker end nil))))))
            (org-id-add-location id file)
            (add-to-list 'org-agenda-files directory)
            (list :id id :title title :state "INBOX" :file file
                  :position position))))))

(defun task-workflow-capture-manual (title)
  "Capture plain TITLE as an INBOX task, visit it, and return its stable ID.
Use the same writer as reviewed proposals, without requiring a model.
Capture does not change focus, clocks, or task execution sessions."
  (interactive (list (read-string "Personal task: " nil t)))
  (let* ((entry (task-workflow-entry-create
                 :root (expand-file-name task-workflow-personal-root)
                 :title title :body ""))
         (record (task-workflow-save-entry entry)))
    (task-workflow--visit record)
    (message "Captured personal task: %s" (plist-get record :title))
    (plist-get record :id)))

(defun task-workflow-select ()
  "Select and visit an unfinished personal task without focusing it."
  (interactive)
  (task-workflow--visit (task-workflow--find (task-workflow--read-id "Personal task: "))))

(defun task-workflow--id-at-point ()
  "Return the nearest marked task's ID in a managed Org buffer, or nil."
  (when (and (derived-mode-p 'org-mode)
             (task-workflow--managed-file-p buffer-file-name))
    (save-excursion
      (unless (org-before-first-heading-p)
        (org-back-to-heading t)
        (while (and (not (equal (org-entry-get nil "TASK") "t"))
                    (org-up-heading-safe)))
        (when (equal (org-entry-get nil "TASK") "t")
          (or (org-entry-get nil "ID")
              (user-error "Task has no ID; use org-id-get-create")))))))

(defun task-workflow-focus ()
  "Explicitly focus the task at point, or select an unfinished task.
Do not visit a different buffer, alter lifecycle, or start a clock."
  (interactive)
  (let* ((id (or (task-workflow--id-at-point)
                 (task-workflow--read-id "Focus personal task: ")))
         (record (task-workflow--find id)))
    (setq task-workflow-focused-id id)
    (task-workflow--refresh-modeline record)
    (message "Focused task: %s" (plist-get record :title))
    id))

(defun task-workflow-visit-focused ()
  "Visit the explicitly focused task, even if it has since been completed.
A deleted or moved-outside-the-task-directory task is reported as unavailable;
its focus identity is retained until explicitly cleared or replaced."
  (interactive)
  (unless task-workflow-focused-id
    (user-error "No focused task; use task-workflow-focus"))
  (task-workflow--refresh-modeline)
  (task-workflow--visit (task-workflow--find task-workflow-focused-id)))

(defun task-workflow-clear-focus ()
  "Clear the in-memory task focus without changing task records."
  (interactive)
  (setq task-workflow-focused-id nil)
  (task-workflow--refresh-modeline)
  (message "Task focus cleared"))

(provide 'task-workflow)
;;; task-workflow.el ends here
