;;; task-workflow-capture.el --- Local selection intake for task proposals -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture only the active selection and local provenance, never surrounding
;; buffer/file contents.  Opening the review does not generate or save a task.
;; Customer/work and remote selections are not authorized for personal intake.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'vc-git)
(require 'task-workflow-draft)

(defgroup task-workflow-capture nil
  "Local selection intake for reviewed personal tasks."
  :group 'task-workflow)

(defcustom task-workflow-capture-excluded-source-roots '("~/Projects/work/")
  "Local trees whose selections must not enter personal task capture.
Both observed paths and symlink-resolved paths are checked.  This is an
intake privacy guard, not a list of customer task roots or authorization
to send customer material.  Remote sources are always refused."
  :type '(repeat directory)
  :group 'task-workflow-capture)

(defun task-workflow-capture--under-root-p (path root)
  "Whether absolute PATH is ROOT itself or a descendant, without file IO."
  (string-prefix-p (file-name-as-directory root)
                   (file-name-as-directory path)))

(defun task-workflow-capture--check-location (file directory)
  "Refuse unauthorized FILE or DIRECTORY before probing project/VC metadata.
FILE may be nil for a non-file buffer.  Return the absolute observed source
location.  Canonicalization is local and is used only for the privacy guard;
the captured file name remains the observed path."
  (when (or (file-remote-p directory)
            (and file (file-remote-p file)))
    (user-error "Remote selections are not authorized for task capture"))
  (let* ((location (expand-file-name (or file directory)))
         (roots (mapcar
                 (lambda (root)
                   (when (file-remote-p root)
                     (user-error "Excluded source roots must be local"))
                   (expand-file-name root))
                 task-workflow-capture-excluded-source-roots)))
    ;; Refuse a directly named excluded tree without even resolving its files.
    (when (cl-some (lambda (root)
                     (task-workflow-capture--under-root-p location root))
                   roots)
      (user-error "Work/customer selections are not authorized for personal task capture"))
    (let ((canonical (file-truename location)))
      (when (cl-some
             (lambda (root)
               (task-workflow-capture--under-root-p canonical (file-truename root)))
             roots)
        (user-error "Work/customer selections are not authorized for personal task capture")))
    location))

(defun task-workflow-capture--git-value (&rest arguments)
  "Return successful local Git output for fixed metadata ARGUMENTS, or nil.
Use the VC backend's executable and argument handling, never a shell or any
selection text.  `default-directory' must already be an authorized local tree."
  (condition-case nil
      (with-temp-buffer
        (when (eq 0 (apply #'vc-git-command (current-buffer) nil nil arguments))
          (let ((value (string-trim (buffer-string))))
            (unless (string-empty-p value) value))))
    (file-error nil)))

(defun task-workflow-capture--provenance (file directory)
  "Collect existing local project and Git provenance for FILE in DIRECTORY.
Call only after intake authorization.  A Git root is the checkout root found
by VC, including for untracked files and linked worktrees, not its common
Git directory.  Missing metadata is omitted, not guessed."
  (let* ((default-directory directory)
         (project (project-current nil directory))
         (project-root (and project (project-root project)))
         (git-root (vc-git-root (or file directory)))
         metadata)
    (when project-root
      (setq project-root (task-workflow-capture--check-location nil project-root))
      (setq metadata (list :project_name (project-name project)
                           :project_root (file-name-as-directory project-root)))
      (when (and file (file-in-directory-p file project-root))
        (setq metadata
              (append metadata (list :project_relative_file
                                     (file-relative-name (file-truename file)
                                                         (file-truename project-root)))))))
    (when git-root
      (setq git-root (task-workflow-capture--check-location nil git-root))
      (let* ((default-directory (file-name-as-directory (expand-file-name git-root)))
             (branch (task-workflow-capture--git-value "symbolic-ref" "--quiet" "--short" "HEAD"))
             (revision (task-workflow-capture--git-value "rev-parse" "--verify" "HEAD")))
        (setq metadata (append metadata (list :git_root default-directory)))
        (when branch
          (setq metadata (append metadata (list :git_branch branch))))
        (when revision
          (setq metadata (append metadata (list :git_revision revision))))))
    metadata))

(defun task-workflow-capture--source (begin end)
  "Snapshot the current buffer's nonempty selection from BEGIN through END.
Bounds may be reversed or narrowed.  Only file-backed sources have absolute,
inclusive line numbers; END at beginning-of-line excludes the following line."
  (let* ((start (min begin end))
         (finish (max begin end))
         (location (task-workflow-capture--check-location buffer-file-name default-directory))
         (file (and buffer-file-name location))
         (directory (if file (file-name-directory location)
                      (file-name-as-directory location))))
    (when (= start finish)
      (user-error "Select nonempty task context"))
    (let ((source (list :kind "selection"
                        :description (buffer-substring-no-properties start finish)
                        :captured_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                        :buffer (copy-sequence (buffer-name))
                        :major_mode (copy-sequence (symbol-name major-mode))
                        :modified (if (buffer-modified-p) t :json-false))))
      (when file
        (save-restriction
          (widen)
          (setq source
                (append source
                        (list :file file
                              :line_start (line-number-at-pos start t)
                              :line_end (line-number-at-pos (1- finish) t))))))
      (append source (task-workflow-capture--provenance file directory)))))

;;;###autoload
(defun task-workflow-capture ()
  "Review the active selection and local provenance as a personal task source.
With no nonempty active region, open the ordinary editable rough-context
review instead.  This command neither requests a model nor creates a task;
Generate and Accept remain explicit actions in `task-workflow-draft-mode'."
  (interactive)
  (if (and (use-region-p) (/= (region-beginning) (region-end)))
      (let ((source (task-workflow-capture--source (region-beginning) (region-end))))
        (task-workflow-draft (plist-get source :description) :sources (list source)))
    (task-workflow-draft)))

(provide 'task-workflow-capture)
;;; task-workflow-capture.el ends here
