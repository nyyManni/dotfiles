;;; task-workflow-draft-test-helpers.el --- Shared proposal fixtures -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'task-workflow-draft)

(cl-defstruct task-workflow-draft-test-call request callback prompt root owner backend model)
(defvar task-workflow-draft-test--calls nil)
(defvar task-workflow-draft-test--cancelled nil)
(defvar task-workflow-draft-test--executed nil)

(defconst task-workflow-draft-test--proposal
  '(:title "Restore backup notifications" :outcome "Backup failures are visible"
    :description "Investigate the missing notification."
    :completion_criteria ["A failed backup produces a notification"]
    :next_action "Inspect the notification path" :questions ["Which backup service?"]))

(defmacro task-workflow-draft-test-with-root (&rest body)
  "Run BODY with real isolated persistence and a controllable asynchronous adapter."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "task-proposal-test-" t))
          (task-workflow-personal-root root)
          (task-workflow-focused-id "unrelated-focus")
          (task-workflow-modeline-string nil)
          (gptel--known-backends nil)
          (task-workflow-gptel-backend (gptel-make-openai-oauth "Proposal test"))
          (task-workflow-gptel-model 'gpt-6.1-sol)
          (task-workflow-draft-test--calls nil)
          (task-workflow-draft-test--cancelled nil)
          (org-mode-hook nil)
          (org-agenda-files nil)
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-locations-file (expand-file-name "id-locations" root))
          (org-id-files nil) (org-id-extra-files nil)
          (org-element-use-cache nil)
          (org-log-done nil) (org-log-into-drawer nil)
          (make-backup-files nil)
          ;; Transient buffers must override a user who enables autosave.
          (auto-save-default t))
     (save-window-excursion
       (cl-letf (((symbol-function 'task-workflow-gptel-draft)
                  (lambda (prompt callback &rest arguments)
                    (let ((request (task-workflow-gptel--make-request :state 'running)))
                      (push (make-task-workflow-draft-test-call
                             :request request :callback callback :prompt prompt
                             :root (plist-get arguments :root) :owner (plist-get arguments :owner)
                             :backend task-workflow-gptel-backend :model task-workflow-gptel-model)
                            task-workflow-draft-test--calls)
                      request)))
                 ((symbol-function 'task-workflow-gptel-cancel)
                  (lambda (request)
                    (push request task-workflow-draft-test--cancelled)
                    (setf (task-workflow-gptel-request-state request) 'cancelled))))
         (unwind-protect
             (progn ,@body)
           (dolist (buffer (buffer-list))
             (when (with-current-buffer buffer
                     (or (and (derived-mode-p 'task-workflow-draft-mode)
                              task-workflow-draft-session
                              (equal (task-workflow-draft-session-root task-workflow-draft-session)
                                     (file-name-as-directory root)))
                         (and buffer-file-name (file-in-directory-p buffer-file-name root))))
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer)))
           (delete-directory root t))))))

(defun task-workflow-draft-test-complete (call &optional state proposal error)
  "Deliver CALL, including late duplicate callbacks a real transport must reject."
  (let ((request (task-workflow-draft-test-call-request call)))
    (setf (task-workflow-gptel-request-state request) (or state 'done)
          (task-workflow-gptel-request-result request) (or proposal task-workflow-draft-test--proposal)
          (task-workflow-gptel-request-error request) error)
    (funcall (task-workflow-draft-test-call-callback call) request)
    ;; Match the command loop after an asynchronous callback: current buffer is
    ;; the selected window, even if with-current-buffer restored its caller.
    (set-buffer (window-buffer (selected-window)))))

(defun task-workflow-draft-test-ready (&optional description)
  "Generate and finish a real review candidate, returning its buffer."
  (let* ((owner (task-workflow-draft (or description "The backup notification is missing.")))
         (session (buffer-local-value 'task-workflow-draft-session owner)))
    (with-current-buffer owner (task-workflow-draft-generate))
    (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
    (car (task-workflow-draft-session-buffers session))))
(provide 'task-workflow-draft-test-helpers)
;;; task-workflow-draft-test-helpers.el ends here
