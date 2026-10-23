;;; nyy-agent-shell-session.el --- Agent-shell session APIs -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit ACP session context, descriptive names, and manager integration.
;; Loaded by the agent-shell use-package configuration in init.el.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun nyy/agent-shell-data-directory (subdir)
  "Return a central, project-specific Agent Shell data path for SUBDIR.
Keep transcripts and screenshots out of both home and project directories.
Hash the working directory without resolving it, avoiding remote file access."
  (expand-file-name
   (file-name-concat
    (secure-hash 'sha256 (file-name-as-directory
                         (expand-file-name (agent-shell-cwd))))
    subdir)
   (expand-file-name "var/agent-shell/" user-emacs-directory)))

(setq agent-shell-dot-subdir-function #'nyy/agent-shell-data-directory)

(defun nyy/agent-shell-request-with-session-context (request)
  "Add the ACP session ID to ordinary prompts, leaving slash commands intact."
  (let* ((params (map-elt request :params))
         (first-text (seq-find (lambda (block) (equal (map-elt block 'type) "text"))
                               (map-elt params 'prompt))))
    (if (or (not (equal (map-elt request :method) "session/prompt"))
            (and first-text
                 (string-prefix-p "/" (string-trim-left (map-elt first-text 'text)))))
        request
      (let ((session-id (map-elt params 'sessionId))
            (decorated (copy-tree request t)))
        (unless (and (stringp session-id) (not (string-empty-p session-id)))
          (error "Agent-shell prompt request has no session ID"))
        (setf (alist-get 'prompt (alist-get :params decorated))
              (vconcat
               (vector
                `((type . "text")
                  (text . ,(format "<agent-shell-context>\nACP session ID: %s\nUse this exact ID as session_id for the Emacs MCP rename-session tool and other session-scoped APIs.\n</agent-shell-context>"
                                    session-id))))
               (map-elt params 'prompt)))
        decorated))))

(setq agent-shell-outgoing-request-decorator
      #'nyy/agent-shell-request-with-session-context)

(defun nyy/agent-shell-omp-reuse-selected-session (original sessions)
  "Reuse an open OMP shell when ORIGINAL selects it from SESSIONS.
Only discard the new bootstrapping shell; never close an existing session."
  (let* ((bootstrap (current-buffer))
         (omp (eq (map-nested-elt agent-shell--state '(:agent-config :identifier)) 'omp))
         (choice (funcall original sessions))
         (id (and (listp choice) (map-elt choice 'sessionId)))
         (existing
          (and omp id
               (not (agent-shell-session-id :shell-buffer bootstrap))
               (seq-find
                (lambda (buffer)
                  (and (not (eq buffer bootstrap))
                       (eq (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                           '(:agent-config :identifier))
                           'omp)
                       (equal (file-remote-p (buffer-local-value 'default-directory buffer))
                              (file-remote-p default-directory))
                       (equal id (agent-shell-session-id :shell-buffer buffer))))
                (agent-shell-buffers)))))
    (if (not existing)
        choice
      (agent-shell--display-buffer existing)
      (kill-buffer bootstrap)
      :other-shell)))

(advice-add 'agent-shell--prompt-select-session :around
            #'nyy/agent-shell-omp-reuse-selected-session)

(defvar-local nyy/omp-auto-title-session nil
  "New OMP session awaiting its first completed ordinary prompt.")

(defvar-local nyy/omp-auto-title-timer nil
  "Deferred naming command, scheduled after turn callbacks finish.")

(defvar-local nyy/omp-native-title nil
  "Pair of ACP session ID and authoritative OMP title, not a seeded prompt.")

(defun nyy/omp-auto-title-cancel ()
  "Cancel pending automatic naming in the current buffer."
  (when nyy/omp-auto-title-timer
    (cancel-timer nyy/omp-auto-title-timer))
  (setq nyy/omp-auto-title-timer nil
        nyy/omp-auto-title-session nil))

(defun nyy/omp-auto-title-run (buffer state id)
  "Name new session ID in BUFFER after STATE's user work has finished."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq nyy/omp-auto-title-timer nil)
      (when (and (eq state agent-shell--state)
                 (equal id nyy/omp-auto-title-session)
                 (equal id (agent-shell-session-id))
                 (eq (agent-shell-status) 'ready)
                 (not (map-elt state :pending-prompts))
                 (not (seq-some
                       (lambda (request) (equal (map-elt request :method) "session/prompt"))
                       (map-elt state :active-requests))))
        ;; Consume before dispatch: the generated command must not name itself.
        (setq nyy/omp-auto-title-session nil)
        (condition-case err
            (agent-shell--send-request
             :state state :client (map-elt state :client) :buffer buffer
             :request (acp-make-session-prompt-request
                       :session-id id
                       :prompt [((type . "text") (text . "/rename"))])
             :on-success (lambda (_response)
                           (agent-shell--flush-stream-render state))
             :on-failure (lambda (error _raw)
                           (message "OMP automatic naming failed: %s"
                                    (map-elt error 'message))))
          (error (message "OMP automatic naming failed: %s" (error-message-string err))))))))

(defun nyy/omp-auto-title-request (original &rest args)
  "Track new OMP sessions and completed prompts sent through ORIGINAL ARGS."
  (let* ((state (plist-get args :state))
         (buffer (plist-get args :buffer))
         (request (plist-get args :request))
         (method (map-elt request :method))
         (callback (plist-get args :on-success)))
    (when (and (buffer-live-p buffer)
               (eq (map-nested-elt state '(:agent-config :identifier)) 'omp))
      (with-current-buffer buffer
        (pcase method
          ((or "session/new" "session/load" "session/resume" "session/fork")
           (nyy/omp-auto-title-cancel)
           (setq nyy/omp-native-title nil)
           (when (equal method "session/new")
             (add-hook 'kill-buffer-hook #'nyy/omp-auto-title-cancel nil t)
             (setq args
                   (plist-put
                    args :on-success
                    (lambda (response)
                      (when (and (buffer-live-p buffer)
                                 (eq state (buffer-local-value 'agent-shell--state buffer)))
                        (with-current-buffer buffer
                          (setq nyy/omp-auto-title-session (map-elt response 'sessionId))))
                      (when callback (funcall callback response)))))))
          ("session/prompt"
           (let* ((blocks (map-nested-elt request '(:params prompt)))
                  (block (seq-find
                          (lambda (block) (equal (map-elt block 'type) "text"))
                          blocks))
                  (text (string-trim-left (or (map-elt block 'text) "")))
                  (id (map-nested-elt request '(:params sessionId))))
             (cond
              ((string-match-p "\\`/rename\\(?:[[:space:]:]\\|\\'\\)" text)
               (nyy/omp-auto-title-cancel))
              ((and (equal id nyy/omp-auto-title-session)
                    (or (not (string-empty-p text))
                        (seq-some (lambda (block) (not (equal (map-elt block 'type) "text")))
                                  blocks))
                    (not (string-prefix-p "/" text)))
               (setq args
                     (plist-put
                      args :on-success
                      (lambda (response)
                        ;; Let the normal callback finish and drain queued prompts first.
                        (when callback (funcall callback response))
                        (when (and (buffer-live-p buffer)
                                   (equal (map-elt response 'stopReason) "end_turn"))
                          (with-current-buffer buffer
                            (when (and (eq state agent-shell--state)
                                       (equal id nyy/omp-auto-title-session)
                                       (not nyy/omp-auto-title-timer))
                              (setq nyy/omp-auto-title-timer
                                    (run-at-time 0 nil #'nyy/omp-auto-title-run
                                                 buffer state id)))))))))))))))
    (apply original args)))

(defun nyy/omp-auto-title-notification (&rest args)
  "Remember native OMP titles from the notification in ARGS.
Seeded first-prompt titles are deliberately not authoritative."
  (let* ((state (plist-get args :state))
         (notification (plist-get args :acp-notification))
         (buffer (map-elt state :buffer))
         (id (map-nested-elt notification '(params sessionId)))
         (title (map-nested-elt notification '(params update title))))
    (when (and (buffer-live-p buffer)
               (eq (map-nested-elt state '(:agent-config :identifier)) 'omp)
               (equal (map-elt notification 'method) "session/update")
               (equal (map-nested-elt notification '(params update sessionUpdate))
                      "session_info_update")
               (stringp title) (not (string-blank-p title)))
      (with-current-buffer buffer
        (when (and (eq state agent-shell--state)
                   (equal id (agent-shell-session-id)))
          (setq nyy/omp-native-title (cons id title))
          (nyy/omp-auto-title-cancel)
          (when (fboundp 'agent-shell-manager-refresh)
            (agent-shell-manager-refresh)))))))

(advice-add 'agent-shell--send-request :around #'nyy/omp-auto-title-request)
(advice-add 'agent-shell--on-notification :after #'nyy/omp-auto-title-notification)

(defun nyy/agent-shell-rename-session (session-id topic)
  "Rename SESSION-ID using TOPIC, preserving its viewport association."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (user-error "Supply the explicit ACP session ID from agent context"))
  (unless (and (stringp topic) (not (string-empty-p (string-trim topic)))
               (not (string-match-p "[\n\r]" topic)))
    (user-error "Topic must be a nonempty single line"))
  (let ((matches (seq-filter
                  (lambda (buffer)
                    (equal session-id (agent-shell-session-id :shell-buffer buffer)))
                  (agent-shell-buffers))))
    (unless (= (length matches) 1)
      (user-error "Expected one Agent-shell session for this ID; found %d"
                  (length matches)))
    (let* ((target (car matches))
           (agent (map-nested-elt (buffer-local-value 'agent-shell--state target)
                                  '(:agent-config :buffer-name)))
           (old-name (buffer-name target))
           (old-override (buffer-local-value 'shell-maker--buffer-name-override target))
           (viewport (agent-shell-viewport--buffer :shell-buffer target :existing-only t))
           (old-viewport-name (and viewport (buffer-name viewport)))
           (base-name (format "[%s] %s" agent (string-trim topic)))
           (new-name base-name)
           (number 2))
      (unless (and (stringp agent) (not (string-empty-p agent)))
        (user-error "Session lacks agent metadata"))
      ;; Reserve both names so another session's viewport is never adopted.
      (while (or (let ((existing (get-buffer new-name)))
                   (and existing (not (eq existing target))))
                 (let ((existing (get-buffer (concat new-name agent-shell-viewport--suffix))))
                   (and existing (not (eq existing viewport)))))
        (setq new-name (format "%s<%d>" base-name number)
              number (1+ number)))
      (condition-case err
          (progn
            (shell-maker-set-buffer-name target new-name)
            (when viewport
              (with-current-buffer viewport
                (rename-buffer (concat new-name agent-shell-viewport--suffix)))))
        (error
         (with-current-buffer target
           (rename-buffer old-name)
           (setq shell-maker--buffer-name-override old-override))
         (when (buffer-live-p viewport)
           (with-current-buffer viewport (rename-buffer old-viewport-name)))
         (signal (car err) (cdr err))))
      (when (fboundp 'agent-shell-manager-refresh)
        (agent-shell-manager-refresh))
      (list :session-id session-id :name (buffer-name target)
            :override (buffer-local-value 'shell-maker--buffer-name-override target)
            :viewport (and viewport (buffer-name viewport))))))

(defun nyy/agent-shell-mcp-rename-session (args)
  "Handle a dedicated MCP rename-session call with ARGS."
  (unless (and (listp args)
               (= (length args) 2)
               (stringp (alist-get 'session_id args))
               (stringp (alist-get 'topic args)))
    (user-error "Expected only session_id and topic string arguments"))
  (let ((result (nyy/agent-shell-rename-session
                 (alist-get 'session_id args) (alist-get 'topic args))))
    (json-serialize
     `((session_id . ,(plist-get result :session-id))
       (name . ,(plist-get result :name))
       (override . ,(plist-get result :override))
       (viewport . ,(or (plist-get result :viewport) :null))))))

(defun nyy/agent-shell-register-mcp-tools ()
  "Register the dedicated Agent-shell session tools with Emacs MCP."
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name "rename-session"
    :title "Rename Agent-shell session"
    :description "Set a descriptive name for the Agent-shell conversation identified by its explicit ACP session ID. Supply a short topic without an agent prefix or project path. Preserves viewport association and refreshes the manager."
    :input-schema '((type . "object")
                    (properties . ((session_id . ((type . "string")
                                                 (description . "Exact ACP session ID from agent-shell-context.")))
                                   (topic . ((type . "string")
                                             (description . "Short descriptive topic, without agent prefix or project path.")))))
                    (required . ["session_id" "topic"])
                    (additionalProperties . :false))
    :function #'nyy/agent-shell-mcp-rename-session
    :annotations '((readOnlyHint . :false)
                   (destructiveHint . :false)
                   (idempotentHint . t)
                   (openWorldHint . :false)))))

(with-eval-after-load 'mcp-server-tools
  (nyy/agent-shell-register-mcp-tools))

;; Descriptive names such as "[Codex] Camera discovery" need not contain
;; the agent or project naming convention expected by the manager.
(defun nyy/agent-shell-manager-agent-kind (original buffer)
  "Read BUFFER's agent kind from its config, falling back to ORIGINAL."
  (or (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                      '(:agent-config :buffer-name))
      (funcall original buffer)))

(defun nyy/agent-shell-manager-buffer-config (original buffer)
  "Read BUFFER's agent config from its state, falling back to ORIGINAL."
  (or (map-elt (buffer-local-value 'agent-shell--state buffer) :agent-config)
      (funcall original buffer)))

(defun nyy/agent-shell-manager-session-titles (entries)
  "Show concise names in manager ENTRIES, retaining buffer identities.
Prefer OMP's native title, then an explicit buffer name, then a cached prompt.
Consult uses these same entries, so both session lists show the name."
  (mapcar
   (lambda (entry)
     (let* ((buffer (car entry))
            (state (and (buffer-live-p buffer)
                        (buffer-local-value 'agent-shell--state buffer)))
            (override (and state
                           (buffer-local-value 'shell-maker--buffer-name-override buffer)))
            (native (and state (buffer-local-value 'nyy/omp-native-title buffer)))
            (prefix (and state
                         (agent-shell--buffer-name-prefix
                          (or (map-nested-elt state '(:agent-config :buffer-name)) ""))))
            (title (and state
                        (or (and (equal (car native) (map-nested-elt state '(:session :id)))
                                 (cdr native))
                            (and (stringp override)
                                 (not (and prefix (string-prefix-p prefix override)))
                                 override)
                            (map-nested-elt state '(:session :title))))))
       (if (and (stringp title) (not (string-blank-p title)))
           (let ((columns (copy-sequence (cadr entry))))
             (aset columns 0
                   (truncate-string-to-width
                    (string-join (split-string title) " ") 64 nil nil "…"))
             (list buffer columns))
         entry)))
   entries))

(with-eval-after-load 'agent-shell-manager
  (advice-add 'agent-shell-manager--entries :filter-return
              #'nyy/agent-shell-manager-session-titles)
  (advice-add 'agent-shell-manager--get-agent-kind :around
              #'nyy/agent-shell-manager-agent-kind)
  (advice-add 'agent-shell-manager--get-buffer-config :around
              #'nyy/agent-shell-manager-buffer-config))

(defvar nyy/agent-shell-omp-sessions-directory
  (expand-file-name "~/.omp/agent/sessions/")
  "OMP session storage containing project directories and JSONL sessions.")

(defun nyy/agent-shell-omp-local-file (url)
  "Resolve a local:// URL within the current OMP session's local directory.
Use the exact session ID, including from a viewport; never choose another
session in the same project.  Signal a user error for unavailable artifacts."
  (let* ((shell (if (derived-mode-p 'agent-shell-mode)
                    (current-buffer)
                  (agent-shell-viewport--shell-buffer)))
         (state (and shell (buffer-local-value 'agent-shell--state shell)))
         (id (map-nested-elt state '(:session :id))))
    (unless (eq (map-nested-elt state '(:agent-config :identifier)) 'omp)
      (user-error "local:// links require an OMP session buffer"))
    (unless (and (stringp id)
                 (string-match-p "\\`[[:alnum:]-]+\\'" id))
      (user-error "OMP session has no usable session ID"))
    (let ((sessions (file-expand-wildcards
                     (expand-file-name
                      (concat "*/*_" id ".jsonl")
                      nyy/agent-shell-omp-sessions-directory) t)))
      (unless (= (length sessions) 1)
        (user-error "Expected one OMP session file for %s; found %d"
                    id (length sessions)))
      (let* ((root (file-name-as-directory
                    (concat (file-name-sans-extension (car sessions)) "/local")))
             (relative (decode-coding-string
                        (url-unhex-string (substring url (length "local://")))
                        'utf-8))
             (file (expand-file-name relative root)))
        (unless (and (not (file-name-absolute-p relative))
                     (file-in-directory-p file root))
          (user-error "local:// link escapes the OMP session's local directory"))
        (unless (file-exists-p file)
          (user-error "OMP local artifact not found: %s" file))
        file))))

(defun nyy/agent-shell-open-omp-local-link (original url)
  "Open OMP local:// URL as a file; pass other links to ORIGINAL unchanged."
  (funcall original
           (if (string-prefix-p "local://" url)
               (nyy/agent-shell-omp-local-file url)
             url)))

(defun nyy/agent-shell-omp-local-link-verb (original url verb)
  "Describe local:// URL as a file link, otherwise use ORIGINAL and VERB."
  (if (and (not verb) (string-prefix-p "local://" url))
      "open file"
    (funcall original url verb)))

(with-eval-after-load 'agent-shell-markdown
  (advice-add 'agent-shell-markdown--open-link :around
              #'nyy/agent-shell-open-omp-local-link)
  (advice-add 'agent-shell-markdown--link-verb :around
              #'nyy/agent-shell-omp-local-link-verb))

(provide 'nyy-agent-shell-session)
;;; nyy-agent-shell-session.el ends here
