;;; nyy-agent-shell-session.el --- Agent-shell session APIs -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit ACP session context, descriptive names, and manager integration.
;; Loaded by the agent-shell use-package configuration in init.el.

;;; Code:

(require 'agent-shell)
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
  "Provide the explicit ACP session ID in each outgoing prompt REQUEST."
  (if (not (equal (map-elt request :method) "session/prompt"))
      request
    (let* ((params (map-elt request :params))
           (session-id (map-elt params 'sessionId))
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
      decorated)))

(setq agent-shell-outgoing-request-decorator
      #'nyy/agent-shell-request-with-session-context)

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
  "Show cached session titles in manager ENTRIES, retaining buffer identities.
Fall back to the original buffer label when no nonblank title is available.
Consult uses these same entries, so both session lists show the title."
  (mapcar
   (lambda (entry)
     (let* ((buffer (car entry))
            (title (and (buffer-live-p buffer)
                        (map-nested-elt
                         (buffer-local-value 'agent-shell--state buffer)
                         '(:session :title)))))
       (if (and (stringp title) (not (string-blank-p title)))
           (let ((columns (copy-sequence (cadr entry))))
             (aset columns 0 title)
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

(provide 'nyy-agent-shell-session)
;;; nyy-agent-shell-session.el ends here
