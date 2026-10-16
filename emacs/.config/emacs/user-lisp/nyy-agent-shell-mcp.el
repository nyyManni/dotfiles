;;; nyy-agent-shell-mcp.el --- Project-scoped MCP servers -*- lexical-binding: t -*-

;;; Commentary:
;; Select customer servers with AGENT_SHELL_MCP_EXTRAS from direnv.
;; Other servers are always available.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'direnv)

(defvar nyy/agent-shell-mcp-optional-servers nil
  "Servers enabled only by AGENT_SHELL_MCP_EXTRAS.
All other configured servers are always available.")

(defvar nyy/agent-shell-mcp-server-roots nil
  "Alist mapping restricted server names to allowed local directory roots.")

(defun nyy/agent-shell-mcp-register-server (server &optional roots)
  "Register private SERVER, requiring explicit selection in direnv.
When ROOTS is non-nil, also restrict SERVER to those local directories.
Registration replaces an existing definition with the same name.  Credential
lambdas in SERVER are not evaluated here."
  (let ((name (map-elt server 'name)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (error "MCP server needs a nonempty name"))
    (setq agent-shell-mcp-servers
          (append (seq-remove (lambda (item) (equal (map-elt item 'name) name))
                              agent-shell-mcp-servers)
                  (list server)))
    (cl-pushnew name nyy/agent-shell-mcp-optional-servers :test #'equal)
    (setf (alist-get name nyy/agent-shell-mcp-server-roots nil nil #'equal)
          (mapcar #'expand-file-name roots))))

(defun nyy/agent-shell-mcp-extras ()
  "Read optional servers from AGENT_SHELL_MCP_EXTRAS.
Accept comma-separated or whitespace-separated names; aws aliases aws-mcp.
An unset or empty variable enables no optional servers."
  (mapcar (lambda (name) (if (equal name "aws") "aws-mcp" name))
          (split-string (or (getenv "AGENT_SHELL_MCP_EXTRAS") "")
                        "[,[:space:]]+" t)))

(defun nyy/agent-shell-mcp-server-allowed-p (server extras)
  "Return non-nil when SERVER is selected and its directory policy allows it."
  (let* ((name (map-elt server 'name))
         (roots (alist-get name nyy/agent-shell-mcp-server-roots nil nil #'equal)))
    (and (or (not (member name nyy/agent-shell-mcp-optional-servers))
             (member name extras))
          (or (null roots)
              (and (not (file-remote-p default-directory))
                   (seq-some
                    (lambda (root)
                      (and (not (file-remote-p root))
                           (file-in-directory-p
                            (file-truename default-directory)
                            (file-name-as-directory (file-truename root)))))
                    roots))))))

(defun nyy/agent-shell-mcp-for-project (original &rest args)
  "Call ORIGINAL with MCP servers selected by the session environment.
Filter definitions before agent-shell evaluates their credential lambdas.
Bind copies so filtering one session does not change another's configuration.
ARGS are forwarded unchanged."
  (let* ((directory (agent-shell-cwd))
         (default-directory directory))
    ;; Refresh from the actual session directory, not the last visited buffer.
    ;; Do not import the selected local project's extras into remote sessions.
    (when (and direnv-mode (not (file-remote-p directory)))
      (direnv-update-directory-environment directory))
    (let* ((extras (unless (file-remote-p directory)
                     (nyy/agent-shell-mcp-extras)))
           (agent-shell--state (copy-alist agent-shell--state))
           (config (copy-alist (map-elt agent-shell--state :agent-config)))
           (servers (or (map-elt config :mcp-servers) agent-shell-mcp-servers))
           (agent-shell-mcp-servers
            (seq-filter (lambda (server)
                          (nyy/agent-shell-mcp-server-allowed-p server extras))
                        servers)))
      (when config
        (setf (map-elt config :mcp-servers) agent-shell-mcp-servers
              (map-elt agent-shell--state :agent-config) config))
      (apply original args))))

(advice-add 'agent-shell--mcp-servers :around #'nyy/agent-shell-mcp-for-project)

(provide 'nyy-agent-shell-mcp)
;;; nyy-agent-shell-mcp.el ends here
