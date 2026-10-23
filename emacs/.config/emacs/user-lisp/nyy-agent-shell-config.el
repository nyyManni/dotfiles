;;; nyy-agent-shell-config.el --- Agent Shell setup -*- lexical-binding: t -*-

;;; Commentary:
;; Public defaults live here; optional customer definitions stay outside Git.

;;; Code:

(require 'nyy-agent-shell-session)
(require 'nyy-agent-shell-mcp)

(declare-function corfu-insert "corfu")

(defvar nyy/agent-shell-customer-directory
  (expand-file-name "emacs/etc/customers/"
                    (or (getenv "XDG_DATA_HOME") "~/.local/share/"))
  "Directory of trusted, private customer configuration files.
Files are loaded alphabetically.  A missing directory is fine; errors in an
existing file are reported normally rather than silently disabling servers.")

(defun nyy/agent-shell-keyring-secret (name)
  "Read NAME from the default keyring when a selected server starts."
  (require 'secrets)
  (or (secrets-get-secret (secrets-get-alias "default") name)
      (user-error "%s not found in keyring" name)))

(defun nyy/agent-shell-default-mcp-servers ()
  "Return fresh public MCP definitions without reading credentials."
  `(((name . "emacs")
     (command . "python3")
     (args . (lambda ()
               (require 'mcp-server)
               (unless mcp-server-running
                 (mcp-server-start-unix))
               (list (expand-file-name
                      "mcp-wrapper.py"
                      (file-name-directory (locate-library "mcp-server")))
                     (mcp-server-get-socket-path)))))
    ((name . "agent-lsp")
     (command . "agent-lsp")
     (args . ("c:clangd" "cpp:clangd"
              "javascript:typescript-language-server,--stdio"
              "rust:rust-analyzer"
              "typescript:typescript-language-server,--stdio"
              "python:rass,--,ty,server,--,ruff,server")))
    ((name . "githits")
     (command . "npx")
     (args . ("-y" "githits@latest" "mcp" "start"))
     (env . (lambda ()
              (unless (getenv "DBUS_SESSION_BUS_ADDRESS")
                (user-error "Emacs has no desktop D-Bus session address"))
              (delq nil
                    (mapcar
                     (lambda (name)
                       (when-let ((value (getenv name)))
                         `((name . ,name) (value . ,value))))
                     '("DBUS_SESSION_BUS_ADDRESS" "XDG_RUNTIME_DIR"
                       "XDG_CONFIG_HOME"))))))
    ((name . "brave-search")
     (command . "npx")
     (args . ("-y" "@brave/brave-search-mcp-server" "--transport" "stdio"))
     (env . (((name . "BRAVE_API_KEY")
              (value . (lambda ()
                         (nyy/agent-shell-keyring-secret "BRAVE_API_KEY")))))))))

(defun nyy/agent-shell-configure-mcp ()
  "Reset public defaults, then load trusted private customer definitions.
Re-running this removes stale definitions and policies before loading files.
No credential lambdas are evaluated during configuration."
  (setq agent-shell-mcp-servers (nyy/agent-shell-default-mcp-servers)
        nyy/agent-shell-mcp-optional-servers nil
        nyy/agent-shell-mcp-server-roots nil)
  (when (file-directory-p nyy/agent-shell-customer-directory)
    (dolist (file (directory-files nyy/agent-shell-customer-directory t "\\.el\\'"))
      (load file nil t t))))

(defun nyy-agent-shell-normalize-return ()
  "Make graphical Return behave like RET in Agent Shell."
  (setq-local key-translation-map (copy-keymap key-translation-map))
  (define-key key-translation-map (kbd "<return>") (kbd "RET")))

(defun nyy/agent-shell-submit (&optional arg)
  "Accept the active Corfu selection, then submit Agent Shell input with ARG."
  (interactive "P")
  (when (and (bound-and-true-p corfu-mode)
             (bound-and-true-p completion-in-region-mode))
    (corfu-insert))
  (agent-shell-submit arg))

(defun nyy/agent-shell-diff-emacs-state ()
  "Use Emacs state for Agent Shell diff buffers."
  (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
    (evil-emacs-state)))

(defun nyy/agent-shell-without-aws (_buffer)
  "Strip inherited AWS access from agent processes and shell commands.
MCP servers must explicitly supply their own profile and credential-file
paths.  This prevents ambient credential discovery, not filesystem access."
  '("env"
    "-u" "AWS_PROFILE"
    "-u" "AWS_DEFAULT_PROFILE"
    "-u" "AWS_ACCESS_KEY_ID"
    "-u" "AWS_SECRET_ACCESS_KEY"
    "-u" "AWS_SESSION_TOKEN"
    "-u" "AWS_SECURITY_TOKEN"
    "-u" "AWS_ROLE_ARN"
    "-u" "AWS_ROLE_SESSION_NAME"
    "-u" "AWS_WEB_IDENTITY_TOKEN_FILE"
    "-u" "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
    "-u" "AWS_CONTAINER_CREDENTIALS_FULL_URI"
    "-u" "AWS_CONTAINER_AUTHORIZATION_TOKEN"
    "-u" "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"
    "AWS_CONFIG_FILE=/dev/null"
    "AWS_SHARED_CREDENTIALS_FILE=/dev/null"
    "AWS_EC2_METADATA_DISABLED=true"))

(defun nyy/agent-shell-configure ()
  "Configure Agent Shell servers, AWS environment filtering, and editing."
  (nyy/agent-shell-configure-mcp)
  (add-hook 'agent-shell-mode-hook #'nyy-agent-shell-normalize-return)
  (setq agent-shell-command-prefix #'nyy/agent-shell-without-aws
        agent-shell-elicitation--experimental-feature-enabled t)
  (with-eval-after-load 'agent-shell-elicitation
    (define-key agent-shell-elicitation-map
                (kbd "C-j") #'agent-shell-elicitation-next-field)
    (define-key agent-shell-elicitation-map
                (kbd "C-k") #'agent-shell-elicitation-previous-field))
  (leader-def-key
    :keymaps 'override :states 'normal
    "c c" 'consult-agent-shell-sessions
    "c n" 'agent-shell-omp-start)
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'insert agent-shell-mode-map (kbd "C-<return>") #'nyy/agent-shell-submit)
  (add-hook 'diff-mode-hook #'nyy/agent-shell-diff-emacs-state))

(provide 'nyy-agent-shell-config)
;;; nyy-agent-shell-config.el ends here
