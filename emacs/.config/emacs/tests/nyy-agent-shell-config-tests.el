;;; nyy-agent-shell-config-tests.el --- Config tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'nyy-agent-shell-config)
(require 'corfu)
(require 'agent-shell-completion)

(defconst nyy/config-test-fixtures
  (expand-file-name "fixtures/customers/"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nyy/config-works-without-private-files ()
  (let ((nyy/agent-shell-customer-directory
         (expand-file-name "not-present/" nyy/config-test-fixtures))
        agent-shell-mcp-servers nyy/agent-shell-mcp-optional-servers
        nyy/agent-shell-mcp-server-roots nyy/agent-shell-uv-credential-stores)
    (nyy/agent-shell-configure-mcp)
    (should (equal (mapcar (lambda (s) (map-elt s 'name)) agent-shell-mcp-servers)
                   '("emacs" "agent-lsp" "githits" "brave-search")))
    (should-not nyy/agent-shell-mcp-optional-servers)
    (should-not nyy/agent-shell-mcp-server-roots)
    (should-not nyy/agent-shell-uv-credential-stores)))

(ert-deftest nyy/config-private-load-is-lazy-and-repeatable ()
  (let ((nyy/agent-shell-customer-directory nyy/config-test-fixtures)
        agent-shell-mcp-servers nyy/agent-shell-mcp-optional-servers
        nyy/agent-shell-mcp-server-roots nyy/agent-shell-uv-credential-stores)
    (dotimes (_ 2)
      (nyy/agent-shell-configure-mcp)
      (should (= (length agent-shell-mcp-servers) 5))
      (should (equal nyy/agent-shell-mcp-optional-servers '("customer-mcp")))
      (should (equal nyy/agent-shell-mcp-server-roots
                     '(("customer-mcp" . ("/tmp/customer/")))))
      (should (equal nyy/agent-shell-uv-credential-stores
                     (list (cons (expand-file-name "~/.config/uv/uv_example.toml")
                                 "example")))))
    (let ((nyy/agent-shell-customer-directory
           (expand-file-name "not-present/" nyy/config-test-fixtures)))
      (nyy/agent-shell-configure-mcp)
      (should (= (length agent-shell-mcp-servers) 4))
      (should-not nyy/agent-shell-mcp-optional-servers)
      (should-not nyy/agent-shell-mcp-server-roots)
      (should-not nyy/agent-shell-uv-credential-stores))))

(ert-deftest nyy/config-register-replaces-server-and-policy ()
  (let (agent-shell-mcp-servers nyy/agent-shell-mcp-optional-servers
        nyy/agent-shell-mcp-server-roots)
    (nyy/agent-shell-mcp-register-server
     '((name . "customer-mcp") (command . "old")) '("/tmp/customer/"))
    (nyy/agent-shell-mcp-register-server
     '((name . "customer-mcp") (command . "new")) '("/tmp/new-root/"))
    (should (= (length agent-shell-mcp-servers) 1))
    (should (equal (map-elt (car agent-shell-mcp-servers) 'command) "new"))
    (should (equal nyy/agent-shell-mcp-optional-servers '("customer-mcp")))
    (should (equal (cdr (assoc "customer-mcp" nyy/agent-shell-mcp-server-roots))
                   '("/tmp/new-root/")))))

(ert-deftest nyy/config-keyring-read-is-deferred ()
  (cl-letf (((symbol-function 'nyy/agent-shell-keyring-secret)
             (lambda (_name) (error "Unexpected keyring access"))))
    (should (= (length (nyy/agent-shell-default-mcp-servers)) 4))))

(ert-deftest nyy/config-graphical-return-is-buffer-local ()
  (let ((key-translation-map (make-sparse-keymap)))
    (with-temp-buffer
      (nyy-agent-shell-normalize-return)
      (should (equal (lookup-key key-translation-map (kbd "<return>")) (kbd "RET"))))
    (should-not (lookup-key key-translation-map (kbd "<return>")))))

(ert-deftest nyy/config-agent-child-does-not-inherit-aws-access ()
  (let* ((names '("AWS_PROFILE" "AWS_DEFAULT_PROFILE" "AWS_ACCESS_KEY_ID"
                  "AWS_SECRET_ACCESS_KEY" "AWS_SESSION_TOKEN" "AWS_SECURITY_TOKEN"
                  "AWS_ROLE_ARN" "AWS_ROLE_SESSION_NAME" "AWS_WEB_IDENTITY_TOKEN_FILE"
                  "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
                  "AWS_CONTAINER_CREDENTIALS_FULL_URI"
                  "AWS_CONTAINER_AUTHORIZATION_TOKEN"
                  "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"))
         (process-environment
          (append (mapcar (lambda (name) (concat name "=synthetic-admin")) names)
                  '("AWS_CONFIG_FILE=/synthetic/admin-config"
                    "AWS_SHARED_CREDENTIALS_FILE=/synthetic/admin-credentials"
                    "AWS_EC2_METADATA_DISABLED=false"
                    "AGENT_SHELL_MCP_EXTRAS=aws" "NYY_SMOKE=preserved")
                  process-environment))
         (agent-shell-command-prefix #'nyy/agent-shell-without-aws)
         (command
          (agent-shell--build-command-for-execution
           (append
            '("sh" "-c"
              "for name do printenv \"$name\" && exit 1; done
test \"$AWS_CONFIG_FILE\" = /dev/null &&
test \"$AWS_SHARED_CREDENTIALS_FILE\" = /dev/null &&
test \"$AWS_EC2_METADATA_DISABLED\" = true &&
test \"$AGENT_SHELL_MCP_EXTRAS\" = aws &&
test \"$NYY_SMOKE\" = preserved && printf isolated"
              "aws-isolation")
            names))))
    (with-temp-buffer
      (should (zerop (apply #'call-process (car command) nil t nil (cdr command))))
      (should (equal (buffer-string) "isolated")))
    (should (equal (getenv "AWS_PROFILE") "synthetic-admin"))))

(ert-deftest nyy/config-agent-uv-follows-session-not-last-customer ()
  (let* ((process-environment (copy-sequence process-environment))
         (direnv-mode t)
         (nyy/agent-shell-uv-credential-stores nil)
         (agent-shell-command-prefix #'nyy/agent-shell-without-aws))
    (nyy/agent-shell-register-uv-credentials "/tmp/uv-alpha.toml" "alpha")
    (nyy/agent-shell-register-uv-credentials "/tmp/uv-beta.toml" "beta")
    (setenv "XDG_DATA_HOME" "/tmp/agent-uv-test-data")
    (setenv "UV_KEYRING_PROVIDER" "subprocess")
    (setenv "UV_PREVIEW" "true")
    (setenv "UV_PREVIEW_FEATURES" "native-auth,index-by-name")
    (dolist (name '("UV_INDEX_PRIVATE_REGISTRY_USERNAME"
                    "UV_INDEX_PRIVATE_REGISTRY_PASSWORD"
                    "UV_INDEX_OTHER_PASSWORD" "UV_PUBLISH_PASSWORD"
                    "UV_INDEX_URL"))
      (setenv name "synthetic-other-customer"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () default-directory))
              ((symbol-function 'direnv-update-directory-environment)
               (lambda (directory &optional _summary)
                 (setenv "UV_CONFIG_FILE"
                         (if (equal directory "/tmp/alpha/")
                             "/tmp/uv-alpha.toml"
                           "/tmp/uv-beta.toml")))))
      ;; Both transitions matter: the last visited buffer can be either customer.
      (dolist (customer '("alpha" "beta" "alpha"))
        (setenv "UV_CONFIG_FILE" "/tmp/stale-other-customer.toml")
        (with-temp-buffer
          (let* ((default-directory (concat "/tmp/" customer "/"))
                 (command
                  (agent-shell--build-command-for-execution
                   '("sh" "-c"
                     "test \"$UV_KEYRING_PROVIDER\" = disabled &&
test \"$NETRC\" = /dev/null &&
test \"$UV_PREVIEW_FEATURES\" = index-by-name &&
test -z \"${UV_PREVIEW+x}${UV_INDEX_PRIVATE_REGISTRY_USERNAME+x}${UV_INDEX_PRIVATE_REGISTRY_PASSWORD+x}${UV_INDEX_OTHER_PASSWORD+x}${UV_PUBLISH_PASSWORD+x}${UV_INDEX_URL+x}\" &&
printf '%s\\n%s' \"$UV_CONFIG_FILE\" \"$UV_CREDENTIALS_DIR\""))))
            (let ((default-directory temporary-file-directory))
              (should (zerop (apply #'call-process (car command) nil t nil
                                    (cdr command)))))
            (should (equal (buffer-string)
                           (format "/tmp/uv-%s.toml\n/tmp/agent-uv-test-data/uv/agent-credentials/%s"
                                   customer customer)))))))
    (should (equal (getenv "UV_KEYRING_PROVIDER") "subprocess"))
    (should (equal (getenv "UV_INDEX_OTHER_PASSWORD") "synthetic-other-customer"))))

(ert-deftest nyy/config-agent-uv-unregistered-and-remote-have-no-customer-store ()
  (let ((process-environment (copy-sequence process-environment))
        (direnv-mode t)
        (nyy/agent-shell-uv-credential-stores nil))
    (nyy/agent-shell-register-uv-credentials "/tmp/uv-alpha.toml" "alpha")
    (setenv "XDG_DATA_HOME" "/tmp/agent-uv-test-data")
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () default-directory))
              ((symbol-function 'direnv-update-directory-environment)
               (lambda (&rest _) (setenv "UV_CONFIG_FILE" "/tmp/unregistered.toml"))))
      (let ((command (append (nyy/agent-shell-without-aws (current-buffer))
                             '("printenv" "UV_CREDENTIALS_DIR"))))
        (with-temp-buffer
          (should (zerop (apply #'call-process (car command) nil t nil (cdr command))))
          (should (equal (buffer-string)
                         "/tmp/agent-uv-test-data/uv/agent-credentials/_unconfigured\n")))))
    (setenv "UV_CONFIG_FILE" "/tmp/uv-alpha.toml")
    (cl-letf (((symbol-function 'agent-shell-cwd)
               (lambda () "/ssh:example.invalid:/tmp/alpha/"))
              ((symbol-function 'direnv-update-directory-environment)
               (lambda (&rest _) (ert-fail "Remote session imported local direnv"))))
      (let ((command (append (nyy/agent-shell-without-aws (current-buffer))
                             '("printenv" "UV_CREDENTIALS_DIR"))))
        (with-temp-buffer
          (should (zerop (apply #'call-process (car command) nil t nil (cdr command))))
          (should (equal (buffer-string)
                         "/tmp/agent-uv-test-data/uv/agent-credentials/_unconfigured\n")))))))

(ert-deftest nyy/config-agent-uv-rejects-store-paths ()
  (let (nyy/agent-shell-uv-credential-stores)
    (dolist (customer '("../other" "/tmp/other" "_unconfigured" "" "Other"))
      (should-error
       (nyy/agent-shell-register-uv-credentials "/tmp/uv.toml" customer)))
    (should-not nyy/agent-shell-uv-credential-stores)))

(ert-deftest nyy/config-submit-accepts-the-selected-corfu-candidate ()
  (dolist (candidate '("rename" "render"))
    (with-temp-buffer
      (insert "/ren")
      (corfu-mode 1)
      (let ((completion-extra-properties
             '(:exit-function agent-shell--capf-exit-with-space))
            (completion-in-region-mode-predicate #'always)
            submitted)
        (unwind-protect
            (progn
              (corfu--setup 2 (copy-marker (point-max) t) '("rename" "render") nil)
              (corfu--update)
              (setq corfu--index (cl-position candidate corfu--candidates :test #'equal))
              (cl-letf (((symbol-function 'agent-shell-submit)
                         (lambda (&optional _) (setq submitted (buffer-string)))))
                (nyy/agent-shell-submit))
              (should (equal submitted (concat "/" candidate " ")))
              (should-not completion-in-region-mode))
          (when completion-in-region-mode (corfu-quit))
          (corfu-mode -1))))))

;;; nyy-agent-shell-config-tests.el ends here
