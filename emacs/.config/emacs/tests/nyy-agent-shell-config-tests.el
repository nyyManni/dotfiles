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
        nyy/agent-shell-mcp-server-roots)
    (nyy/agent-shell-configure-mcp)
    (should (equal (mapcar (lambda (s) (map-elt s 'name)) agent-shell-mcp-servers)
                   '("emacs" "agent-lsp" "githits" "brave-search")))
    (should-not nyy/agent-shell-mcp-optional-servers)
    (should-not nyy/agent-shell-mcp-server-roots)))

(ert-deftest nyy/config-private-load-is-lazy-and-repeatable ()
  (let ((nyy/agent-shell-customer-directory nyy/config-test-fixtures)
        agent-shell-mcp-servers nyy/agent-shell-mcp-optional-servers
        nyy/agent-shell-mcp-server-roots)
    (dotimes (_ 2)
      (nyy/agent-shell-configure-mcp)
      (should (= (length agent-shell-mcp-servers) 5))
      (should (equal nyy/agent-shell-mcp-optional-servers '("customer-mcp")))
      (should (equal nyy/agent-shell-mcp-server-roots
                     '(("customer-mcp" . ("/tmp/customer/"))))))
    (let ((nyy/agent-shell-customer-directory
           (expand-file-name "not-present/" nyy/config-test-fixtures)))
      (nyy/agent-shell-configure-mcp)
      (should (= (length agent-shell-mcp-servers) 4))
      (should-not nyy/agent-shell-mcp-optional-servers)
      (should-not nyy/agent-shell-mcp-server-roots))))

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
