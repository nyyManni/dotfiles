;;; nyy-agent-shell-config-tests.el --- Config tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'nyy-agent-shell-config)

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

;;; nyy-agent-shell-config-tests.el ends here
