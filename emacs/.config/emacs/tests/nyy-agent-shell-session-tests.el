;;; nyy-agent-shell-session-tests.el --- Session MCP tests -*- lexical-binding: t -*-

(require 'ert)
(require 'nyy-agent-shell-session)
(require 'mcp-server-tools)

(defmacro nyy/session-tool-test (&rest body)
  "Run BODY with one isolated Agent-shell buffer and MCP registry."
  (declare (indent 0))
  `(let ((shell (generate-new-buffer "test session tool"))
         (mcp-server-tools--registry (make-hash-table :test 'equal))
         (mcp-server-tools-filter nil))
     (unwind-protect
         (progn
           (with-current-buffer shell
             (setq major-mode 'agent-shell-mode)
             (setq-local agent-shell--state
                         '((:session . ((:id . "test-session-id")))
                           (:agent-config . ((:buffer-name . "Codex")))))
             (setq-local shell-maker--buffer-name-override nil))
           (nyy/agent-shell-register-mcp-tools)
           (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list shell)))
                     ((symbol-function 'agent-shell-manager-refresh) #'ignore))
             ,@body))
       (when (buffer-live-p shell) (kill-buffer shell)))))

(ert-deftest nyy/session-tool-registration ()
  (nyy/session-tool-test
    (let* ((tool (mcp-server-tools-get "rename-session"))
           (schema (mcp-server-tool-input-schema tool))
           (annotations (mcp-server-tool-annotations tool)))
      (should (eq (mcp-server-tool-function tool) 'nyy/agent-shell-mcp-rename-session))
      (should (equal (map-elt schema 'required) ["session_id" "topic"]))
      (should (eq (map-elt schema 'additionalProperties) :false))
      (should (eq (map-elt annotations 'readOnlyHint) :false))
      (should (eq (map-elt annotations 'destructiveHint) :false))
      (should (eq (map-elt annotations 'idempotentHint) t))
      (should (eq (map-elt annotations 'openWorldHint) :false))
      (should (= (length (mcp-server-tools-list)) 1)))))

(ert-deftest nyy/session-tool-dispatch-and-idempotence ()
  (nyy/session-tool-test
    (let* ((args '((session_id . "test-session-id") (topic . "Tool dispatch")))
           (result (mcp-server-tools-call "rename-session" args))
           (payload (json-parse-string (map-elt (aref result 0) 'text)
                                       :object-type 'alist :null-object nil)))
      (should (equal (map-elt payload 'session_id) "test-session-id"))
      (should (equal (map-elt payload 'name) "[Codex] Tool dispatch"))
      (should (equal (map-elt payload 'override) (buffer-name shell)))
      (should-not (map-elt payload 'viewport))
      (should (equal result (mcp-server-tools-call "rename-session" args))))))

(ert-deftest nyy/session-tool-rejects-invalid-arguments ()
  (nyy/session-tool-test
    (dolist (args '(((topic . "No ID"))
                    ((session_id . 42) (topic . "Wrong ID type"))
                    ((session_id . "test-session-id") (topic . 42))
                    ((session_id . "test-session-id") (topic . " "))
                    ((session_id . "test-session-id") (topic . "Two\nlines"))
                    ((session_id . "test-session-id") (topic . "Unexpected field")
                     (expression . "arbitrary code"))
                    ((session_id . "missing-session") (topic . "Unknown ID"))))
      (should-error (nyy/agent-shell-mcp-rename-session args) :type 'user-error))
    (should (equal (buffer-name shell) "test session tool"))))

(ert-deftest nyy/session-tool-preserves-viewport ()
  (nyy/session-tool-test
    (let ((viewport (generate-new-buffer (concat (buffer-name shell)
                                                agent-shell-viewport--suffix))))
      (unwind-protect
          (let* ((result (nyy/agent-shell-mcp-rename-session
                          '((session_id . "test-session-id") (topic . "Viewport"))))
                 (payload (json-parse-string result :object-type 'alist)))
            (should (equal (map-elt payload 'viewport) (buffer-name viewport)))
            (should (eq (agent-shell-viewport--shell-buffer viewport) shell)))
        (kill-buffer viewport)))))

(ert-deftest nyy/session-tool-respects-tool-filter ()
  (nyy/session-tool-test
    (let ((mcp-server-tools-filter (lambda (_name) nil)))
      (should-not (mcp-server-tools-list))
      (should-error (mcp-server-tools-call
                     "rename-session"
                     '((session_id . "test-session-id") (topic . "Disabled")))))))

(ert-deftest nyy/session-manager-shows-title-without-renaming ()
  (nyy/session-tool-test
    (let* ((status (propertize "Ready" 'face 'success))
           (columns (vector (buffer-name shell) status "Default" "Model" "-" "~/"))
           (entry (list shell columns)))
      (with-current-buffer shell
        (setf (alist-get :title (alist-get :session agent-shell--state))
              "OpenCode session title"))
      (let* ((result (nyy/agent-shell-manager-session-titles (list entry)))
             (row (cadar result)))
        (should (eq (caar result) shell))
        (should (equal (aref row 0) "OpenCode session title"))
        (should (eq (aref row 1) status))
        (should (equal (seq-subseq row 1) (seq-subseq columns 1)))
        (should (equal (aref columns 0) "test session tool"))
        (should (equal (buffer-name shell) "test session tool"))
        (should-not shell-maker--buffer-name-override)))))

(ert-deftest nyy/session-manager-title-fallback ()
  (nyy/session-tool-test
    (let ((entry (list shell ["Original label" "Ready" "-" "-" "-" "~/"])))
      (dolist (title '(nil "" "  \t\n" 42))
        (with-current-buffer shell
          (setf (alist-get :title (alist-get :session agent-shell--state)) title))
        (should (eq (car (nyy/agent-shell-manager-session-titles (list entry)))
                    entry)))
      (kill-buffer shell)
      (should (eq (car (nyy/agent-shell-manager-session-titles (list entry)))
                  entry))
      (should-not (nyy/agent-shell-manager-session-titles nil)))))

(ert-deftest nyy/session-data-is-centralized-and-project-specific ()
  (let ((user-emacs-directory "/test/emacs/")
        (cwd "/test/project-a/"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () cwd)))
      (let ((transcripts (nyy/agent-shell-data-directory "transcripts"))
            (screenshots (nyy/agent-shell-data-directory "screenshots")))
        (should (string-prefix-p "/test/emacs/var/agent-shell/" transcripts))
        (should (equal (file-name-directory transcripts)
                       (file-name-directory screenshots)))
        (setq cwd "/test/project-a")
        (should (equal transcripts (nyy/agent-shell-data-directory "transcripts")))
        (setq cwd "/test/project-b/")
        (should-not (equal transcripts (nyy/agent-shell-data-directory "transcripts")))))))

(ert-deftest nyy/session-data-directory-is-configured ()
  (should (eq agent-shell-dot-subdir-function #'nyy/agent-shell-data-directory)))

(provide 'nyy-agent-shell-session-tests)
;;; nyy-agent-shell-session-tests.el ends here
