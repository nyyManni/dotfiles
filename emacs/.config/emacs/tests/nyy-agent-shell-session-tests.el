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

(defmacro nyy/omp-local-test (&rest body)
  "Run BODY with two OMP sessions containing identically named artifacts."
  (declare (indent 0))
  `(let* ((nyy/agent-shell-omp-sessions-directory (make-temp-file "omp-links-" t))
          (shell (generate-new-buffer "test OMP links"))
          (viewport (generate-new-buffer
                     (concat (buffer-name shell) agent-shell-viewport--suffix)))
          (first-file (expand-file-name "project/time_session-one/local/a b.svg"
                                       nyy/agent-shell-omp-sessions-directory))
          (second-file (expand-file-name "project/time_session-two/local/a b.svg"
                                        nyy/agent-shell-omp-sessions-directory)))
     (unwind-protect
         (progn
           (dolist (file (list first-file second-file))
             (make-directory (file-name-directory file) t)
             (write-region "<svg/>" nil file nil 'silent)
             (write-region "" nil
                           (concat (directory-file-name
                                    (file-name-directory
                                     (directory-file-name (file-name-directory file))))
                                   ".jsonl")
                           nil 'silent))
           (with-current-buffer shell
             (setq major-mode 'agent-shell-mode)
             (setq-local agent-shell--state
                         (copy-tree
                          '((:session . ((:id . "session-one")))
                            (:agent-config . ((:identifier . omp)))))))
           ,@body)
       (kill-buffer viewport)
       (kill-buffer shell)
       (delete-directory nyy/agent-shell-omp-sessions-directory t))))

(ert-deftest nyy/omp-local-links-use-exact-session-and-viewport ()
  (nyy/omp-local-test
    (with-current-buffer shell
      (should (equal (nyy/agent-shell-omp-local-file "local://a%20b.svg")
                     first-file))
      (setf (alist-get :id (alist-get :session agent-shell--state)) "session-two")
      (should (equal (nyy/agent-shell-omp-local-file "local://a%20b.svg")
                     second-file)))
    (with-current-buffer viewport
      (should (equal (nyy/agent-shell-omp-local-file "local://a%20b.svg")
                     second-file)))))

(ert-deftest nyy/omp-local-links-do-not-fall-back-to-other-sessions ()
  (nyy/omp-local-test
    (delete-file first-file)
    (with-current-buffer shell
      (should-error (agent-shell-markdown--open-link "local://a%20b.svg")
                    :type 'user-error)
      (setf (alist-get :id (alist-get :session agent-shell--state)) "missing")
      (should-error (nyy/agent-shell-omp-local-file "local://a%20b.svg")
                    :type 'user-error))))

(ert-deftest nyy/omp-local-links-stay-inside-session ()
  (nyy/omp-local-test
    (with-current-buffer shell
      (dolist (url '("local://../outside" "local://%2e%2e/outside"
                     "local:///etc/passwd"))
        (should-error (nyy/agent-shell-omp-local-file url) :type 'user-error))
      (make-symbolic-link second-file
                         (expand-file-name "escape.svg"
                                           (file-name-directory first-file)))
      (should-error (nyy/agent-shell-omp-local-file "local://escape.svg")
                    :type 'user-error))))

(ert-deftest nyy/session-context-preserves-slash-commands-and-arguments ()
  (dolist (text '("/rename Implementing the ETL iteration"
                  "/rename" "/model openai/gpt-5" " \n/rename A title"))
    (let* ((request `((:method . "session/prompt")
                      (:params . ((sessionId . "test-session")
                                  (prompt . [((type . "text") (text . ,text))])))))
           (result (nyy/agent-shell-request-with-session-context request)))
      (should (equal result request))
      (should (equal (map-elt (aref (map-nested-elt result '(:params prompt)) 0) 'text)
                     text)))))

(ert-deftest nyy/session-context-keeps-ordinary-prompts-and-attachments ()
  (let* ((blocks [((type . "text") (text . "Explain /rename"))
                  ((type . "image") (data . "image-data") (mimeType . "image/png"))])
         (request `((:method . "session/prompt")
                    (:params . ((sessionId . "test-session")
                                (prompt . ,blocks)))))
         (result (nyy/agent-shell-request-with-session-context request))
         (prompt (map-nested-elt result '(:params prompt))))
    (should (string-match-p "ACP session ID: test-session"
                            (map-elt (aref prompt 0) 'text)))
    (should (equal (seq-subseq prompt 1) blocks))
    (should (equal (map-nested-elt request '(:params prompt)) blocks))))

(defmacro nyy/omp-reuse-test (&rest body)
  "Run BODY with a bootstrapping shell and two different saved sessions."
  (declare (indent 0))
  `(let ((bootstrap (generate-new-buffer "test OMP bootstrap"))
         (existing (generate-new-buffer "test OMP existing"))
         (unrelated (generate-new-buffer "test OMP unrelated"))
         (noninteractive t)
         displayed)
     (unwind-protect
         (progn
           (dolist (buffer (list bootstrap existing unrelated))
             (with-current-buffer buffer
               (setq major-mode 'agent-shell-mode)
               (setq-local default-directory temporary-file-directory)
               (setq-local agent-shell--state
                           (list (cons :buffer buffer)
                                 (cons :agent-config (list (cons :identifier 'omp)))
                                 (cons :session
                                       (list (cons :id (cond
                                                       ((eq buffer existing) "saved")
                                                       ((eq buffer unrelated) "other")))))))))
           (cl-letf (((symbol-function 'agent-shell-buffers)
                      (lambda () (seq-filter #'buffer-live-p
                                             (list existing unrelated bootstrap))))
                     ((symbol-function 'agent-shell--display-buffer)
                      (lambda (buffer) (setq displayed buffer))))
             (with-current-buffer bootstrap ,@body)))
       (dolist (buffer (list bootstrap existing unrelated))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest nyy/omp-picker-reuses-open-session-without-closing-others ()
  (nyy/omp-reuse-test
    (should (eq (agent-shell--prompt-select-session '(((sessionId . "saved"))))
                :other-shell))
    (should (eq displayed existing))
    (should-not (buffer-live-p bootstrap))
    (should (buffer-live-p existing))
    (should (buffer-live-p unrelated))))

(ert-deftest nyy/omp-picker-keeps-unopened-and-other-agent-sessions ()
  (nyy/omp-reuse-test
    (let ((unopened '((sessionId . "unopened")))
          (saved '((sessionId . "saved"))))
      (should (equal (agent-shell--prompt-select-session (list unopened)) unopened))
      (setf (alist-get :identifier (alist-get :agent-config agent-shell--state)) 'codex)
      (should (equal (agent-shell--prompt-select-session (list saved)) saved))
      (should-not displayed)
      (should (buffer-live-p bootstrap)))))

(ert-deftest nyy/session-manager-prefers-explicit-names-and-bounds-prompt-titles ()
  (nyy/session-tool-test
    (with-current-buffer shell
      (setq-local shell-maker--buffer-name-override "Codex Agent @ project")
      (setf (alist-get :title (alist-get :session agent-shell--state))
            (concat "First line\n\n" (make-string 100 ?界)))
      (let* ((entry (list shell ["Original" "Ready" "-" "-" "-" "~/"]))
             (title (aref (cadar (nyy/agent-shell-manager-session-titles (list entry))) 0)))
        (should (string-prefix-p "First line " title))
        (should (<= (string-width title) 64))
        (should-not (string-match-p "[\n\r\t]" title))
        (setq-local shell-maker--buffer-name-override "[OMP] ETL iteration")
        (should (equal
                 (aref (cadar (nyy/agent-shell-manager-session-titles (list entry))) 0)
                 "[OMP] ETL iteration"))))))

(defmacro nyy/omp-auto-title-test (&rest body)
  "Exercise BODY against the real request tracker with an isolated transport."
  (declare (indent 0))
  `(let ((shell (generate-new-buffer "test OMP automatic title"))
         sent busy)
     (unwind-protect
         (with-current-buffer shell
           (setq major-mode 'agent-shell-mode)
           (setq-local agent-shell--state
                       (list (cons :buffer shell)
                             (cons :agent-config (list (cons :identifier 'omp)))
                             (cons :session (list (cons :id "auto-session")))
                             (cons :active-requests nil)
                             (cons :pending-prompts nil)))
           (cl-letf (((symbol-function 'acp-send-request)
                      (lambda (&rest args) (push args sent)))
                     ((symbol-function 'agent-shell-status)
                      (lambda (&rest _) (if busy 'busy 'ready)))
                     ((symbol-function 'agent-shell--flush-stream-render) #'ignore))
             (cl-flet ((send (method &optional text)
                         (agent-shell--send-request
                          :state agent-shell--state :buffer shell
                          :request (if (equal method "session/prompt")
                                       (acp-make-session-prompt-request
                                        :session-id "auto-session"
                                        :prompt (if (vectorp text) text
                                                  (vector `((type . "text") (text . ,text)))))
                                     (list (cons :method method)))))
                       (reply (response)
                         (funcall (plist-get (car sent) :on-success) response))
                       (fire ()
                         (when nyy/omp-auto-title-timer
                           (cancel-timer nyy/omp-auto-title-timer))
                         (nyy/omp-auto-title-run shell agent-shell--state "auto-session"))
                       (rename-count ()
                         (seq-count
                          (lambda (args)
                            (equal (map-nested-elt (plist-get args :request)
                                                   '(:params prompt))
                                   [((type . "text") (text . "/rename"))]))
                          sent)))
               ,@body)))
       (when (buffer-live-p shell)
         (with-current-buffer shell (nyy/omp-auto-title-cancel))
         (kill-buffer shell)))))

(ert-deftest nyy/omp-auto-title-supports-image-only-conversations ()
  (nyy/omp-auto-title-test
    (send "session/new")
    (reply '((sessionId . "auto-session")))
    (send "session/prompt" [((type . "image") (data . "image-data") (mimeType . "image/png"))])
    (reply '((stopReason . "end_turn")))
    (fire)
    (should (= (rename-count) 1))))

(ert-deftest nyy/omp-auto-title-names-once-after-a-completed-conversation-turn ()
  (nyy/omp-auto-title-test
    (send "session/new")
    (reply '((sessionId . "auto-session")))
    (send "session/prompt" "/model")
    (reply '((stopReason . "end_turn")))
    (should-not nyy/omp-auto-title-timer)
    (send "session/prompt" "Implement the ETL iteration")
    (reply '((stopReason . "cancelled")))
    (should-not nyy/omp-auto-title-timer)
    (send "session/prompt" "Continue the ETL iteration")
    (reply '((stopReason . "end_turn")))
    (should nyy/omp-auto-title-timer)
    (fire)
    (should (= (rename-count) 1))
    (reply '((stopReason . "end_turn")))
    (should-not nyy/omp-auto-title-timer)
    (send "session/prompt" "Add another importer")
    (reply '((stopReason . "end_turn")))
    (should (= (rename-count) 1))
    (should-not nyy/omp-auto-title-timer)))

(ert-deftest nyy/omp-auto-title-does-not-overwrite-manual-or-restored-titles ()
  (dolist (method '("session/load" "session/resume" "session/fork"))
    (nyy/omp-auto-title-test
      (send method)
      (reply '((sessionId . "auto-session")))
      (send "session/prompt" "Continue working")
      (reply '((stopReason . "end_turn")))
      (should-not nyy/omp-auto-title-timer)
      (should (= (rename-count) 0))))
  (nyy/omp-auto-title-test
    (send "session/new")
    (reply '((sessionId . "auto-session")))
    (send "session/prompt" "/rename My chosen name")
    (reply '((stopReason . "end_turn")))
    (send "session/prompt" "Implement the ETL iteration")
    (reply '((stopReason . "end_turn")))
    (should-not nyy/omp-auto-title-timer)
    (should (= (rename-count) 0))))

(ert-deftest nyy/omp-auto-title-defers-to-user-work-and-native-titles ()
  (nyy/omp-auto-title-test
    (send "session/new")
    (reply '((sessionId . "auto-session")))
    (send "session/prompt" "Implement the ETL iteration")
    (reply '((stopReason . "end_turn")))
    (setq busy t)
    (fire)
    (should (= (rename-count) 0))
    (should (equal nyy/omp-auto-title-session "auto-session"))
    (setq busy nil)
    (send "session/prompt" "Use the existing importer")
    (reply '((stopReason . "end_turn")))
    (nyy/omp-auto-title-notification
     :state agent-shell--state
     :acp-notification
     '((method . "session/update")
       (params . ((sessionId . "auto-session")
                  (update . ((sessionUpdate . "session_info_update")
                             (title . "Native title")))))))
    (fire)
    (should (= (rename-count) 0))
    (should-not nyy/omp-auto-title-session)))

(ert-deftest nyy/session-manager-native-title-wins-without-renaming-the-buffer ()
  (nyy/session-tool-test
    (with-current-buffer shell
      (setq-local nyy/omp-native-title '("test-session-id" . "Persisted ETL title"))
      (setq-local shell-maker--buffer-name-override "[OMP] Old buffer label"))
    (let* ((entries (nyy/agent-shell-manager-session-titles
                     (list (list shell ["Original" "Ready" "-" "-" "-" "~/"]))))
           (title (aref (cadar entries) 0)))
      (should (equal title "Persisted ETL title"))
      (should (equal (buffer-name shell) "test session tool")))))

(provide 'nyy-agent-shell-session-tests)
;;; nyy-agent-shell-session-tests.el ends here
