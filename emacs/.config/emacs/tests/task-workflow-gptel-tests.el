;;; task-workflow-gptel-tests.el --- Draft transport behavior -*- lexical-binding: t; -*-
(require 'ert)
(require 'task-workflow-gptel)

(defvar task-workflow-gptel-test--requests nil)

(defmacro task-workflow-gptel-test-with-root (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "task-drafting-test-" t))
          (gptel--known-backends nil)
          (task-workflow-gptel-backend
           (gptel-make-openai-oauth "Task drafting test"
                                   :header #'task-workflow-gptel--oauth-header))
          (task-workflow-gptel-model 'gpt-5.5)
          (gptel--openai-oauth-token-file (expand-file-name "no-token" root))
          (task-workflow-gptel-test--requests nil))
     (unwind-protect
         (progn ,@body)
       (dolist (request task-workflow-gptel-test--requests)
         (task-workflow-gptel-cancel request))
       (delete-directory root t))))

(defun task-workflow-gptel-test-prepare (root &optional callback owner)
  (let ((request (task-workflow-gptel-draft
                  "Fix the missing backup notification." (or callback #'ignore)
                  :root root :owner owner :dry-run t)))
    (push request task-workflow-gptel-test--requests)
    request))

(defconst task-workflow-gptel-test--proposal
  "{\"title\":\"Restore backup notifications\",\"outcome\":\"Failures are visible\",\"description\":\"Investigate missing backup notifications.\",\"completion_criteria\":[\"A failed backup produces a notification\"],\"next_action\":\"Inspect the backup logs\",\"questions\":[]}")

(ert-deftest task-workflow-gptel-isolates-source-history-tools-and-transforms ()
  (task-workflow-gptel-test-with-root
    (let ((gptel-tools '(unrelated-tool)) (gptel-use-tools t)
          (gptel-context '(unrelated-private-context)) (gptel-use-context t)
          (gptel-prompt-transform-functions (list (lambda () (error "Leaked transform"))))
          (gptel--request-params '(:input ["unrelated history"]))
          (gptel-system-prompt "Unrelated system prompt")
          (gptel-backend nil) (gptel-model 'unrelated-model))
      (with-temp-buffer
        (insert "Unrelated confidential conversation")
        (let* ((request (task-workflow-gptel-test-prepare root))
               (data (plist-get (gptel-fsm-info
                                 (task-workflow-gptel-request-fsm request)) :data)))
          (should (equal (plist-get data :input)
                         [(:role "user" :content "Fix the missing backup notification.")]))
          (should (equal (plist-get data :instructions) task-workflow-gptel--system))
          (should-not (plist-member data :tools))
          (should (eq (plist-get data :store) :json-false))
          (should (equal (plist-get (plist-get (plist-get data :text) :format) :schema)
                         task-workflow-gptel--schema))
          (should (equal (buffer-string) "Unrelated confidential conversation"))
          (should (equal gptel-tools '(unrelated-tool)))
          (should (eq gptel-model 'unrelated-model)))))))

(ert-deftest task-workflow-gptel-rejects-payload-overrides-before-sending ()
  (task-workflow-gptel-test-with-root
    (dolist (params '((:tools [(:type "web_search")])
                      (:input [(:role "user" :content "other customer")])
                      (:store t) (:previous_response_id "other-conversation")))
      (setf (gptel-backend-request-params task-workflow-gptel-backend) params)
      (should-error (task-workflow-gptel-test-prepare root)))))

(ert-deftest task-workflow-gptel-missing-auth-never-starts-login ()
  (task-workflow-gptel-test-with-root
    (cl-letf (((symbol-function 'gptel-openai-oauth-login)
               (lambda (&rest _) (ert-fail "Implicit login"))))
      (should-error (task-workflow-gptel-draft "Investigate backups" #'ignore :root root)
                    :type 'user-error)
      (should-not (file-exists-p gptel--openai-oauth-token-file)))))

(ert-deftest task-workflow-gptel-rejects-model-fallback ()
  (task-workflow-gptel-test-with-root
    (let ((task-workflow-gptel-model 'unavailable-model))
      (should-error (task-workflow-gptel-test-prepare root) :type 'user-error))))

(ert-deftest task-workflow-gptel-assembles-stream-and-delivers-once ()
  (task-workflow-gptel-test-with-root
    (let* ((results nil)
           (request (task-workflow-gptel-test-prepare
                     root (lambda (request) (push (task-workflow-gptel-request-result request)
                                                  results))))
           (boundary 63))
      (task-workflow-gptel--receive request '(reasoning . "Not proposal content") nil)
      (task-workflow-gptel--receive request (substring task-workflow-gptel-test--proposal 0 boundary) nil)
      (task-workflow-gptel--receive request (substring task-workflow-gptel-test--proposal boundary) nil)
      (task-workflow-gptel--receive request t nil)
      (task-workflow-gptel--receive request nil '(:error "late failure"))
      (task-workflow-gptel-cancel request)
      (should (eq (task-workflow-gptel-request-state request) 'done))
      (should (equal (plist-get (car results) :title) "Restore backup notifications"))
      (should (equal (plist-get (car results) :completion_criteria)
                     ["A failed backup produces a notification"]))
      (should (= (length results) 1))
      (should-not (buffer-live-p (task-workflow-gptel-request-buffer request))))))

(ert-deftest task-workflow-gptel-rejects-invalid-proposals-and-stream-errors ()
  (task-workflow-gptel-test-with-root
    (dolist (text '("not JSON" "{\"title\":\"Missing fields\"}"
                    "{\"title\":\"Bad\",\"outcome\":\"\",\"description\":\"\",\"completion_criteria\":null,\"next_action\":\"\",\"questions\":[]}"))
      (let ((request (task-workflow-gptel-test-prepare root)))
        (task-workflow-gptel--receive request text nil)
        (task-workflow-gptel--receive request t nil)
        (should (eq (task-workflow-gptel-request-state request) 'failed))
        (should-not (task-workflow-gptel-request-result request))))
    (let ((request (task-workflow-gptel-test-prepare root)))
      (task-workflow-gptel--receive request task-workflow-gptel-test--proposal nil)
      (task-workflow-gptel--receive request t '(:error "provider error despite HTTP 200"))
      (should (eq (task-workflow-gptel-request-state request) 'failed)))))

(ert-deftest task-workflow-gptel-owner-death-discards-late-results ()
  (task-workflow-gptel-test-with-root
    (let* ((owner (generate-new-buffer " *draft-owner-test*"))
           (called nil)
           (request (task-workflow-gptel-test-prepare root (lambda (_) (setq called t)) owner)))
      (kill-buffer owner)
      (task-workflow-gptel--receive request task-workflow-gptel-test--proposal nil)
      (task-workflow-gptel--receive request t nil)
      (should (eq (task-workflow-gptel-request-state request) 'cancelled))
      (should-not called)
      (should-not (buffer-live-p (task-workflow-gptel-request-buffer request))))))

(ert-deftest task-workflow-gptel-cancellation-leaves-other-processes-running ()
  (task-workflow-gptel-test-with-root
    (let* ((request (task-workflow-gptel-test-prepare root))
           (other (task-workflow-gptel-test-prepare root))
           (process (make-pipe-process :name "task-cancel-test" :noquery t))
           (other-process (make-pipe-process :name "other-task-test" :noquery t))
           (gptel--request-alist
            (list (cons process (cons (task-workflow-gptel-request-fsm request)
                                      (lambda () (delete-process process))))
                  (cons other-process (cons (task-workflow-gptel-request-fsm other)
                                            (lambda () (delete-process other-process)))))))
      (unwind-protect
          (progn
            (task-workflow-gptel-cancel request)
            (should-not (process-live-p process))
            (should (process-live-p other-process))
            (should (eq (task-workflow-gptel-request-state other) 'prepared)))
        (delete-process other-process)))))

(ert-deftest task-workflow-gptel-reports-provider-code-without-echoing-source ()
  (task-workflow-gptel-test-with-root
    (let ((request (task-workflow-gptel-test-prepare root)))
      (task-workflow-gptel--receive
       request nil '(:http-status "400"
                     :error (:code "model_not_found" :message "Confidential source material")))
      (should (eq (task-workflow-gptel-request-state request) 'failed))
      (should (string-match-p "HTTP 400.*model_not_found"
                              (task-workflow-gptel-request-error request)))
      (should-not (string-match-p "Confidential"
                                  (task-workflow-gptel-request-error request)))
      (should-not (task-workflow-gptel-request-fsm request)))))

(ert-deftest task-workflow-gptel-debug-settings-do-not-log-transport-data ()
  (task-workflow-gptel-test-with-root
    (let* ((gptel-log-level 'debug)
           (gptel--log-buffer-name "*task-drafting-privacy-test-log*")
           (gptel-proxy "")
           (process-environment (cons "NO_PROXY=127.0.0.1" process-environment))
           (clients nil)
           (server
            (make-network-process
             :name "task-drafting-privacy-test" :server t :host "127.0.0.1"
             :service t :family 'ipv4 :noquery t
             :log (lambda (_server client _message) (push client clients))
             :filter
             (lambda (process chunk)
               (let* ((input (concat (process-get process 'input) chunk))
                      (case-fold-search t)
                      (body-start (string-match "\r\n\r\n" input))
                      (size (and (string-match "Content-Length: *\\([0-9]+\\)" input)
                                 (string-to-number (match-string 1 input)))))
                 (process-put process 'input input)
                 (when (and body-start size (>= (length input) (+ body-start 4 size)))
                   (process-send-string
                    process
                    (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                            "Connection: close\r\n\r\nevent: response.output_text.delta\ndata: "
                            (json-serialize
                             (list :type "response.output_text.delta"
                                   :delta task-workflow-gptel-test--proposal))
                            "\n\ndata: [DONE]\n\n"))
                   (delete-process process))))))
           (task-workflow-gptel-backend
            (gptel-make-openai-responses
             "Local privacy test" :protocol "http"
             :host (format "127.0.0.1:%s" (process-contact server :service))
             :endpoint "/" :key "synthetic-private-key" :stream t :models '(gpt-5.5))))
      (unwind-protect
          (with-temp-buffer
            ;; Exercise a caller with a local logging setting as well as the
            ;; global debug setting.  The real curl filter/sentinel must run.
            (setq-local gptel-log-level 'debug)
            (let ((request (task-workflow-gptel-draft
                            "Private source must not reach gptel logs."
                            #'ignore :root root))
                  (deadline (+ (float-time) 5)))
              (push request task-workflow-gptel-test--requests)
              (while (and (eq (task-workflow-gptel-request-state request) 'running)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (should (eq (task-workflow-gptel-request-state request) 'done))
              (should-not (get-buffer gptel--log-buffer-name))
              (should (eq gptel-log-level 'debug))))
        (delete-process server)
        (dolist (client clients) (when (process-live-p client) (delete-process client)))
        (when (get-buffer gptel--log-buffer-name)
          (kill-buffer gptel--log-buffer-name))))))

(provide 'task-workflow-gptel-tests)
;;; task-workflow-gptel-tests.el ends here
