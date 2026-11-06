;;; task-workflow-draft-tests.el --- Reviewed proposal behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-element)
(require 'task-workflow-draft)
(require 'task-workflow-draft-test-helpers
         (expand-file-name "task-workflow-draft-test-helpers"
                           (file-name-directory (or load-file-name buffer-file-name))))


(ert-deftest task-workflow-draft-opens-editable-context-without-network-or-storage ()
  (task-workflow-draft-test-with-root
    (let ((buffer (task-workflow-draft "Rough context\nwith a second line")))
      (with-current-buffer buffer
        (should (eq task-workflow-draft-state 'captured))
        (should-not task-workflow-draft-test--calls)
        (should-not buffer-file-name)
        (should-not buffer-auto-save-file-name)
        (should-not buffer-offer-save)
        (should-not auto-save-default)
        (should-not make-backup-files)
        (should-not create-lockfiles)
        (should (equal (task-workflow-draft-field-value 'context)
                       "Rough context\nwith a second line"))
        (goto-char (cdr (task-workflow-draft-field-bounds 'context)))
        (insert "\nMore live context")
        (should (string-suffix-p "\nMore live context"
                                 (task-workflow-draft-field-value 'context)))
        (should (eq (key-binding (kbd "C-c C-g")) #'task-workflow-draft-generate))
        (should (eq (key-binding (kbd "C-c C-c")) #'task-workflow-draft-accept))
        (should (eq (key-binding (kbd "C-c C-k")) #'task-workflow-draft-cancel))
        (should (string-match-p "generate/revise" (task-workflow-draft--header)))
        (should-error (task-workflow-draft-accept) :type 'user-error)
        (should-not (task-workflow-entry-id
                     (task-workflow-draft-session-entry task-workflow-draft-session)))))
    (should-not (file-exists-p (expand-file-name "tasks" root)))))

(ert-deftest task-workflow-draft-freezes-source-and-owning-provider-at-creation ()
  (task-workflow-draft-test-with-root
    (let* ((owner (task-workflow-draft "Initial source"))
           (backend task-workflow-gptel-backend)
           (other-root (make-temp-file "task-proposal-other-" t)))
      (unwind-protect
          (with-current-buffer owner
            (task-workflow-draft-set-field 'context "Edited source\nexact second line")
            (task-workflow-draft-set-field 'intent "Investigate, do not implement")
            (let ((task-workflow-personal-root other-root)
                  (task-workflow-gptel-backend nil)
                  (task-workflow-gptel-model 'wrong-model))
              (task-workflow-draft-generate))
            (let* ((call (car task-workflow-draft-test--calls))
                   (input (task-workflow-draft-session-input task-workflow-draft-session))
                   (source (car (task-workflow-draft-input-sources input))))
              (should (equal (task-workflow-draft-test-call-root call) (file-name-as-directory root)))
              (should (eq (task-workflow-draft-test-call-backend call) backend))
              (should (eq (task-workflow-draft-test-call-model call) 'gpt-6.1-sol))
              (should (equal (plist-get source :description) "Edited source\nexact second line"))
              (should (equal (plist-get source :kind) "description"))
              (should (stringp (plist-get source :captured_at)))
              (should (equal (task-workflow-draft-input-intent input) "Investigate, do not implement"))
              (should (string-match-p (regexp-quote "Edited source\\nexact second line")
                                      (task-workflow-draft-test-call-prompt call)))
              (should-not (task-workflow-entry-id
                           (task-workflow-draft-session-entry task-workflow-draft-session)))
              (should-error (task-workflow-draft-set-field 'context "Changed later") :type 'user-error)
              (goto-char (car (task-workflow-draft-field-bounds 'context)))
              (should-error (insert "Changed later"))
              (should (equal (plist-get source :description) (task-workflow-draft-field-value 'context)))))
        (delete-directory other-root t)))))

(ert-deftest task-workflow-draft-delayed-revisions-never-replace-manual-edits ()
  (task-workflow-draft-test-with-root
    (let* ((original (task-workflow-draft-test-ready))
           (session (buffer-local-value 'task-workflow-draft-session original))
           before call)
      (with-current-buffer original
        (task-workflow-draft-set-field 'description "Manual description before revision")
        (task-workflow-draft-set-field 'feedback "Make the outcome concrete\nKeep the caveat")
        (setq before (buffer-string))
        (task-workflow-draft-generate)
        (setq call (car task-workflow-draft-test--calls))
        (should (string-match-p "Manual description before revision"
                                (task-workflow-draft-test-call-prompt call)))
        (should (string-match-p "Keep the caveat" (task-workflow-draft-test-call-prompt call)))
        (should-error (task-workflow-draft-generate) :type 'user-error)
        (should-error (task-workflow-draft-accept) :type 'user-error)
        (should (equal (buffer-string) before))
        ;; Simulate edits while the provider is delayed, in the real visible UI.
        (goto-char (cdr (task-workflow-draft-field-bounds 'description)))
        (insert "\nImportant edit typed during the request")
        (setq before (buffer-string)))
      (task-workflow-draft-test-complete call)
      (let ((candidate (car (task-workflow-draft-session-buffers session))))
        (should-not (eq candidate original))
        (with-current-buffer original
          (should (equal (buffer-string) before))
          (should (eq task-workflow-draft-state 'ready)))
        (with-current-buffer candidate
          (should (eq task-workflow-draft-state 'ready))
          (should (eq task-workflow-draft-session session))
          (should (equal (task-workflow-draft-field-value 'description)
                         "Investigate the missing notification."))
          (should-not buffer-auto-save-file-name)
          (should-not buffer-file-name))))))

(ert-deftest task-workflow-draft-cancel-is-owned-and-late-results-do-not-resurrect ()
  (task-workflow-draft-test-with-root
    (let* ((first (task-workflow-draft "First context"))
           (first-session (buffer-local-value 'task-workflow-draft-session first))
           (second (task-workflow-draft "Second context"))
           first-call second-call)
      (with-current-buffer first (task-workflow-draft-generate)
                           (setq first-call (car task-workflow-draft-test--calls)))
      (with-current-buffer second (task-workflow-draft-generate)
                           (setq second-call (car task-workflow-draft-test--calls)))
      (with-current-buffer first (task-workflow-draft-cancel))
      (should (equal task-workflow-draft-test--cancelled
                     (list (task-workflow-draft-test-call-request first-call))))
      (task-workflow-draft-test-complete first-call)
      (should (= (length (task-workflow-draft-session-buffers first-session)) 1))
      (with-current-buffer first
        (should (eq task-workflow-draft-state 'cancelled))
        (should (equal (task-workflow-draft-field-value 'context) "First context"))
        (should-error (task-workflow-draft-generate) :type 'user-error)
        (should-error (task-workflow-draft-accept) :type 'user-error))
      (task-workflow-draft-test-complete second-call)
      (with-current-buffer second
        (should (= (length (task-workflow-draft-session-buffers task-workflow-draft-session)) 2)))
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-draft-owner-death-and-mismatched-handles-reject-delivery ()
  (task-workflow-draft-test-with-root
    (let* ((owner (task-workflow-draft "Disposable context"))
           (session (buffer-local-value 'task-workflow-draft-session owner))
           call)
      (with-current-buffer owner
        (task-workflow-draft-generate)
        (setq call (car task-workflow-draft-test--calls))
        (funcall (task-workflow-draft-test-call-callback call)
                 (task-workflow-gptel--make-request :state 'done :result task-workflow-draft-test--proposal))
        (should (eq task-workflow-draft-state 'generating))
        (should (= (length (task-workflow-draft-session-buffers session)) 1)))
      (kill-buffer owner)
      (should (memq (task-workflow-draft-test-call-request call) task-workflow-draft-test--cancelled))
      (task-workflow-draft-test-complete call)
      (should-not (task-workflow-draft-session-buffers session))
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-draft-failed-and-malformed-generations-preserve-editable-work ()
  (task-workflow-draft-test-with-root
    (let ((owner (task-workflow-draft "Preserve this source")) before)
      (with-current-buffer owner
        (task-workflow-draft-set-field 'feedback "Preserve this feedback")
        (setq before (buffer-string))
        (task-workflow-draft-generate))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls)
                                        'failed nil "Authentication expired")
      (with-current-buffer owner
        (should (eq task-workflow-draft-state 'failed))
        (should (string-match-p "Authentication expired" task-workflow-draft-error))
        (should (equal (buffer-string) before))
        (should-error (task-workflow-draft-accept) :type 'user-error)
        (task-workflow-draft-generate))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls) 'done '(:title "Incomplete"))
      (with-current-buffer owner
        (should (eq task-workflow-draft-state 'failed))
        (should (string-match-p "invalid task proposal" task-workflow-draft-error))
        (should (equal (buffer-string) before))
        (should (= (length (task-workflow-draft-session-buffers task-workflow-draft-session)) 1))
        (task-workflow-draft-generate))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-draft-setup-and-render-errors-leave-source-and-edits-intact ()
  (task-workflow-draft-test-with-root
    (let ((owner (task-workflow-draft-test-ready)) before)
      (with-current-buffer owner
        (task-workflow-draft-set-field 'outcome "Hand-edited outcome")
        (setq before (buffer-string))
        (cl-letf (((symbol-function 'task-workflow-gptel-draft)
                   (lambda (&rest _) (user-error "Run task-workflow-gptel-login"))))
          (should-not (task-workflow-draft-generate "Try this feedback")))
        (should (eq task-workflow-draft-state 'failed))
        (should (equal (buffer-string) before))
        (should-not task-workflow-draft--token)
        (task-workflow-draft-generate))
      (cl-letf (((symbol-function 'task-workflow-draft--new-buffer)
                 (lambda (&rest _) (error "Synthetic renderer failure"))))
        (task-workflow-draft-test-complete (car task-workflow-draft-test--calls)))
      (with-current-buffer owner
        (should (eq task-workflow-draft-state 'failed))
        (should (equal (buffer-string) before))
        (should (string-match-p "Synthetic renderer failure" task-workflow-draft-error))
        (should (equal task-workflow-draft--response task-workflow-draft-test--proposal))
        (should (equal (task-workflow-draft-field-value 'outcome) "Hand-edited outcome"))))))

(ert-deftest task-workflow-draft-acceptance-is-shared-idempotent-and-does-not-focus ()
  (task-workflow-draft-test-with-root
    (let* ((first (task-workflow-draft-test-ready))
           (session (buffer-local-value 'task-workflow-draft-session first))
           (source (cadr (task-workflow-draft-session-buffers session)))
           second pending record)
      (with-current-buffer first
        (task-workflow-draft-generate "Make another candidate"))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
      (setq second (car (task-workflow-draft-session-buffers session)))
      (with-current-buffer second (task-workflow-draft-set-field 'title "Sibling alternate title"))
      (with-current-buffer source
        (task-workflow-draft-generate)
        (setq pending (car task-workflow-draft-test--calls)))
      (with-current-buffer first
        (task-workflow-draft-set-field 'description "Reviewed manual description")
        (setq record (task-workflow-draft-accept)))
      (set-buffer (window-buffer (selected-window)))
      (should (equal task-workflow-focused-id "unrelated-focus"))
      (should (equal (plist-get record :state) "INBOX"))
      (should (equal (plist-get record :title) "Restore backup notifications"))
      (should (equal (org-entry-get nil "ID") (plist-get record :id)))
      (should-not (buffer-modified-p))
      (should (memq (task-workflow-draft-test-call-request pending) task-workflow-draft-test--cancelled))
      (org-end-of-subtree t t)
      (insert "\nUser notes added after accepting.\n")
      (save-buffer)
      (let ((saved (buffer-string))
            (count (length (task-workflow-draft-session-buffers session))))
        (task-workflow-draft-test-complete pending)
        (should (= count (length (task-workflow-draft-session-buffers session))))
        (with-current-buffer second
          (should (eq task-workflow-draft-state 'accepted))
          (should (equal (task-workflow-draft-accept) record)))
        (should (equal (buffer-string) saved))
        (with-current-buffer first (should (equal (task-workflow-draft-accept) record)))
        (should (equal (buffer-string) saved)))
      (should (= (length (task-workflow--collect t)) 1))
      (should (equal (task-workflow-entry-id (task-workflow-draft-session-entry session))
                     (plist-get record :id))))))

(ert-deftest task-workflow-draft-save-failure-retries-the-same-allocated-identity ()
  (task-workflow-draft-test-with-root
    (let* ((candidate (task-workflow-draft-test-ready))
           (session (buffer-local-value 'task-workflow-draft-session candidate))
           id before)
      (with-current-buffer candidate
        (task-workflow-draft-set-field 'description "Must survive failed persistence")
        (setq before (buffer-string))
        (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (error "Disk unavailable"))))
          (should-error (task-workflow-draft-accept) :type 'user-error))
        (setq id (task-workflow-entry-id (task-workflow-draft-session-entry session)))
        (should (stringp id))
        (should (eq task-workflow-draft-state 'failed))
        (should (equal before (buffer-string)))
        (should-not (task-workflow-draft-session-record session))
        (should (string-match-p "Disk unavailable" task-workflow-draft-error)))
      (should-not (task-workflow--collect t))
      (let ((other-root (make-temp-file "task-proposal-other-" t)))
        (unwind-protect
            (let ((task-workflow-personal-root other-root))
              (with-current-buffer candidate
                (should (equal (plist-get (task-workflow-draft-accept) :id) id))))
          (delete-directory other-root t)))
      (set-buffer (window-buffer (selected-window)))
      (should (file-in-directory-p buffer-file-name root))
      (should (= (length (task-workflow--collect t)) 1))
      (should (equal (org-entry-get nil "ID") id)))))

(ert-deftest task-workflow-draft-untrusted-org-remains-inert-and-source-json-is-exact ()
  (task-workflow-draft-test-with-root
    (let* ((payload (concat ",* Literal comma heading\n#+end_example\n* TODO Injected\n"
                            ":PROPERTIES:\n:TASK: t\n:ID: model-id\n:END:\n"
                            "#+begin_src emacs-lisp\n(setq task-workflow-draft-test--executed t)\n#+end_src\n"
                            "[[elisp:(setq task-workflow-draft-test--executed t)][Run]]\n"
                            "# Local " "Variables:\n# eval: (setq task-workflow-draft-test--executed t)\n# End:\n"))
           (task-workflow-draft-test--executed nil)
           (org-confirm-babel-evaluate nil)
           (candidate (task-workflow-draft-test-ready payload)))
      (with-current-buffer candidate
        (task-workflow-draft-set-field 'description payload)
        ;; Ordinary insertion, not only the setter, must safely quote delimiters.
        (goto-char (cdr (task-workflow-draft-field-bounds 'outcome)))
        (insert "\n#+end_example\n* TODO Pasted heading\n#+begin_src emacs-lisp\n")
        (should (equal (task-workflow-draft-field-value 'description) payload))
        (should (equal (task-workflow-draft-field-value 'context) payload))
        (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
        (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
        (task-workflow-draft-accept))
      (set-buffer (window-buffer (selected-window)))
      (should (= (length (task-workflow--collect t)) 1))
      (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
      (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
      (let ((case-fold-search t))
        (should-not (string-match-p (concat "Local " "Variables:") (buffer-string))))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Capture source$")
        (forward-line 2)
        (let* ((json (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (data (json-parse-string json :object-type 'plist :array-type 'list)))
          (should (equal (plist-get (car (plist-get data :sources)) :description) payload))
          (should (equal (plist-get data :root) (file-name-as-directory root)))
          (should (equal (plist-get data :provider) "Proposal test"))))
      (let ((enable-local-variables :all) (enable-local-eval t))
        (hack-local-variables))
      (should-not task-workflow-draft-test--executed))))

(ert-deftest task-workflow-draft-boundary-and-title-errors-never-discard-edits-or-save ()
  (task-workflow-draft-test-with-root
    (let ((candidate (task-workflow-draft-test-ready)))
      (with-current-buffer candidate
        (task-workflow-draft-set-field 'description "My irreplaceable edit")
        (task-workflow-draft-set-field 'title "Unsafe\n* TODO Other task")
        (should-error (task-workflow-draft-accept) :type 'user-error)
        (should (equal (task-workflow-draft-field-value 'description) "My irreplaceable edit"))
        (task-workflow-draft-set-field 'title "Safe title")
        (pcase-let ((`(,begin . ,_end) (task-workflow-draft-field-bounds 'description)))
          (should-error (delete-region (1- begin) begin)))
        ;; Even an external command bypassing read-only and hooks cannot cause
        ;; extraction to silently ignore modified boundary text.
        (let ((inhibit-read-only t) (inhibit-modification-hooks t))
          (goto-char (point-min))
          (insert "Outside text that must not be silently lost\n"))
        (should-error (task-workflow-draft-accept) :type 'user-error)
        (should-error (task-workflow-draft-generate) :type 'user-error)
        (should (string-match-p "Outside text that must not be silently lost" (buffer-string)))
        (should (equal (task-workflow-draft-field-value 'description) "My irreplaceable edit")))
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-draft-entire-flow-reuses-one-pane ()
  (task-workflow-draft-test-with-root
    (let ((unrelated (generate-new-buffer " *proposal window unrelated*"))
          (switch-to-buffer-obey-display-actions t)
          (display-buffer-alist '((".*" display-buffer-pop-up-window)))
          (split-height-threshold 0)
          (split-width-threshold nil))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((pane (selected-window))
                   (other (split-window-right))
                   (windows (window-list)))
              (set-window-buffer other unrelated)
              (let* ((origin (task-workflow-draft "Plan backup notifications"))
                     (session task-workflow-draft-session))
                (should (eq (selected-window) pane))
                (should (eq (window-buffer pane) origin))
                (should (equal (window-list) windows))
                (task-workflow-draft-generate)
                (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
                (let ((first (car (task-workflow-draft-session-buffers session))))
                  (should (eq (window-buffer pane) first))
                  (task-workflow-draft-set-field 'description "Keep my reviewed description")
                  (task-workflow-draft-generate "Make another candidate")
                  (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
                  (let* ((second (car (task-workflow-draft-session-buffers session)))
                         (record (task-workflow-draft-accept)))
                    (should (eq (selected-window) pane))
                    (should (equal (buffer-file-name (window-buffer pane))
                                   (plist-get record :file)))
                    (should (equal (org-entry-get nil "ID") (plist-get record :id)))
                    (should (equal (window-list) windows))
                    (should (eq (window-buffer other) unrelated))
                    (should (buffer-live-p origin))
                    (should (buffer-live-p second))
                    (with-current-buffer first
                      (should (equal (task-workflow-draft-field-value 'description)
                                     "Keep my reviewed description"))))))))
        (kill-buffer unrelated)))))

(ert-deftest task-workflow-draft-completion-reuses-unselected-owner-pane ()
  (task-workflow-draft-test-with-root
    (let* ((owner (task-workflow-draft "Visible owner"))
           (session task-workflow-draft-session)
           (pane (selected-window))
           (elsewhere (generate-new-buffer " *proposal active elsewhere*"))
           other)
      (unwind-protect
          (progn
            (delete-other-windows)
            (setq other (split-window-right))
            (set-window-buffer other elsewhere)
            (task-workflow-draft-generate)
            (select-window other)
            (let ((windows (window-list)))
              (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
              (should (eq (selected-window) other))
              (should (eq (window-buffer other) elsewhere))
              (should (eq (window-buffer pane)
                          (car (task-workflow-draft-session-buffers session))))
              (should (equal (window-list) windows))
              (should (buffer-live-p owner))))
        (kill-buffer elsewhere)))))

(ert-deftest task-workflow-draft-completion-respects-dedicated-owner-pane ()
  (task-workflow-draft-test-with-root
    (let* ((owner (task-workflow-draft "Dedicated owner"))
           (session task-workflow-draft-session)
           (pane (selected-window))
           (windows (window-list)))
      (unwind-protect
          (progn
            (set-window-dedicated-p pane t)
            (task-workflow-draft-generate)
            (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
            (should (eq (window-buffer pane) owner))
            (should (eq (selected-window) pane))
            (should (equal (window-list) windows))
            (should-not (get-buffer-window
                         (car (task-workflow-draft-session-buffers session)) t)))
        (set-window-dedicated-p pane nil)))))

(ert-deftest task-workflow-draft-completion-does-not-select-an-unrelated-window ()
  (task-workflow-draft-test-with-root
    (let* ((owner (task-workflow-draft "A request whose owner becomes unselected"))
           (session (buffer-local-value 'task-workflow-draft-session owner))
           (elsewhere (generate-new-buffer " *proposal unrelated*"))
           (windows (window-list)))
      (unwind-protect
          (progn
            (with-current-buffer owner (task-workflow-draft-generate))
            (switch-to-buffer elsewhere)
            (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
            (should (eq (window-buffer (selected-window)) elsewhere))
            (should (equal (window-list) windows))
            (should-not (get-buffer-window
                         (car (task-workflow-draft-session-buffers session)) t))
            (should (= (length (task-workflow-draft-session-buffers session)) 2)))
        (kill-buffer elsewhere)))))

(ert-deftest task-workflow-draft-selection-freezes-context-before-generate-not-intent ()
  (task-workflow-draft-test-with-root
    (let* ((text "Unsaved selected context\nwith exact trailing newline\n")
           (metadata (concat "selection-buffer\n#+end_example\n* TODO Not a task\n"
                             "[[elisp:(error \"must not run\")]]\n# Local " "Variables:"))
           (source (list :kind "selection" :description text
                         :captured_at "2026-10-29T10:11:12+0200"
                         :buffer metadata :major_mode "text-mode" :modified :json-false))
           (owner (task-workflow-draft text :sources (list source))))
      (with-current-buffer owner
        (should (eq task-workflow-draft-state 'captured))
        (should-not task-workflow-draft-test--calls)
        (should-not (task-workflow-draft-session-input task-workflow-draft-session))
        (should-not (task-workflow-entry-id
                     (task-workflow-draft-session-entry task-workflow-draft-session)))
        (should (equal (task-workflow-draft-field-value 'context) text))
        (should-error (task-workflow-draft-set-field 'context "Replacement") :type 'user-error)
        (goto-char (car (task-workflow-draft-field-bounds 'context)))
        (should-error (insert "Replacement"))
        (task-workflow-draft-set-field 'intent "First intent")
        (task-workflow-draft-set-field 'intent "Final intent before generating")
        ;; Inspect the real protected provenance preview, not just its formatter.
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Source provenance (application-owned)$")
        (forward-line 2)
        (let* ((begin (line-beginning-position))
               (end (line-end-position))
               (json (task-workflow-draft--raw begin end))
               (preview (car (json-parse-string json :object-type 'plist :array-type 'list
                                               :false-object :json-false))))
          (should (equal (plist-get preview :buffer) metadata))
          (should (eq (plist-get preview :modified) :json-false))
          (should-not (plist-member preview :description))
          (goto-char begin)
          (should-error (insert "Changed provenance")))
        ;; Rejected edits must not disable quoting for later writable fields.
        (goto-char (cdr (task-workflow-draft-field-bounds 'feedback)))
        (insert "\n#+end_example\n#+begin_src emacs-lisp\n(error \"inert\")\n#+end_src")
        (should (string-suffix-p
                 "#+begin_src emacs-lisp\n(error \"inert\")\n#+end_src"
                 (task-workflow-draft-field-value 'feedback)))
        (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
        (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
        (let ((case-fold-search t))
          (should-not (string-match-p (concat "Local " "Variables:") (buffer-string))))
        (should-not (file-exists-p (expand-file-name "tasks" root)))
        (task-workflow-draft-generate)
        (should-not task-workflow-draft--pending-sources)
        (should (equal (task-workflow-draft-input-intent
                        (task-workflow-draft-session-input task-workflow-draft-session))
                       "Final intent before generating"))
        (should-error (task-workflow-draft-set-field 'intent "Too late") :type 'user-error)))))

(ert-deftest task-workflow-draft-selection-snapshot-owns-all-caller-data ()
  (task-workflow-draft-test-with-root
    (let* ((text (copy-sequence "Selected unsaved text"))
           (name (copy-sequence "original-buffer"))
           (nested (copy-sequence "nested value"))
           (table (make-hash-table :test #'equal))
           (vector (vector (list :value nested)))
           (source (list :kind (copy-sequence "selection") :description text
                         :captured_at (copy-sequence "2026-10-29T10:11:12+0200")
                         :buffer name :major_mode "text-mode" :modified t
                         :extra vector :details table))
           (sources (list source))
           expected owner)
      (puthash "name" (copy-sequence "original hash value") table)
      (setq expected (json-serialize (vconcat sources) :false-object :json-false)
            owner (task-workflow-draft text :sources sources))
      ;; Mutate every mutable shape after intake, before the first generation.
      (aset text 0 ?X)
      (aset name 0 ?X)
      (aset nested 0 ?X)
      (aset (gethash "name" table) 0 ?X)
      (puthash "new" "external addition" table)
      (setcar (aref vector 0) :changed)
      (aset vector 0 "external replacement")
      (setcar source :changed)
      (setcar sources nil)
      (with-current-buffer owner
        (should (equal (task-workflow-draft-field-value 'context) "Selected unsaved text"))
        (task-workflow-draft-set-field 'intent "Keep the original snapshot")
        (task-workflow-draft-generate)
        (let* ((input (task-workflow-draft-session-input task-workflow-draft-session))
               (owned (task-workflow-draft-input-sources input)))
          (should (equal (json-serialize (vconcat owned) :false-object :json-false) expected))
          (should (equal (plist-get (car owned) :buffer) "original-buffer"))
          (should-not (string-match-p "external replacement"
                                      (task-workflow-draft-test-call-prompt
                                       (car task-workflow-draft-test--calls)))))))))

(ert-deftest task-workflow-draft-selection-provenance-survives-revisions-and-real-save ()
  (task-workflow-draft-test-with-root
    (let* ((payload (concat "#+end_example\n* TODO Injected\n:PROPERTIES:\n:ID: bogus\n:END:\n"
                            "#+begin_src emacs-lisp\n(error \"must not run\")\n#+end_src\n"
                            "[[elisp:(error \"must not run\")]]\n# Local " "Variables:\n# End:\n"))
           (source (list :kind "selection" :description payload
                         :captured_at "2026-10-29T10:11:12+0200"
                         :buffer "unsaved-source.el" :major_mode "emacs-lisp-mode"
                         :modified :json-false :file "/personal/checkout/unsaved-source.el"
                         :line_start 7 :line_end 18
                         :project_name "checkout" :project_root "/personal/checkout/"
                         :project_relative_file "unsaved-source.el"
                         :git_root "/personal/checkout/" :git_branch "topic/capture"
                         :git_revision "0123456789abcdef"))
           (owner (task-workflow-draft payload :sources (list source)))
           (session (buffer-local-value 'task-workflow-draft-session owner))
           input candidate)
      (with-current-buffer owner
        (task-workflow-draft-set-field 'intent "Explain the selected code")
        (task-workflow-draft-generate)
        (setq input (task-workflow-draft-session-input session)))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
      (setq candidate (car (task-workflow-draft-session-buffers session)))
      (with-current-buffer candidate
        (should (equal (task-workflow-draft-field-value 'context) payload))
        (should (equal (task-workflow-draft-input-sources input) (list source)))
        (should-error (task-workflow-draft-set-field 'context "Changed") :type 'user-error)
        (task-workflow-draft-generate "Narrow the next action"))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
      (dolist (call task-workflow-draft-test--calls)
        (let* ((prompt (task-workflow-draft-test-call-prompt call))
               (request (json-parse-string (substring prompt (1+ (string-match "\n" prompt)))
                                           :object-type 'plist :array-type 'list
                                           :false-object :json-false))
               (capture (plist-get request :capture)))
          (should (equal (plist-get capture :sources) (list source)))
          (should (equal (plist-get capture :intent) "Explain the selected code"))))
      (setq candidate (car (task-workflow-draft-session-buffers session)))
      (with-current-buffer candidate
        (should (eq (task-workflow-draft-session-input task-workflow-draft-session) input))
        (should (equal (task-workflow-draft-field-value 'context) payload))
        (should (equal (task-workflow-draft-field-value 'intent) "Explain the selected code"))
        (task-workflow-draft-set-field 'description payload)
        (task-workflow-draft-accept))
      (set-buffer (window-buffer (selected-window)))
      (should-not (buffer-modified-p))
      (should (= (length (task-workflow--collect t)) 1))
      (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
      (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
      (let ((case-fold-search t))
        (should-not (string-match-p (concat "Local " "Variables:") (buffer-string))))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Capture source$")
      (forward-line 2)
      (let* ((json (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (saved (json-parse-string json :object-type 'plist :array-type 'list
                                      :false-object :json-false)))
        (should (equal (plist-get saved :sources) (list source)))
        (should (equal (plist-get saved :intent) "Explain the selected code"))
        (should (equal (plist-get saved :capture) (task-workflow-draft-input-identity input)))
        (should (equal (plist-get saved :root) (file-name-as-directory root)))))))

(ert-deftest task-workflow-draft-unicode-context-passes-real-transport-isolation ()
  (let ((real-adapter (symbol-function 'task-workflow-gptel-draft))
        (real-cancel (symbol-function 'task-workflow-gptel-cancel)))
    (task-workflow-draft-test-with-root
      (let* ((text "Ääkköset, 日本語 ja 🚀 pitää säilyttää täsmälleen.")
             (source (list :kind "selection" :description text :buffer "notes.txt"))
             (owner (task-workflow-draft text :sources (list source))))
        (with-current-buffer owner
          (task-workflow-draft-set-field 'intent "Tutki häiriö")
          (task-workflow-draft-generate "Précisez l’étape suivante"))
        (let* ((prompt (task-workflow-draft-test-call-prompt
                        (car task-workflow-draft-test--calls)))
               ;; Exercise real gptel parsing and its exact-payload guard, not
               ;; just a JSON round trip in the frontend's mocked transport.
               (request (funcall real-adapter prompt #'ignore :root root :dry-run t)))
          (unwind-protect
              (let* ((data (plist-get (gptel-fsm-info
                                      (task-workflow-gptel-request-fsm request)) :data))
                     (content (plist-get (aref (plist-get data :input) 0) :content))
                     (envelope (json-parse-string
                                (substring content (1+ (string-search "\n" content)))
                                :object-type 'plist :array-type 'list))
                     (capture (plist-get envelope :capture)))
                (should (equal (plist-get capture :sources) (list source)))
                (should (equal (plist-get capture :intent) "Tutki häiriö"))
                (should (equal (plist-get envelope :feedback) "Précisez l’étape suivante"))
                (should-not (plist-member data :tools))
                (should-not (file-exists-p (expand-file-name "tasks" root))))
            (funcall real-cancel request)))))))

(ert-deftest task-workflow-draft-unicode-provenance-previews-and-reopens-exactly ()
  (task-workflow-draft-test-with-root
    (let* ((task-workflow-personal-root (expand-file-name "ää-検証/" root))
           (source (list :kind "selection" :description "Selected context"
                         :buffer "muistiinpanot-ä.org" :modified :json-false
                         :file "/personal/työ/日本語.org" :project_name "Työ 🚀"
                         :git_branch "aihe/ääkköset"))
           owner session record)
      (make-directory task-workflow-personal-root)
      (setq owner (task-workflow-draft "Selected context" :sources (list source))
            session (buffer-local-value 'task-workflow-draft-session owner))
      (with-current-buffer owner
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Destination and provider (application-owned)$")
        (forward-line 2)
        (let ((metadata (json-parse-string
                         (task-workflow-draft--raw (line-beginning-position) (line-end-position))
                         :object-type 'plist)))
          (should (equal (plist-get metadata :root) task-workflow-personal-root)))
        (re-search-forward "^\\*\\* Source provenance (application-owned)$")
        (forward-line 2)
        (let ((preview (car (json-parse-string
                            (task-workflow-draft--raw (line-beginning-position) (line-end-position))
                            :object-type 'plist :array-type 'list :false-object :json-false))))
          (should (equal (plist-get preview :file) (plist-get source :file)))
          (should (equal (plist-get preview :project_name) "Työ 🚀"))
          (should (eq (plist-get preview :modified) :json-false)))
        (task-workflow-draft-set-field 'intent "Selvitä syy")
        (task-workflow-draft-generate))
      (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
      (with-current-buffer (car (task-workflow-draft-session-buffers session))
        (task-workflow-draft-set-field 'title "Selvitä ääkköset 🚀")
        (setq record (task-workflow-draft-accept)))
      (let ((buffer (get-file-buffer (plist-get record :file))))
        (when buffer (kill-buffer buffer)))
      (with-current-buffer (find-file-noselect (plist-get record :file))
        (goto-char (plist-get record :position))
        (should (equal (org-get-heading t t t t) "Selvitä ääkköset 🚀"))
        (re-search-forward "^\\*\\* Capture source$")
        (forward-line 2)
        (let ((saved (json-parse-string
                      (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                      :object-type 'plist :array-type 'list :false-object :json-false)))
          (should (equal (plist-get saved :sources) (list source)))
          (should (equal (plist-get saved :root) task-workflow-personal-root))
          (should (equal (plist-get saved :intent) "Selvitä syy")))))))

(ert-deftest task-workflow-draft-selection-bypassed-protection-cannot-send-changed-source ()
  (task-workflow-draft-test-with-root
    (dolist (target '(context provenance))
      (let* ((text "Immutable selected context")
             (owner (task-workflow-draft
                     text :sources (list (list :kind "selection" :description text
                                               :captured_at "2026-10-29T10:11:12+0200"
                                               :buffer "original-buffer")))))
        (with-current-buffer owner
          (pcase target
            ('context (goto-char (car (task-workflow-draft-field-bounds 'context))))
            ('provenance
             (goto-char (point-min))
             (search-forward "original-buffer")
             (backward-char 1)))
          (let ((inhibit-read-only t) (inhibit-modification-hooks t))
            (delete-char 1)
            (insert "X"))
          (should-error (task-workflow-draft-generate) :type 'user-error)
          (should-not (task-workflow-draft-session-input task-workflow-draft-session))
          (should-not task-workflow-draft-test--calls)
          (should-not (file-exists-p (expand-file-name "tasks" root))))))))

(ert-deftest task-workflow-draft-rejects-mismatched-or-malformed-selection-sources ()
  (task-workflow-draft-test-with-root
    (dolist (sources '(((:kind "selection" :description "Different context"))
                       ((:kind "selection" :description 42))
                       ((:description "Exact context" :unpaired))
                       ((description "Exact context"))))
      (should-error (task-workflow-draft "Exact context" :sources sources) :type 'user-error))
    (should-not task-workflow-draft-test--calls)
    (should-not (file-exists-p (expand-file-name "tasks" root)))))

(provide 'task-workflow-draft-tests)
;;; task-workflow-draft-tests.el ends here
