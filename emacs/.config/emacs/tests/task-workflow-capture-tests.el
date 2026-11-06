;;; task-workflow-capture-tests.el --- Local selection intake behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-element)
(require 'task-workflow-capture)
(require 'task-workflow-draft-test-helpers
         (expand-file-name "task-workflow-draft-test-helpers"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defun task-workflow-capture-test--select (begin end)
  "Activate a selection with mark at BEGIN and point at END, in either order."
  (setq-local transient-mark-mode t)
  (goto-char end)
  (set-mark begin)
  (setq mark-active t))

(defun task-workflow-capture-test--git (directory &rest arguments)
  "Run real local Git fixture setup in DIRECTORY with ARGUMENTS."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (unless (eq 0 (apply #'process-file "git" nil t nil arguments))
        (ert-fail (format "Git fixture failed: %S: %s" arguments (buffer-string))))
      (string-trim (buffer-string)))))

(defmacro task-workflow-capture-test-with-repository (&rest body)
  "Run BODY with a real committed repository inside the draft test root."
  (declare (indent 0) (debug t))
  `(task-workflow-draft-test-with-root
     (let* ((repository (expand-file-name "repository/" root))
            (process-environment
             (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null")
                     process-environment))
            (project-find-functions '(project-try-vc))
            (vc-handled-backends '(Git))
            (task-workflow-capture-excluded-source-roots nil))
       (make-directory repository)
       (task-workflow-capture-test--git repository "init" "-q" "-b" "capture-main")
       (task-workflow-capture-test--git repository "config" "user.name" "Capture Test")
       (task-workflow-capture-test--git repository "config" "user.email" "capture@example.invalid")
       (task-workflow-capture-test--git repository "config" "commit.gpgsign" "false")
       (task-workflow-capture-test--git repository "config" "core.hooksPath" "/dev/null")
       (write-region "Committed disk contents\n" nil (expand-file-name "notes.txt" repository) nil 'silent)
       (task-workflow-capture-test--git repository "add" "notes.txt")
       (task-workflow-capture-test--git repository "commit" "-q" "-m" "Initial fixture")
       ,@body)))

(ert-deftest task-workflow-capture-snapshots-unsaved-narrowed-reversed-region-without-side-effects ()
  (task-workflow-draft-test-with-root
    (let* ((file (expand-file-name "notes.txt" root))
           (task-workflow-capture-excluded-source-roots nil)
           (selected "First unsaved line\nSecond unsaved line\n")
           owner)
      (write-region "Disk text must not be captured\n" nil file nil 'silent)
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root)
              buffer-file-name file)
        (insert "Outside before\n" (propertize selected 'face 'bold) "Outside after\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((begin (point)))
          (forward-line 2)
          (let ((end (point)))
            (narrow-to-region begin end)
            (task-workflow-capture-test--select end begin)
            (setq owner (task-workflow-capture))))
        (should-not task-workflow-draft-test--calls)
        (should-not (file-exists-p (expand-file-name "tasks" root)))
        (with-current-buffer owner
          (should (equal (task-workflow-draft-field-value 'context) selected))
          (should-not (text-properties-at 0 (task-workflow-draft-field-value 'context)))
          (should-error (task-workflow-draft-set-field 'context "Replacement") :type 'user-error)
          (goto-char (car (task-workflow-draft-field-bounds 'context)))
          (should-error (insert "Replacement"))
          (task-workflow-draft-set-field 'intent "Investigate before changing anything")
          (task-workflow-draft-generate)
          (let* ((input (task-workflow-draft-session-input task-workflow-draft-session))
                 (source (car (task-workflow-draft-input-sources input))))
            (should (equal (plist-get source :description) selected))
            (should (equal (plist-get source :kind) "selection"))
            (should (equal (plist-get source :file) file))
            (should (= (plist-get source :line_start) 2))
            (should (= (plist-get source :line_end) 3))
            (should (eq (plist-get source :modified) t))
            (should (equal (plist-get source :major_mode) "fundamental-mode"))
            (should (stringp (plist-get source :captured_at)))
            (should (equal (task-workflow-draft-input-intent input)
                           "Investigate before changing anything"))
            (should-not (string-match-p "Disk text must not be captured\\|Outside before\\|Outside after"
                                        (task-workflow-draft-test-call-prompt
                                         (car task-workflow-draft-test--calls)))))))
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-capture-line-ranges-are-absolute-inclusive-and-direction-independent ()
  (task-workflow-draft-test-with-root
    (let ((task-workflow-capture-excluded-source-roots nil))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root)
              buffer-file-name (expand-file-name "not-yet-saved.txt" root))
        (insert "hidden\none\ntwo\nthree\n")
        (narrow-to-region 8 (point-max))
        ;; Positions 8..12 contain "one\n"; 12 is the next beginning-of-line.
        (dolist (bounds '((8 12 2 2) (12 8 2 2) (9 14 2 3) (14 9 2 3) (12 13 3 3)))
          (let ((source (task-workflow-capture--source (nth 0 bounds) (nth 1 bounds))))
            (should (= (plist-get source :line_start) (nth 2 bounds)))
            (should (= (plist-get source :line_end) (nth 3 bounds)))
            (should (equal (plist-get source :description)
                           (buffer-substring-no-properties
                            (min (nth 0 bounds) (nth 1 bounds))
                            (max (nth 0 bounds) (nth 1 bounds)))))))))))

(ert-deftest task-workflow-capture-fileless-source-has-only-observed-buffer-context ()
  (task-workflow-draft-test-with-root
    (let ((task-workflow-capture-excluded-source-roots nil)
          (project-find-functions '(project-try-vc)))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root))
        (insert "Only this scratch context")
        (set-buffer-modified-p nil)
        (let ((source (task-workflow-capture--source (point-min) (point-max))))
          (should (equal (plist-get source :description) "Only this scratch context"))
          (should (equal (plist-get source :buffer) (buffer-name)))
          (should (eq (plist-get source :modified) :json-false))
          (dolist (key '(:file :line_start :line_end :project_name :project_root
                              :project_relative_file :git_root :git_branch :git_revision))
            (should-not (plist-member source key))))))))

(ert-deftest task-workflow-capture-real-git-untracked-file-and-linked-checkout-metadata ()
  (task-workflow-capture-test-with-repository
    (let ((worktree (expand-file-name "linked-checkout/" root)))
      (task-workflow-capture-test--git repository "worktree" "add" "-q" "-b" "capture-linked"
                                     (directory-file-name worktree) "HEAD")
      (write-region "Untracked disk text must not replace the selection\n"
                    nil (expand-file-name "untracked.txt" worktree) nil 'silent)
      (with-temp-buffer
        (setq default-directory worktree
              buffer-file-name (expand-file-name "untracked.txt" worktree))
        (insert "Unsaved, untracked worktree selection\n")
        (let ((source (task-workflow-capture--source (point-min) (point-max))))
          (should (equal (plist-get source :file) buffer-file-name))
          (should (equal (plist-get source :description) "Unsaved, untracked worktree selection\n"))
          (should (equal (plist-get source :git_root) worktree))
          (should-not (equal (plist-get source :git_root) repository))
          (should (equal (plist-get source :git_branch) "capture-linked"))
          (should (equal (plist-get source :git_revision)
                         (task-workflow-capture-test--git worktree "rev-parse" "HEAD")))
          (should (equal (plist-get source :project_root) worktree))
          (should (equal (plist-get source :project_name) "linked-checkout"))
          (should (equal (plist-get source :project_relative_file) "untracked.txt"))
          (should (equal (task-workflow-capture-test--git worktree "status" "--porcelain")
                         "?? untracked.txt"))))
      (task-workflow-capture-test--git worktree "checkout" "-q" "--detach" "HEAD")
      (with-temp-buffer
        (setq default-directory worktree
              buffer-file-name (expand-file-name "notes.txt" worktree))
        (insert "Detached selection")
        (let ((source (task-workflow-capture--source (point-min) (point-max))))
          (should (equal (plist-get source :git_root) worktree))
          (should-not (plist-member source :git_branch))
          (should (string-match-p "\\`[0-9a-f]+\\'" (plist-get source :git_revision))))))))

(ert-deftest task-workflow-capture-source-death-keeps-full-provenance-and-inert-accepted-json ()
  (task-workflow-capture-test-with-repository
    (let* ((payload (concat "#+end_example\n* TODO Not a task\n:PROPERTIES:\n:ID: injected\n:END:\n"
                            "#+begin_src emacs-lisp\n(setq task-workflow-draft-test--executed t)\n#+end_src\n"
                            "[[elisp:(setq task-workflow-draft-test--executed t)][Run]]\n"
                            "# Local " "Variables:\n# eval: (setq task-workflow-draft-test--executed t)\n# End:\n"
                            (make-string 50000 ?x) "\n"))
           (task-workflow-draft-test--executed nil)
           (source-buffer (generate-new-buffer "selection\n#+end_example\n* TODO Metadata is inert"))
           (source-name (buffer-name source-buffer))
           (file (expand-file-name "notes.txt" repository))
           owner session snapshot)
      (unwind-protect
          (progn
            (with-current-buffer source-buffer
              (setq default-directory repository buffer-file-name file)
              (insert payload)
              (task-workflow-capture-test--select (point-min) (point-max))
              (setq owner (task-workflow-capture)))
            (should-not task-workflow-draft-test--calls)
            (should-not (file-exists-p (expand-file-name "tasks" root)))
            ;; Changing and killing the original before Generate must not affect
            ;; the snapshot or cause a disk read to replace the unsaved text.
            (with-current-buffer source-buffer
              (erase-buffer)
              (insert "Source replaced after opening review")
              (set-buffer-modified-p nil))
            (kill-buffer source-buffer)
            (with-current-buffer owner
              (should (equal (task-workflow-draft-field-value 'context) payload))
              (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
              (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
              (should-not
               (org-element-map (org-element-parse-buffer) 'headline
                 (lambda (headline)
                   (member (org-element-property :raw-value headline)
                           '("Not a task" "Metadata is inert")))))
              (task-workflow-draft-set-field 'intent "Preserve the literal evidence")
              (task-workflow-draft-generate)
              (setq session task-workflow-draft-session
                    snapshot (copy-tree (car (task-workflow-draft-input-sources
                                               (task-workflow-draft-session-input session)))))
              (should (equal (plist-get snapshot :description) payload))
              (should (equal (plist-get snapshot :buffer) source-name))
              (should (equal (plist-get snapshot :file) file))
              (should (eq (plist-get snapshot :modified) t))
              (should (equal (plist-get snapshot :git_branch) "capture-main"))
              (should (equal (plist-get snapshot :git_root) repository))
              (should (equal (plist-get snapshot :project_relative_file) "notes.txt")))
            (task-workflow-draft-test-complete (car task-workflow-draft-test--calls))
            (with-current-buffer (car (task-workflow-draft-session-buffers session))
              (should (equal (car (task-workflow-draft-input-sources
                                   (task-workflow-draft-session-input task-workflow-draft-session)))
                             snapshot))
              (should (equal (task-workflow-draft-field-value 'context) payload))
              (task-workflow-draft-accept))
            (set-buffer (window-buffer (selected-window)))
            (should (= (length (task-workflow--collect t)) 1))
            (should-not (org-element-map (org-element-parse-buffer) 'src-block #'identity))
            (should-not (org-element-map (org-element-parse-buffer) 'link #'identity))
            (should-not
             (org-element-map (org-element-parse-buffer) 'headline
               (lambda (headline)
                 (member (org-element-property :raw-value headline)
                         '("Not a task" "Metadata is inert")))))
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "^\\*\\* Capture source$")
              (forward-line 2)
              (let* ((json (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     (saved (json-parse-string json :object-type 'plist :array-type 'list
                                               :false-object :json-false)))
                (should (equal (car (plist-get saved :sources)) snapshot))))
            (let ((enable-local-variables :all) (enable-local-eval t))
              (hack-local-variables))
            (should-not task-workflow-draft-test--executed))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer (set-buffer-modified-p nil))
          (kill-buffer source-buffer))))))

(ert-deftest task-workflow-capture-inactive-and-empty-regions-use-editable-rough-context ()
  (task-workflow-draft-test-with-root
    (let ((task-workflow-capture-excluded-source-roots (list root)))
      (dolist (empty '(nil t))
        (with-temp-buffer
          ;; No-region entry is allowed even where selected intake is forbidden.
          (setq default-directory (file-name-as-directory root))
          (insert "Private text must not enter rough context")
          (task-workflow-capture-test--select (if empty (point-max) (point-min)) (point-max))
          (setq mark-active empty)
          (let ((use-empty-active-region t)
                (owner (task-workflow-capture)))
            (with-current-buffer owner
              (should (equal (task-workflow-draft-field-value 'context) ""))
              (should-not task-workflow-draft-test--calls)
              (should-not (file-exists-p (expand-file-name "tasks" root)))
              (task-workflow-draft-set-field 'context "Rough context entered deliberately")
              (task-workflow-draft-generate)
              (let ((source (car (task-workflow-draft-input-sources
                                  (task-workflow-draft-session-input task-workflow-draft-session)))))
                (should (equal (plist-get source :kind) "description"))
                (should (equal (plist-get source :description) "Rough context entered deliberately"))
                (should-not (plist-member source :buffer))))))
        (setq task-workflow-draft-test--calls nil)))))

(ert-deftest task-workflow-capture-explicit-draft-and-manual-remain-selection-independent ()
  (task-workflow-draft-test-with-root
    (with-temp-buffer
      (setq default-directory (file-name-as-directory root))
      (insert "Selected text is not an implicit source for other entry points")
      (task-workflow-capture-test--select (point-min) (point-max))
      (let ((owner (task-workflow-draft "Deliberate text-only context")))
        (with-current-buffer owner
          (task-workflow-draft-generate)
          (let ((source (car (task-workflow-draft-input-sources
                              (task-workflow-draft-session-input task-workflow-draft-session)))))
            (should (equal (plist-get source :kind) "description"))
            (should (equal (plist-get source :description) "Deliberate text-only context"))
            (should-not (plist-member source :file)))))
      (setq task-workflow-draft-test--calls nil)
      (let ((id (task-workflow-capture-manual "Manual task despite active selection")))
        (should (stringp id))
        (should-not task-workflow-draft-test--calls)
        (should (= (length (task-workflow--collect t)) 1))
        (should (equal (plist-get (car (task-workflow--collect t)) :title)
                       "Manual task despite active selection"))))))

(ert-deftest task-workflow-capture-excluded-files-and-fileless-directories-refuse-before-probes ()
  (task-workflow-draft-test-with-root
    (let* ((work (expand-file-name "work/" root))
           (task-workflow-capture-excluded-source-roots (list work)))
      (make-directory work)
      (dolist (file (list nil (expand-file-name "customer.txt" work)))
        (with-temp-buffer
          (setq default-directory work buffer-file-name file)
          (insert "Customer selection")
          (task-workflow-capture-test--select (point-min) (point-max))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (ert-fail "Project probe")))
                    ((symbol-function 'vc-git-root) (lambda (&rest _) (ert-fail "VC probe")))
                    ((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "Excluded path IO")))
                    ((symbol-function 'task-workflow-draft) (lambda (&rest _) (ert-fail "Draft opened"))))
            (should-error (task-workflow-capture) :type 'user-error))))
      (should-not task-workflow-draft-test--calls)
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-capture-symlink-aliases-cannot-bypass-excluded-roots ()
  (task-workflow-draft-test-with-root
    (let* ((work (expand-file-name "work/" root))
           (alias (expand-file-name "innocent-alias/" root)))
      (make-directory work)
      (make-symbolic-link (directory-file-name work) (directory-file-name alias))
      ;; Both a source alias and an excluded-root alias must resolve safely.
      (dolist (locations (list (list alias work) (list work alias)))
        (let ((task-workflow-capture-excluded-source-roots (list (cadr locations))))
          (dolist (file-backed '(nil t))
            (with-temp-buffer
              (setq default-directory (car locations)
                    buffer-file-name (and file-backed (expand-file-name "customer.txt" (car locations))))
              (insert "Customer selection via symlink")
              (task-workflow-capture-test--select (point-min) (point-max))
              (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (ert-fail "Project probe")))
                        ((symbol-function 'vc-git-root) (lambda (&rest _) (ert-fail "VC probe")))
                        ((symbol-function 'process-file) (lambda (&rest _) (ert-fail "Process started")))
                        ((symbol-function 'insert-file-contents) (lambda (&rest _) (ert-fail "File contents read")))
                        ((symbol-function 'task-workflow-draft) (lambda (&rest _) (ert-fail "Draft opened"))))
                (should-error (task-workflow-capture) :type 'user-error))))))
      (should-not task-workflow-draft-test--calls)
      (should-not (file-exists-p (expand-file-name "tasks" root))))))

(ert-deftest task-workflow-capture-remote-files-and-directories-refuse-before-io ()
  (task-workflow-draft-test-with-root
    (dolist (locations (list (list (file-name-as-directory root) "/ssh:unreachable.invalid:/work/notes.txt")
                            (list "/ssh:unreachable.invalid:/work/" nil)
                            (list "/ssh:unreachable.invalid:/work/" (expand-file-name "local.txt" root))))
      (with-temp-buffer
        (setq default-directory (car locations) buffer-file-name (cadr locations))
        (insert "Remote selection")
        (task-workflow-capture-test--select (point-min) (point-max))
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (ert-fail "Project probe")))
                  ((symbol-function 'vc-git-root) (lambda (&rest _) (ert-fail "VC probe")))
                  ((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "Remote resolution")))
                  ((symbol-function 'process-file) (lambda (&rest _) (ert-fail "Remote process")))
                  ((symbol-function 'start-file-process) (lambda (&rest _) (ert-fail "Remote subprocess")))
                  ((symbol-function 'make-network-process) (lambda (&rest _) (ert-fail "Network connection")))
                  ((symbol-function 'insert-file-contents) (lambda (&rest _) (ert-fail "Remote contents")))
                  ((symbol-function 'task-workflow-draft) (lambda (&rest _) (ert-fail "Draft opened"))))
          (should-error (task-workflow-capture) :type 'user-error))))
    (should-not task-workflow-draft-test--calls)
    (should-not (file-exists-p (expand-file-name "tasks" root)))))

(provide 'task-workflow-capture-tests)
;;; task-workflow-capture-tests.el ends here
