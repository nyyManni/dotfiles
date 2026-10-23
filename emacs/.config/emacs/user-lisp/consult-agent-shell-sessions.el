;;; consult-agent-shell-sessions.el --- Pick an Agent-shell session -*- lexical-binding: t -*-

(require 'consult)
(require 'agent-shell-manager)

(defvar consult-agent-shell-sessions-history nil)

(defun consult-agent-shell-sessions--candidates (entries)
  "Format manager ENTRIES as aligned completion rows paired with buffers."
  (let ((widths (make-vector 6 0))
        (counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (dotimes (column 6)
        (aset widths column
              (max (aref widths column) (string-width (aref (cadr entry) column))))))
    (let ((rows
           (mapcar
            (lambda (entry)
              (let ((label
                     (mapconcat
                      (lambda (column)
                        (let ((text (aref (cadr entry) column)))
                          (if (= column 5) text
                            (concat text (make-string (- (aref widths column) (string-width text)) ?\s)))))
                      (number-sequence 0 5) "  ")))
                (puthash label (1+ (gethash label counts 0)) counts)
                (cons label (car entry))))
            entries)))
      ;; Distinct buffers can have identical titles, even for the same ACP ID.
      (mapcar (lambda (row)
                (if (= (gethash (car row) counts) 1)
                    row
                  (cons (format "%s  [%s]" (car row) (buffer-name (cdr row)))
                        (cdr row))))
              rows))))

;;;###autoload
(defun consult-agent-shell-sessions ()
  "Focus an Agent-shell session selected from a single manager snapshot."
  (interactive)
  (let ((candidates (consult-agent-shell-sessions--candidates
                     (agent-shell-manager--entries))))
    (unless candidates (user-error "No Agent-shell sessions"))
    (let ((buffer (consult--read candidates
                                 :prompt "Agent-shell session: "
                                 :category 'agent-shell-session
                                 :require-match t :sort nil
                                 :history 'consult-agent-shell-sessions-history
                                 :lookup #'consult--lookup-cdr)))
      (unless (buffer-live-p buffer) (user-error "Session buffer no longer exists"))
      (if-let* ((window (get-buffer-window buffer t)))
          (progn
            (select-frame-set-input-focus (window-frame window))
            (select-window window))
        (agent-shell--display-buffer buffer)))))

(provide 'consult-agent-shell-sessions)
;;; consult-agent-shell-sessions.el ends here
