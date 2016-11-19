;;; pydebug.el --- Python debugger functions for Emacs

;; Copyright (c) 2016 Henrik Nyman

;; Author     : Henrik Nyman <henrikjohannesnyman@gmail.com>
;; Maintainer : Henrik Nyman <henrikjohannesnyman@gmail.com>
;; Created    : 10 Aug 2016
;; Modified   : 10 Aug 2016
;; Version    : 1.0

;; The MIT License

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.


;;; Commentary:

;;; Code:
(require 'realgud)

(load-library "realgud")
(add-hook 'realgud-short-key-mode-hook
          (lambda ()
            (local-set-key "\C-c" realgud:shortkey-mode-map)))

(defun pydebug--get-python-version ()
  "Get current python venv major version, either 2 or 3.
Runs command 'Python -V' and captures major version from it."
  (string-to-number
   (substring
    (shell-command-to-string "python -V") 7 8)))

(defun with-py2-and-py3 (py2code py3code)
  "Execute code based on the current Python version.
PY2CODE: code to run when python2.x interpreter is in use
PY3CODE: code to run when python3.x interpreter is in use"
  (cond ((= (pydebug--get-python-version) 2) (py2code))
        ((= (pydebug--get-python-version) 3) (py3code))
        (t (error "Could not deduce python version"))))

(defun pydebug--version-dependent (&python2 py2code &python3 py3code)
  "Evaluate one of two expression based on which python virtualenv is active."
  (cond ((= (pydebug--get-python-version) 2) (eval py2code))
        ((= (pydebug--get-python-version) 3) (eval py3code))
        (t (error "Could not deduce python version"))))

(defun pydebug-run-pdb-current-file ()
  "Start a Python debugging session using currently visited file as __main__.
Python debugger shell is opened in the split view."
  (interactive)
  (let ((f (buffer-file-name))
        (b (buffer-name)))
    (if (string-suffix-p ".py" f)
        (progn
          (pdb (concat "python -m pdb " f))
          (switch-to-buffer-other-window "*gud-pdb*")
          (switch-to-buffer-other-window b))
      (message "Not in a Python buffer, or not visiting a file."))))

(defun pydebug-quit-pdb ()
  "Quit a python debugging session."
  (interactive)
  (progn
    (let ((p (get-process "gud-pdb")))
      (if p
          (kill-process (get-process p))
        (message "No active debugging process")))
    (kill-buffer "*gud-pdb*")))

(defun pydebug-run-realgud-current-file ()
  "Start a Python debugging session with realgud debugger framework."
  (interactive)
  ; If functions from realgud library are not bound, try loading the library
  (unless (fboundp 'realgud:trepan3k)
    (load-library "realgud"))
  (let ((f (buffer-file-name))
        (b (buffer-name)))
    (if (string-suffix-p ".py" f)
        (progn
          (delete-other-windows)
          (split-window-horizontally)
          (pydebug--version-dependent
           :python2
           `(realgud:trepan ,(concat "trepan " f))
           :python3
           `(realgud:trepan3k ,(concat "trepan3k " f)))
          (switch-to-buffer-other-window (realgud-get-cmdbuf)))
      (message "Not in a Python buffer, or not visiting a file."))))

(defun pydebug-quit-realgud ()
  "Quits an active realgud debugging session."
  (interactive)
  (progn
    (let ((p (realgud-get-process))
          (b (realgud-get-cmdbuf)))
      (progn
        (if p
            (kill-process p)
          (error "No active debugging process"))
        (if b
            (kill-buffer b)
          (error "No buffer connected to debugger process"))
        (delete-other-windows)))))

(defun pydebug-eval-in-minibuffer ()
  "Evaluate current python buffer and print its stdout in the minibuffer."
  (interactive)
  (shell-command (concat "python " (buffer-file-name)))
  (if (<= (* 2 (window-height)) (frame-height))
      (enlarge-window 20)
    (/ (frame-height) 2)))

(defun pydebug-send-buffer-or-region ()
  "Evaluate an s-expression or a region."
  (interactive)
  (if (evil-visual-state-p)
      (let ((r (evil-visual-range)))
        (python-shell-send-region (car r) (cadr r)))
    (python-shell-send-buffer)))

(defun pydebug-add-or-remove-bp ()
  "Toggle breakpoint on current line."
  (interactive)
    (block 'remove-bp
      (dolist (bp (realgud-srcbuf-bp-list (get-buffer "manage.py")))
        (let ((num (realgud-loc-num bp))
              (fname (realgud-loc-filename bp))
              (line (realgud-loc-line-number bp)))
          (when
              (and
               (string= fname (buffer-file-name))
               (= line (line-number-at-pos)))
            (realgud:cmd-clear line)
            (return-from 'remove-bp))))
      (realgud:cmd-break)))



(provide 'pydebug)

;;; pydebug.el ends here
