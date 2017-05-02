;;; init.el --- Emacs configuration

;; Copyright (c) 2016 - 2017 Henrik Nyman

;; Author     : Henrik Nyman <henrikjohannesnyman@gmail.com>
;; Created    : 10 Aug 2016
;; Modified   : 20 Mar 2017
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

;; nyyManni's configuration for Emacs.
;;
;; Main packages:
;; - evil for movement
;; - general for keybindings
;; - helm for searching and narrowing
;; - company for completion
;; - flycheck for syntax-checking


;;; Code:

;; Global settings
(setq user-full-name                       "Henrik Nyman"
      user-login-name                      "nyman"
      user-mail-address                    "henrikjohannesnyman@gmail.com"
      user-emacs-directory                 "~/.emacs.d"
      vc-follow-symlinks                   t

      inhibit-startup-screen               t
      sentence-end-double-space            nil

      ;; Disable custom-set-variable by pointing it's output to a file that is
      ;; never executed.
      custom-file                          (concat user-emacs-directory
                                                   "/customize-ignored.el")

      initial-scratch-message              ""
      ad-redefinition-action               'accept
      backup-directory-alist               '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms       '((".*" "~/.emacs.d/auto-save-list" t))
      delete-old-versions                  -1
      version-control                      t
      vc-make-backup-files                 t
      tab-width                            2
      frame-title-format                   '("" "Emacs v" emacs-version))

(setq-default indent-tabs-mode             nil
              fill-column                  80)

;; OS X specific settings
(when (eq system-type 'darwin)
  (setq exec-path                          (append exec-path '("/usr/local/bin"))
        default-input-method               "MacOSX"
        mac-command-modifier               'meta
        mac-option-modifier                nil
        mac-allow-anti-aliasing            t
        ns-use-srgb-colorspace             nil)

  ;; Environment variables
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setenv "SHELL" "/bin/zsh")
  (setenv "LC_CTYPE" "UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Transparent frames. On Linux the same is achieved with compton.
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  (add-to-list 'default-frame-alist '(alpha 90 90)))

;; Load customizations that cannot be put under public VCS. Do not die if the
;; file does not exist.
(condition-case err
    (load-file (concat user-emacs-directory "/work-config.el"))
  (error nil))

;; Setup use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure     t
        use-package-check-before-init t))

(require 'diminish)
(require 'bind-key)


(blink-cursor-mode 0)
(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Fix linum-mode to not scale line-numbers while zooming
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))


(function-put #'add-hook 'lisp-indent-function 'defun)

;; Package configurations

(use-package gotham-theme
  :demand
  :if (or (daemonp) window-system)
  :init
  (global-unset-key (kbd "C-z"))
  :config
  (load-theme 'gotham t)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (load-theme 'gotham t)
      (scroll-bar-mode -1)
      (powerline-reset))))


(defun my-sudo-at-point ()
  "Reopen current file as sudo, preserving location of point."
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo::" buffer-file-name))
    (goto-char p)))

(defun my-reload-file ()
  "Reopen current file, preserving location of point."
  (interactive)
  (let ((p (point)))
    (find-alternate-file buffer-file-name)
    (goto-char p)))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defmacro append-to-list (l1 l2)
  "Modify list L1 by appending L2 to it."
  `(setq ,l1 (append ,l1 ,l2)))

(defmacro setq-mode-local (mode &rest args)
  "Add a hook to MODE and set mode-local values for ARGS.

Allows for setting mode-local variables like:
   (setq-mode-local mode-name
                    (variable  . value)
                    (variable2 . value2)
                     ...
                    (variableN . valueN))"
  ;; TODO: use make-symbol for arg.
  `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       ,@(mapcar
          #'(lambda (arg) `(set (make-local-variable ',(car arg)) ,(cdr arg)))
          args))))

(defun is-current-file-tramp ()
  "Check if the file is a remote tramp file."
  (require 'tramp)
  (tramp-tramp-file-p (buffer-file-name (current-buffer))))

(defun my-inside-range-p (value lower-bound upper-bound)
  "Check if VALUE is between LOWER-BOUND and UPPER-BOUND.
VALUE being equal to either of the bounds is considered inside."
  (and (<= value upper-bound) (>= value lower-bound)))

(defun my-detect-quotes ()
  "Detects whether point is inside a quoted string.
If it is, then the type of the quotes is returned (double|single)."
  ;; Verify that we are inside a quoted string.
  (when (nth 3 (syntax-ppss))
    (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
           (dbl-match (string-match "\"[^\\\\\"]+\\(?:\\\\.[^\\\\\"]*\\)*\"" line))
           (dbl-begin (if dbl-match (match-beginning 0) nil))
           (dbl-end (if dbl-match (match-end 0) nil))
           (sgl-match (string-match "'[^\\\\']+\\(?:\\\\.[^\\\\']*\\)*'" line))
           (sgl-begin (if sgl-match (match-beginning 0) nil))
           (sgl-end (if sgl-match (match-end 0) nil))
           (point-pos (- (point) (line-beginning-position))))

      (cond ((and dbl-match sgl-match)
             ;; The line contains both double- and single-quotes, need to
             ;; further analyze.
             (cond ((and (my-inside-range-p point-pos dbl-begin dbl-end)
                         (not (my-inside-range-p point-pos sgl-begin sgl-end)))
                    ;; Point is inside double-quotes, but not inside single-
                    ;; quotes.
                    ;;         " |  "     '    '
                    'double)
                   ((and (my-inside-range-p point-pos sgl-begin sgl-end)
                         (not (my-inside-range-p point-pos dbl-begin dbl-end)))

                    ;; Point is inside single-quotes, but not inside double-
                    ;; quotes.
                    ;;         "    "     ' |  '
                    'single)
                   ((and (my-inside-range-p sgl-begin dbl-begin dbl-end)
                         (my-inside-range-p sgl-end dbl-begin dbl-end))
                    ;; Single-quotes nested inside double-quotes.
                    ;;          "    '  |  '    "
                    'double)
                   ((and (my-inside-range-p dbl-begin sgl-begin sgl-end)
                         (my-inside-range-p dbl-end sgl-begin sgl-end))
                    ;; Double-quotes nested inside single-quotes.
                    ;;          '    "  |  "    '
                    'double)
                   (t
                    ;; Quotations are too complex to be analyzed.
                    nil)))
            (dbl-match
             ;; Line contains only double quotes.
             'double)
            (sgl-match
             ;; Line contains only single quotes.
             'single)
            (t nil)))))

(defun my-split-string (invert)
  "Split a string delimited with single or double quotes at point.
When INVERT equals to t, the return value is set to the other type of quote.
That is for situations where the function detects wrong quotes, and thus the
user can manually override it to use the correct ones."
  (interactive "P")
  (let ((quote-type (my-detect-quotes)))
    (when (not quote-type) (error "Point is not inside a string"))
    (progn
      (insert (if (or (and (equal quote-type 'double) (not invert))
                      (and (equal quote-type 'single) invert))
                  "\"\"" "''"))
      (backward-char)
      (when (commandp 'evil-insert-state)
        (evil-insert-state)))))

;; Disable backup's with tramp files.
(add-hook 'find-file-hook
  (lambda ()
    (if (is-current-file-tramp) (setq-local make-backup-files nil))))

(use-package general
  :functions (space-leader)
  :config
  ;; Fix auto indentation
  (function-put #'general-define-key 'lisp-indent-function 'defun)
  (function-put #'general-create-definer 'lisp-indent-function 'defun)

  (general-create-definer space-leader
    :states '(normal visual insert emacs)
    :global-prefix "C-c"
    :non-normal-prefix "M-SPC"
    :prefix "SPC")

  (function-put #'space-leader 'lisp-indent-function 'defun)

  ;; Global keybindings
  (general-define-key
    :prefix "SPC"
    :states '(normal visual)
    "x"    'helm-M-x
    "b"    'helm-mini
    "c"    'comment-dwim-2
    "O"    'helm-occur
    "A"    'helm-apropos
    "y"    'helm-show-kill-ring
    "e"    'eval-last-sexp
    "m h"  'mark-whole-buffer
    "w"    'save-buffer
    "D"    'kill-this-buffer
    "a a"  'align-regexp
    "s u"  'my-sudo-at-point
    "s h"  'my-eshell-here
    "s '"  'my-split-string
    "s l"  'sort-lines
    "r"    'my-reload-file
    "f"    'helm-imenu
    "g g"  'magit-status
    "S"    'delete-trailing-whitespace
    "i"    'indent-region
    "0"    'delete-window
    "1"    'delete-other-windows
    "2"    'split-window-below
    "3"    'split-window-right))

(use-package org
  :ensure nil
  :init
  (setq org-use-fast-todo-selection t)
  :general
  (space-leader
    :keymaps '(org-mode-map)
    "o t c" 'org-table-create

    ;; Task management keybindings.
    "o a"   'org-agenda
    "o c"   'org-capture
    "o t i" 'org-clock-clock-in
    "o t o" 'org-clock-clock-out
    "o s"   'org-todo
    ))

(use-package evil
  :after general
  :config
  ;; Make escape quit everything, whenever possible.
  (general-define-key
    :keymaps '(evil-normal-state-map evil-visual-state-map)
    "<escape>" 'keyboard-quit)
  (general-define-key
    :keymaps '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
    "<escape>" 'minibuffer-keyboard-quit)

  ;; Completely disable the mouse
  (dolist (key '([drag-mouse-1] [down-mouse-1] [mouse-1]
                 [drag-mouse-2] [down-mouse-2] [mouse-2]
                 [drag-mouse-3] [down-mouse-3] [mouse-3]))
    (global-unset-key key)
    (general-define-key :keymaps '(evil-motion-state-map) key nil))

  (general-define-key
    :states '(visual)
    "<"       'my-evil-shift-left-visual
    ">"       'my-evil-shift-right-visual
    "S-<tab>" 'my-evil-shift-left-visual
    "<tab>"   'my-evil-shift-right-visual)

  ;; Disable C-k, it conflicts with company selecting.
  (eval-after-load "evil-maps"
    (dolist (map '(evil-motion-state-map
       evil-insert-state-map
       evil-emacs-state-map))
      (define-key (eval map) (kbd "C-k") nil)))

  (defun my-evil-shift-left-visual ()
    "Shift left and keep region active."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun my-evil-shift-right-visual ()
    "Shift right and keep region active."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-mode 1))

(use-package key-chord
  :after general
  :config
  (dolist (chord '("jk" "kj" "JK" "KJ" "jK" "kJ" "Jk" "Kj"))
    (general-define-key
      :keymaps '(evil-insert-state-map evil-visual-state-map)
      (general-chord chord) 'evil-normal-state))
  (key-chord-mode t))

(use-package company
  :after evil
  :functions (is-empty-line-p my-complete-or-indent)
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t)
  :config
  (general-define-key
    :states '(insert)
    "<tab>" 'my-complete-or-indent)
  (defun is-empty-line-p ()
    (string-match "^[[:blank:]]*$"
                  (buffer-substring (line-beginning-position)
                                    (point))))

  (defun my-complete-or-indent ()
    "On an empty (only whitespace) line, do an indent, otherwise auto-complete."
    (interactive)
    (if (is-empty-line-p)
        (indent-for-tab-command)
      (company-complete)))
  (company-quickhelp-mode 1)
  :bind
  (:map company-active-map
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  :bind
  (:map company-active-map
   ("C-h" . company-quickhelp-manual-begin)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-reload-all snippet-mode yas-minor-mode)
  :mode ("\\.yasnippet" . snippet-mode)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all)

  ;; Disable tab key for yasnippet, so that it does not cause confusion with
  ;; company-mode.
  (dolist (keymap '(yas-minor-mode-map yas-keymap))
    (define-key (eval keymap) (kbd "<tab>") nil)
    (define-key (eval keymap) [(tab)] nil)
    (define-key (eval keymap) (kbd "S-<tab>") nil)
    (define-key (eval keymap) [(shift tab)] nil)
    (define-key (eval keymap) [backtab] nil))

  (defvar my-yas-expanding nil
    "A flag that is t when a yasnippet expansion is in progress. It is used to
    not load fci-mode with yasnippet expansion.")

  ;; Add hooks for disabling fill-column-indicator while expanding a snippet.
  (defun my-yas-begin-hook ()
    (setq my-yas-expanding t)
    (message "enabling yas-expand-mode")
    (when (and (derived-mode-p 'prog-mode)
               (functionp 'turn-off-fci-mode))
      (turn-off-fci-mode)))
  (defun my-yas-end-hook ()
    (setq my-yas-expanding nil)
    (when (and (derived-mode-p 'prog-mode)
               (functionp 'turn-off-fci-mode))
      (turn-on-fci-mode)))

  (add-hook 'yas-before-expand-snippet-hook 'my-yas-begin-hook)
  (add-hook 'yas-after-exit-snippet-hook 'my-yas-end-hook)

  ;; Use C-& and C-* for going through the fields, since they are positioned
  ;; nicely on a US keyboard.
  (general-define-key
    :states '(insert)
    "C-&" 'yas-expand)
  (general-define-key
    :keymaps '(yas-keymap)
    "C-&" 'yas-next-field-or-maybe-expand
    "C-*" 'yas-prev-field))

(use-package expand-region
  :after evil
  :init
  (general-define-key
    :states '(normal visual)
    "C-+" 'er/expand-region))

(use-package flycheck
  :diminish
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (general-define-key
    :states '(normal visual)
    "[ e" 'flycheck-previous-error
    "] e" 'flycheck-next-error))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :functions (my-python-hook my-ipython-hook my-jedi-show-doc
                             my-python-change-venv)
  :defines (jedi:doc-display-buffer
            jedi:tooltip-method
            realgud:pdb-command-name
            realgud:trepan3k-command-name)
  :after (company evil)
  :init
  ;; Workaround for Emacs 25.1 not working correctly with Python 3 native
  ;; completion.
  (setq python-shell-completion-native-enable nil
        jedi:doc-display-buffer               'my-jedi-show-doc
        jedi:tooltip-method                   nil
        realgud:pdb-command-name              "python -m pdb"
        realgud:trepan3k-command-name         "trepan3k --highlight=plain")

  (when (executable-find "ipython")
    (setq python-shell-interpreter      "ipython"
          python-shell-interpreter-args (concat "--simple-prompt "
                                                "-i --no-confirm-exit "
                                                "--colors=NoColor")))
  :config
  (define-coding-system-alias 'UTF-8 'utf-8)

  (setq-mode-local python-mode
                   (company-backends . '((company-jedi))))
  (setq-mode-local inferior-python-mode
                   (company-backends . '((company-capf company-jedi))))

  (defun my-python-hook ()
    (set-face-attribute 'jedi:highlight-function-argument nil
                        :inherit 'bold
                        :foreground "chocolate"))

  (defun my-python-change-venv ()
    "Switches to a new virtualenv, and reloads flycheck and company."
    (interactive)
    (call-interactively 'pyvenv-workon)
    (when (eq major-mode 'python-mode)
      ;; Reset flycheck and company to new venv.
      (flycheck-buffer)
      (jedi:stop-server)
      (pyvenv-restart-python)))

  (defun my-jedi-show-doc (buffer)
    (with-current-buffer buffer
      (message (buffer-string))))

  (defun my-python-send-region-or-buffer ()
    "Send buffer contents to an inferior Python process."
    (interactive)
    (if (evil-visual-state-p)
        (let ((r (evil-visual-range)))
          (python-shell-send-region (car r) (cadr r)))
      (python-shell-send-buffer t)))

  (define-key inferior-python-mode-map
    [(control return)] 'my-ipython-follow-traceback)

  (defun my-ipython-follow-traceback ()
    "Open the file at the line where the exception was rised."
    (interactive)
    (backward-paragraph)
    (forward-line)
    (re-search-forward "^\\(.*\\) in .*$")
    (let ((filename (match-string 1)))
      (re-search-forward "^-+> \\([0-9]+\\)")
      (let ((lineno (match-string 1)))
        (forward-whitespace 1)
        (find-file-existing filename)
        (goto-char (point-min))
        (forward-line (- (string-to-number lineno) 1))
        (forward-whitespace 1)
        (recenter))))

  (defun my-run-unittests (arg)
    "Run unittests in the current project. Use prefix-argument ARG to specify
the command to run the tests with."
    (interactive "P")
    (let ((compilation-read-command arg))
      (call-interactively 'projectile-test-project)))

  (add-hook 'inferior-python-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'my-python-hook)

  (general-define-key
    :states '(insert)
    :keymaps '(python-mode-map)
    "C-<tab>" 'jedi:get-in-function-call)

  (general-define-key
    :keymaps '(python-mode-map)
    "C->" 'sp-forward-slurp-sexp
    "C-<" 'sp-forward-barf-sexp)

  (general-define-key
    :keymaps '(python-mode-map)
    :states '(normal visual)
    "[ f" 'python-nav-backward-defun
    "] f" 'python-nav-forward-defun
    "[ b" 'python-nav-backward-block
    "] b" 'python-nav-forward-block)

  (function-put #'font-lock-add-keywords 'lisp-indent-function 'defun)

  ;; Syntax highlighting for ipython tracebacks.
  (font-lock-add-keywords 'inferior-python-mode
    '(("^-\\{3\\}-+$" . font-lock-comment-face)
      ("^\\([a-zA-Z_0-9]+\\) +\\(Traceback (most recent call last)\\)$"
       (1 font-lock-warning-face)
       (2 font-lock-constant-face))
      ("^\\(.*\\) in \\(<?[a-zA-Z_0-9]+>?\\)(.*)$"
       (1 font-lock-constant-face)
       (2 font-lock-function-name-face))
      ("^-*> +[[:digit:]]+ .*$" . font-lock-builtin-face)
      ("^   +[[:digit:]]+ " . font-lock-comment-face)))

  ;; Recenter the buffer after following the symbol under cursor.
  (defun my-recenter (&rest args) (recenter))
  (advice-add #'jedi:goto-definition--nth :after #'my-recenter)

  (use-package pydebug
    :ensure nil
    :load-path "~/projects/elisp/pydebug"
    :config
    (defun my-shortkey-mode-hook ()
      (evil-insert-state 1))
    (add-hook 'realgud-short-key-mode-hook #'my-shortkey-mode-hook)
    :general
    (space-leader
      :keymaps '(python-mode-map)
      "p b r" 'pydebug-run-realgud-current-file))

  :general
  (space-leader
    :keymaps '(python-mode-map realgud-mode-map)
    "p v"   'my-python-change-venv
    "p d"   'jedi:goto-definition
    "p b a" 'realgud-short-key-mode
    "p u"   'helm-jedi-related-names
    "p ?"   'jedi:show-doc
    "p r"   'run-python
    "p t"   'my-run-unittests
    "m f"   'python-mark-defun
    "e"     'my-python-send-region-or-buffer))


(use-package jedi-core
  :after python)

(use-package pyvenv
  :after python
  :commands (pyvenv-tracking-mode)
  :init
  (add-hook 'python-mode-hook #'pyvenv-tracking-mode))

(use-package py-autopep8
  :commands (py-autopep8-enable-on-save)
  :init
  ;; Set the line-length for autopep to something large so that it
  ;; does not touch too long lines, it usually cannot fix them properly
  (setq py-autopep8-options '("--max-line-length=200"))
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(use-package ace-window
  :after general
  :defines (aw-dispatch-always)
  :init
  (setq aw-dispatch-always 1))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"
        slime-default-lisp    'sbcl)
  :config
  (defun my-eval-sexp-or-region ()
    "Evaluate an s-expression or a region."
    (interactive)
    (if (evil-visual-state-p)
        (let ((r (evil-visual-range)))
          (eval-region (car r) (cadr r)))
      (eval-last-sexp nil)))
  (defun my-eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
:general
  (general-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "C-c C-c" 'my-eval-sexp-or-region)
  (space-leader
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m f" 'mark-defun
    "p d" 'find-function-at-point
    "E"   'my-eval-and-replace))
(use-package hlinum
  :config
  (hlinum-activate)
  (add-hook 'prog-mode-hook 'linum-mode)
  (remove-hook 'post-command-hook 'hlinum-highlight-region)
  (set-face-attribute 'linum-highlight-face nil
                      :inherit 'linum
                      :foreground "#CAE682"
                      :background "#444444"
                      :weight 'bold))

(use-package comment-dwim-2)

(use-package magit
  :commands (magit-status)
  :init
  (setq magit-branch-arguments nil)
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/local/bin/git"))
  :config
  ;; Start the commit window in insert mode
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :general
  (space-leader
    "g b"   'magit-blame
    "g f h" 'magit-log-buffer-file))

(use-package evil-magit
  :after magit)

(use-package powerline
  :config (powerline-center-evil-theme))

(use-package smooth-scrolling
  :config
  (setq scroll-step              1
        scroll-conservatively    10000
        scroll-margin            1
        smooth-scroll-margin     1
        scroll-up-aggressively   0.0
        scroll-down-aggressively 0.0)
  (setq-default scroll-up-aggressively   0.0)
  (setq-default scroll-down-aggressively 0.0)
  (smooth-scrolling-mode t))

(use-package helm
  :after evil
  :defines (helm-idle-delay
            helm-quick-update
            helm-M-x-requires-pattern
            helm-ff-skip-boring-files
            helm-ff-search-library-in-sexp
            helm-ff-file-name-history-use-recentf)
  :diminish helm-mode
  :init
  (setq helm-candidate-number-limit           100
        helm-idle-delay                       0.0
        helm-input-idle-delay                 0.01
        helm-quick-update                     t
        helm-M-x-requires-pattern             nil
        helm-ff-skip-boring-files             t
        helm-move-to-line-cycle-in-source     t
        helm-split-window-in-side-p           t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)
  :config
  (require 'helm-config)
  (helm-mode 1)

  (defun helm-skip-dots (old-func &rest args)
    "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
    (apply old-func args)

    ;; When doing rgrepping, it is usually preferred to select '.'
    (when (not (equal (buffer-name) "*helm-mode-rgrep*"))
      (let ((sel (helm-get-selection)))
        (if (and (stringp sel) (string-match "/\\.$" sel))
            (helm-next-line 2)))
      (let ((sel (helm-get-selection))) ; if we reached .. move back
        (if (and (stringp sel) (string-match "/\\.\\.$" sel))
            (helm-previous-line 1)))))

  (advice-add #'helm-preselect :around #'helm-skip-dots)
  (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots)
  :bind
  (("M-x" . helm-M-x)
   :map helm-map
   ("[tab]" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-k"   . helm-previous-line)
   ("C-j"   . helm-next-line)
   ("C-l"   . helm-next-source))
  :general
  (space-leader
    ";" 'helm-find-files))

(use-package projectile
  :after helm
  :defines (projectile-completion-system
            projectile-enable-caching
            projectile-use-git-grep)
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'default
        projectile-enable-caching    t
        projectile-use-git-grep      t)
  :config
  (projectile-mode)
  (append-to-list projectile-globally-ignored-directories
                  '(".git" "venv" "build" "dist"))
  (append-to-list projectile-globally-ignored-file-suffixes
                  '("pyc" "jpeg" "jpg" "png"))
  (append-to-list projectile-globally-ignored-files
                  '(".DS_Store")))

(use-package helm-projectile
  :after projectile
  :general
  (space-leader
    "G p" 'projectile-grep
    "G P" 'helm-projectile-grep
    "G r" 'rgrep
    "'"   'projectile-switch-project
    ":"   'helm-projectile-find-file
    "\""  'helm-projectile))


(use-package windmove
  :config
  (defun my-frame-pos-x (frame)
    "Get the x position of the FRAME is display.
On multi-monitor systems the display spans across all the monitors."
    (+ (car (frame-position frame))
       (cadaar
        (display-monitor-attributes-list frame))))

  (defun my-frame-not-current-but-visible-p (frame)
    ;; TODO: frame-visible-p does not work on OS X, so this function returns
    ;;       also frames that are on other virtual desktops.
    (and (frame-visible-p frame)
         (not (eq frame (selected-frame)))))

  (defun my-frame-to (direction)
    "Find next frame to DIRECTION or nil."
    (let* ((current-frame-pos (my-frame-pos-x (selected-frame)))
           (frame-candidates
            (cl-remove-if-not #'my-frame-not-current-but-visible-p
                              (frame-list)))
           (frame-to-left
            (car
             (sort
              (cl-remove-if-not (lambda (frame) (< (my-frame-pos-x frame)
                                                   current-frame-pos))
                                frame-candidates)
              (lambda (a b) (> (my-frame-pos-x a) (my-frame-pos-x b))))))
           (frame-to-right
            (car
             (sort
              (cl-remove-if-not (lambda (frame) (> (my-frame-pos-x frame)
                                                   current-frame-pos))
                                frame-candidates)
              (lambda (a b) (< (my-frame-pos-x a) (my-frame-pos-x b)))))))

      (cond ((eq direction 'left)
             frame-to-left)
            ((eq direction 'right)
             frame-to-right)
            (t (error "Unknown direction")))))

  (defun my-windmove-advice (orig-fun dir &rest args)
    "Extend the range of windmove to go to next and previous frames."
    (condition-case err
        (apply orig-fun dir (cons dir args))
      (user-error
       (if (or (eq dir 'left) (eq dir 'right))
           (progn
             (select-frame-set-input-focus
              (or (my-frame-to dir)
                  (signal (car err) (cdr err))))
             (condition-case err
                 (let ((inverted-dir (if (eq dir 'right) 'left 'right)))
                   (while t
                     ;; Switched frame, go as far as possible to the other
                     ;; direction, user-error is signaled when it hits the frame
                     ;; boundary.
                     (apply orig-fun inverted-dir (cons inverted-dir args))))
               (user-error nil)))
         (signal (car err) (cdr err))))))

  (advice-add 'windmove-do-window-select :around #'my-windmove-advice)

  :bind
  (("C-S-j" . windmove-down)
   ("C-S-k" . windmove-up)
   ("C-S-h" . windmove-left)
   ("C-S-l" . windmove-right)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-indent-textobject
  :after evil)

(use-package evil-visualstar
  :after evil
  :defines (evil-visualstar/persistent)
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :after evil
  :general
  (space-leader
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

(use-package evil-ediff
  :after evil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package evil-indent-plus
  :after evil
  :general
  (general-define-key
    :keymaps '(evil-inner-text-objects-map)
    "i" 'evil-indent-plus-i-indent
    "I" 'evil-indent-plus-i-indent-up
    "J" 'evil-indent-plus-i-indent-up-down)
  (general-define-key
    :keymaps '(evil-outer-text-objects-map)
    "i" 'evil-indent-plus-a-indent
    "I" 'evil-indent-plus-a-indent-up
    "J" 'evil-indent-plus-a-indent-up-down))

(use-package smartparens
  :diminish smartparens-mode
  :after general
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (general-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "C->" 'sp-forward-slurp-sexp)
  (sp-pair "{%" "%}")
  (show-smartparens-global-mode 1))

(use-package org
  :functions (my-org-mode-hook)
  :defines (org-export-async-init-file)
  :init
  (setq org-export-async-init-file (concat user-emacs-directory
                                           "/org-async-init.el"))
  (add-hook 'org-mode-hook #'my-org-mode-hook)
  :config
  (defun my-org-pdf-async ()
    "Perform an async pdf export."
    (interactive)
    (org-latex-export-to-pdf t))
  (defun my-org-mode-hook ()
    (visual-line-mode 1))
  :general
  (space-leader
    :keymaps '(org-mode-map)
    "E" 'my-org-pdf-async))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package fill-column-indicator
  :if (or (daemonp) window-system)
  :functions (on-off-fci-before-company)
  :commands (fci-mode)
  :init
  (setq fci-rule-column 80
        fci-rule-color "#195466"
        fci-rule-image-format 'pbm)
  (add-hook 'prog-mode-hook #'fci-mode)
  :config
  ;; fci-mode conflicts with company-dialogs. Temporarily disable fci when
  ;; company-dialog is visible.
  (defun on-off-fci-before-company (command)

    ;; While yasnippet is expanding, the fci-mode is already disabled, and it
    ;; should not be enabled before snippet expanding is done.
    (unless (and (boundp 'my-yas-expanding) my-yas-expanding)
      (when (derived-mode-p 'prog-mode)
        (when (string= "show" command)
          (turn-off-fci-mode))
        (when (string= "hide" command)
          (turn-on-fci-mode)))))
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company))

(use-package eshell
  :commands (my-eshell-here)
  :functions (my-eshell-hook)
  :defines (eshell-banner-message eshell-cmpl-cycle-completions)
  :init
  (setq eshell-banner-message         ""
        eshell-cmpl-cycle-completions nil
        pcomplete-cycle-completions   nil)
  (add-hook 'eshell-mode-hook #'my-eshell-hook)

  :general
  (space-leader
    :keymaps '(eshell-mode-map)
    "h" 'helm-eshell-history)
  :config
  ;; bug#18951: complete-at-point removes an asterisk when it tries to
  ;;            complete. Disable idle completion until resolved.
  (setq-mode-local eshell-mode
                   (company-idle-delay . nil)
                   (company-backends   . '((company-shell company-capf))))

  (defun my-eshell-history ()
    (interactive)
    (my-eshell-go-to-prompt)
    (eshell-bol)
    ;; If the line is not empty, kill the rest of the line.
    (when (not (looking-at "$"))
      (kill-line nil))
    (call-interactively 'helm-eshell-history))
  (defun my-eshell-hook ()
    (general-define-key
      :states '(normal)
      :keymaps '(eshell-mode-map)
      :prefix "SPC"
      "h" 'my-eshell-history)
    (general-define-key
      :keymaps 'eshell-mode-map
      "C-S-q" 'my-quit-eshell)
    (general-define-key
      :states '(normal)
      :keymaps '(eshell-mode-map)
      "i" 'my-eshell-go-to-prompt
      "I" 'my-eshell-insert-beginning-of-line
      "0" 'eshell-bol))

  (defun my-eshell-here ()
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  (defun my-quit-eshell ()
    (interactive)
    (eshell-life-is-too-much)
    (delete-window))

  (defun my-eshell-within-command-p ()
    "Check if point is at the command prompt."
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (let ((v (>= p (point))))
        (goto-char p)
        v)))

  (defun my-eshell-go-to-prompt ()
    "Puts point to the end of the prompt."
    (interactive)
    (if (my-eshell-within-command-p)
        (evil-insert-state)
      (progn
        (evil-goto-line)
        (evil-append-line 1))))

  (defun my-eshell-insert-beginning-of-line ()
    "Puts point to eshell-bol and enters insert mode."
    (interactive)
    (eshell-bol)
    (evil-insert-state t)))

(use-package company-shell
  :after eshell
  :config

  (add-hook 'eshell-mode-hook #'company-mode)
  (add-hook 'eshell-mode-hook #'my-eshell-hook))

(use-package eclim
  :defines (company-emacs-eclim-ignore-case
            eclim-print-debug-messages
            help-at-pt-timer-delay)
  :init
  (setq eclimd-executable               "/Applications/Eclipse.app/Contents/Eclipse/eclimd"
        eclim-executable                "/Applications/Eclipse.app/Contents/Eclipse/eclim"
        eclimd-default-workspace        "~/Documents/workspace"
        help-at-pt-display-when-idle    t
        company-emacs-eclim-ignore-case nil
        eclim-print-debug-messages      nil ; Set to t to enable logging
        help-at-pt-timer-delay          0.1)
  (setq-mode-local java-mode
                   (indent-tabs-mode . nil)
                   (tab-width        . 4)
                   (company-backends . '((company-emacs-eclim))))
  (defun my-java-hook ()
    (help-at-pt-set-timer)
    (eclim-mode))
  (add-hook 'java-mode-hook #'my-java-hook)
  :general
  (space-leader
    :keymaps '(java-mode-map)
    "p d"   'eclim-java-find-declaration
    "p r f" 'eclim-problems-correct
    "p r r" 'eclim-java-refactor-rename-symbol-at-point))

(use-package company-emacs-eclim
  :after eclim)

(use-package rtags
  :functions (my-c-mode-hook)
  :defines (rtags-use-helm)
  :commands (my-c-mode-hook)
  :init
  (setq rtags-use-helm                 t
        rtags-enable-unsaved-reparsing t
        rtags-rc-log-enabled           t) ; Set to t to enable logging
  (setq-default c-basic-offset         4)
  (add-hook 'c-mode-common-hook #'my-c-mode-hook)
  :config
  (require 'helm-rtags)
  (defun my-c-mode-hook ()
    (require 'flycheck-rtags)
    (flycheck-select-checker 'rtags))

  (defun my-rtags-switch-to-project ()
    "Set active project without finding a file."
    (interactive)
    (let ((projects nil)
          (project nil)
          (current ""))
      (with-temp-buffer
        (rtags-call-rc :path t "-w")
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (point-at-bol) (point-at-eol))))
            (cond ((string-match "^\\([^ ]+\\)[^<]*<=$" line)
                   (let ((name (match-string-no-properties 1 line)))
                     (setq projects (add-to-list 'projects name t))
                     (setq current name)))
                  ((string-match "^\\([^ ]+\\)[^<]*$" line)
                   (setq projects
                         (add-to-list 'projects
                                      (match-string-no-properties 1 line))))
                  (t)))
          (forward-line)))
      (setq project (completing-read
                     (format "RTags select project (current is %s): " current)
                     projects))
      (when project
        (with-temp-buffer
          (rtags-call-rc :path t "-w" project)))))

  (setq-mode-local c++-mode
                   (indent-tabs-mode                    . nil)
                   (tab-width                           . 4)
                   (flycheck-highlighting-mode          . nil)
                   (flycheck-check-syntax-automatically . nil)
                   (company-backends                    . '((company-rtags))))
  (setq-mode-local c-mode
                   (indent-tabs-mode                    . nil)
                   (tab-width                           . 4)
                   (flycheck-highlighting-mode          . nil)
                   (flycheck-check-syntax-automatically . nil))

  :general
  (space-leader
    :keymaps '(c++-mode-map c-mode-map)
    "p d"   'rtags-find-symbol-at-point
    "p r"   'rtags-rename-symbol
    "p u"   'rtags-find-references-at-point
    "p s t" 'rtags-symbol-type
    "p s i" 'rtags-symbol-info
    "p v"   'my-rtags-switch-to-project
    "m f"   'c-mark-function))

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after 'irony
  :config
  (setq-mode-local c++-mode
                   (company-backends . '((company-irony))))
  (setq-mode-local c-mode
                   (company-backends . '((company-irony)))))

(use-package ttymenu
  :ensure nil
  :load-path "~/projects/elisp/ttymenu"
  :commands (ttymenu-display-menus)
  :config
  (general-define-key
    :keymaps '(ttymenu-mode-map)
    :states '(normal)
    "l" 'ttymenu-next-day
    "h" 'ttymenu-previous-day
    "j" 'ttymenu-next-restaurant
    "k" 'ttymenu-previous-restaurant
    "q" 'ttymenu-close))

(use-package dired
  :ensure nil
  :general
  (space-leader
    :keymaps '(dired-mode-map)
    "E" 'dired-toggle-read-only))

(use-package compilation
  :ensure nil
  :after helm
  :general
  (general-define-key
    :keymaps '(compilation-mode-map)
    "SPC" nil
    "h"   nil
    "g"   nil)

  ;; Compilation-mode maps need to be filled separately, since it overrides
  ;; most of the keybindings.
  (general-define-key
    :keymaps '(compilation-mode-map)
    ";" 'helm-find-files
    "'" 'helm-projectile-switch-project)
  (general-define-key
    :keymaps '(compilation-mode-map)
    :prefix "SPC"
    "b"   'helm-mini
    "x"   'helm-M-x
    "ö"   'helm-projectile
    "O"   'helm-occur
    "A"   'helm-apropos
    "w"   'save-buffer
    "SPC" 'ace-window
    "D"   'kill-this-buffer
    "s h" 'my-eshell-here
    "r"   'compilation-recompile
    "g"   'magit-status
    "0"   'delete-window
    "1"   'delete-other-windows
    "2"   'split-window-below
    "3"   'split-window-right))

(use-package json-mode)
(use-package gitignore-mode)
(use-package coffee-mode)
(use-package jade-mode)

(use-package neotree
  :general
  (space-leader
    "n t" 'neotree-projectile-action)
  :init
  (setq neo-window-width 45
        neo-theme 'ascii
        neo-hidden-regexp-list '("^\\.$" "^\\.\\.$" "\\.pyc$" "~$" "^#.*#$"
                                 "\\.elc$" "^\\.git"))
  :general
  (general-define-key
    :states '(normal)
    :keymaps '(neotree-mode-map)
    "C"        'neotree-change-root
    "U"        'neotree-select-up-node
    "r"        'neotree-refresh
    "o"        'neotree-enter
    "<return>" 'neotree-enter
    "i"        'neotree-enter-horizontal-split
    "s"        'neotree-enter-vertical-split
    "n"        'evil-search-next
    "N"        'evil-search-previous
    "m a"      'neotree-create-node
    "m c"      'neotree-copy-file
    "m d"      'neotree-delete-node
    "m m"      'neotree-rename-node
    "g g"      'evil-goto-first-line)
  :bind
  (("<f8>" . neotree-toggle)))

(use-package nxml-mode
  :mode "\\.xml\\'"
  :ensure nil
  :config
  (defun my-xml-format ()
    "Format an XML buffer with `xmllint'."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             "xmllint -format -"
                             (current-buffer) t
                             "*Xmllint Error Buffer*" t))
  :general
  (space-leader
    :keymaps '(nxml-mode-map)
    "I" 'my-xml-format))

(use-package pdf-tools
  :defines (pdf-info-epdfinfo-program)
  :init
  (setq with-editor-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
        pdf-info-epdfinfo-program          "/usr/local/bin/epdfinfo"))

(use-package zoom-frm
  :init
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

;;; init.el ends here
