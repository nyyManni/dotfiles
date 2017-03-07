;;; init.el --- Emacs configuration

;; Copyright (c) 2016 Henrik Nyman

;; Author     : Henrik Nyman <henrikjohannesnyman@gmail.com>
;; Created    : 10 Aug 2016
;; Modified   : 26 Feb 2017
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

;;; Code:


;; Global settings
(setq user-full-name                       "Henrik Nyman"
      user-login-name                      "hnyman"
      user-mail-address                    "henrik.nyman@optofidelity.com"
      user-emacs-directory                 "~/.emacs.d"
      vc-follow-symlinks                   t

      use-package-always-ensure            t
      use-package-check-before-init        t

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
      tab-width                            2)

(setq-default indent-tabs-mode nil)

;; OS X specific settings
(when (eq system-type 'darwin)
  (setq exec-path                          (append exec-path '("/usr/local/bin"))
        with-editor-emacsclient-executable "/usr/local/Cellar/emacs/25.1/bin/emacsclient"
	pdf-info-epdfinfo-program          "/usr/local/bin/epdfinfo"
	default-input-method               "MacOSX"
	mac-command-modifier               'meta
	mac-option-modifier                nil
	mac-allow-anti-aliasing            t
	mac-command-key-is-meta            t
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


;; Setup use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

(use-package gotham-theme
  :demand
  :if window-system
  :init
  (global-unset-key (kbd "C-z"))
  :config
  (load-theme 'gotham t)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
	(lambda (frame)
	  (load-theme 'gotham t)
	  (scroll-bar-mode -1)
	  (powerline-reset)))))


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
          #'(lambda (arg) `(set (make-local-variable ',(car arg)) ,(cdr arg))) args))))

(defun is-current-file-tramp ()
  "Check if the file is a remote tramp file."
  (tramp-tramp-file-p (buffer-file-name (current-buffer))))

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
    "e"    'eval-last-sexp
    "w"    'save-buffer
    "SPC"  'ace-window
    "D"    'kill-this-buffer
    "a a"  'align-regexp
    "s u"  'my-sudo-at-point
    "s h"  'my-eshell-here
    "s '"  'my-split-string-single-quote
    "s \"" 'my-split-string-double-quote
    "r"    'my-reload-file
    "f"    'helm-imenu
    "g"    'magit-status
    "S"    'delete-trailing-whitespace
    "i"    'indent-region
    "0"    'delete-window
    "1"    'delete-other-windows
    "2"    'split-window-below
    "3"    'split-window-right))

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

  ;; Activating window with mouse turning on visual state is super annoying.
  ;; Still keeping up-mouse-1 to be able to move cursor by clicking.
  (global-unset-key [drag-mouse-1])
  (global-unset-key [down-mouse-1])
  (general-define-key :keymaps '(evil-motion-state-map) [down-mouse-1] nil)

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

  (defun my-split-string-single-quote ()
    "Split a string delimited with single quotes at point."
    (interactive)
    (insert "''")
    (backward-char)
    (evil-insert-state))

  (defun my-split-string-double-quote ()
    "Split a string delimited with double quotes at point."
    (interactive)
    (insert "\"\"")
    (backward-char)
    (evil-insert-state))

  (evil-mode t))

(use-package key-chord
  :after general
  :config
  (general-define-key
    :keymaps '(evil-insert-state-map evil-visual-state-map)
    (general-chord "jk") 'evil-normal-state
    (general-chord "kj") 'evil-normal-state)
  (key-chord-mode t))

(use-package company
  :after evil
  :functions (is-empty-line-p my-complete-or-indent)
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
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

  ;; Use C-' and C-* for going through the fields, since they are positioned
  ;; nicely on a nordic keyboard.
  (general-define-key
    :states '(insert)
    "C-'" 'yas-expand)
  (general-define-key
    :keymaps '(yas-keymap)
    "C-'" 'yas-next-field-or-maybe-expand
    "C-*" 'yas-prev-field))

(use-package flycheck
  :diminish
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :functions (my-python-hook my-ipython-hook my-jedi-show-doc
	      my-python-change-venv)
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
                   (company-backends . '((company-jedi company-capf))))
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

  (add-hook 'inferior-python-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'my-python-hook)

  (general-define-key
    :states '(insert)
    :keymaps '(python-mode-map)
    "C-<tab>" 'jedi:get-in-function-call)

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

  :general
  (space-leader
    :keymaps '(python-mode-map)
    "p v"  'my-python-change-venv
    "p d"  'jedi:goto-definition
    "p ?"  'jedi:show-doc
    "p r"  'run-python
    "m f"  'python-mark-defun
    "e"    'my-python-send-region-or-buffer))

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
  :config
  ;; Start the commit window in insert mode
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package evil-magit
  :after magit)

(use-package powerline
  :config (powerline-center-evil-theme))

(use-package smooth-scrolling
  :config
  (setq scroll-step              1
        scroll-conservatively    10000
        scroll-margin            5
        smooth-scroll-margin     5
        scroll-up-aggressively   0.0
        scroll-down-aggressively 0.0)
  (setq-default scroll-up-aggressively   0.0)
  (setq-default scroll-down-aggressively 0.0))

(use-package helm
  :after evil
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
  :bind
  (("M-x" . helm-M-x)
   :map evil-normal-state-map
   ("ö" . helm-find-files)
   :map helm-map
   ("[tab]" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-k"   . helm-previous-line)
   ("C-j"   . helm-next-line)))

(use-package projectile
  :after helm
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'default
        projectile-enable-caching    t
        projectile-use-git-grep      t)
  :config
  (projectile-global-mode)
  (append-to-list projectile-globally-ignored-directories
                  '(".git" "venv" "build" "dist"))
  (append-to-list projectile-globally-ignored-file-suffixes
                  '("pyc" "jpeg" "jpg" "png"))
  (append-to-list projectile-globally-ignored-files
                  '(".DS_Store")))

(use-package helm-projectile
  :after projectile
  :general
  (general-define-key :states '(normal) "ä" 'helm-projectile)
  (space-leader
    "ö" 'helm-projectile))

(use-package windmove
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
  :if window-system
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
  :defines (eshell-banner-message)
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
    ;; If the lien is not empty, kill the rest of the line.
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
  :init
  (setq eclimd-executable            "/Applications/Eclipse.app/Contents/Eclipse/eclimd"
	eclim-executable             "/Applications/Eclipse.app/Contents/Eclipse/eclim"
	help-at-pt-display-when-idle t
        eclim-print-debug-messages   nil ; Set to t to enable logging
	help-at-pt-timer-delay       0.1)
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
    "p r r" 'eclim-java-refactor-rename-symbol-at-point))

(use-package company-emacs-eclim
  :after eclim)

(use-package rtags
  :functions (my-c-mode-hook)
  :init
  (setq rtags-use-helm                 t
	rtags-enable-unsaved-reparsing t
	rtags-rc-log-enabled           t) ; Set to t to enable logging
  (setq-default c-basic-offset         4)
  :config
  (require 'rtags-helm)
  (defun my-c-mode-hook ()
    (require 'flycheck-rtags)
    (flycheck-select-checker 'rtags))

  (defun my-rtags-switch-to-project ()
    "Set active project."
    (interactive)
    (let ((projects nil)
          (project nil)
          (current ""))
      (with-temp-buffer
        (rtags-call-rc :path t "-w")
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (cond ((string-match "^\\([^ ]+\\)[^<]*<=$" line)
                   (let ((name (match-string-no-properties 1 line)))
                     (setq projects (add-to-list 'projects name t))
                     (setq current name)))
                  ((string-match "^\\([^ ]+\\)[^<]*$" line)
                   (setq projects (add-to-list 'projects (match-string-no-properties 1 line))))
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
                   (flycheck-check-syntax-automatically . nil)
                   (company-backends                    . '((company-rtags))))

  (add-hook 'c-mode-common-hook #'my-c-mode-hook)
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

(use-package ttymenu
  :ensure nil
  :load-path "~/projects/elisp/ttymenu"
  :commands (ttymenu-display-menus)
  :config
  (evil-define-key 'normal ttymenu-mode-map (kbd "l") 'ttymenu-next-day)
  (evil-define-key 'normal ttymenu-mode-map (kbd "h") 'ttymenu-previous-day)
  (evil-define-key 'normal ttymenu-mode-map (kbd "q") 'ttymenu-close))

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
    "ö" 'helm-find-files
    "ä" 'helm-projectile-switch-project)
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

;;; init.el ends here
