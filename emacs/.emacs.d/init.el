;; init.el --- -*- lexical-binding: t -*-
;; Copyright (c) 2016 - 2023 Henrik Nyman


;; Author     : Henrik Nyman <h@nyymanni.com>
;; Created    : 10 Aug 2016
;; Version    : 0.4

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

;; nyyManni's configuration for Emacs, 2023 flavor.

;;; Code:


(setq user-full-name       "Henrik Nyman"
      user-login-name      "hnyman"
      user-mail-address    "h@nyymanni.com"
      user-emacs-directory "~/.emacs.d"

      vc-follow-symlinks                t
      inhibit-startup-screen            t
      initial-scratch-message           ""
      load-prefer-newer                 t

      inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      sentence-end-double-space nil

      read-extended-command-predicate #'command-completion-default-include-p

      backup-directory-alist         '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t))

      custom-file          (concat user-emacs-directory "/customize-ignored.el")
      delete-old-versions  -1
      version-control      t
      display-line-numbers-type 'relative
      vc-make-backup-files t
      tab-width            2
      tab-always-indent    'complete

      ring-bell-function   'ignore

      frame-title-format   '("" "Emacs v" emacs-version))

(eval-after-load 'warnings
  (setq warning-minimum-level :error))

(when (eq system-type 'gnu/linux)
  (defvar ns-command-modifier nil)
  (defvar ns-option-modifier nil)
  (defvar ns-antialias-text nil)
  (defvar ns-use-srgb-colorspace nil)
  (setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket")))
(when (eq system-type 'darwin)

  ;; macOS does not inherit the shell environment
  (setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs"))

  (setq exec-path                     (append exec-path '("/usr/local/bin"))
        default-input-method          "MacOSX"
        default-directory             "/Users/hnyman/"
        ns-command-modifier           'control
        read-process-output-max       (* 1024 1024)
        ns-control-modifier           'meta
        ns-option-modifier            nil
        ns-antialias-text             t
        ns-use-srgb-colorspace        nil
        frame-resize-pixelwise        t
        mouse-wheel-scroll-amount     '(5 ((shift) . 5) ((control)))
        mouse-wheel-progressive-speed nil)
  )

(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed in emacs#95692f6")

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(defvar my-ejira-username      "")
(defvar my-ejira-projects      '())
(defvar my-ejira-server        "https://localhost:8080")
(defvar my-ejira-kanban-boards nil)
(defvar my-ejira-scrum-boards  nil)
(defvar my-misc-org-file       "~/org/MISC.org")
(defvar my-jira-directory      (expand-file-name "~/JIRA"))

(dolist (conf (file-expand-wildcards "~/[a-zA-Z]*/.emacs-init.el"))
  (message "loading configurations from: %s" conf)
  (load-file conf))


(defvar user-hostname (shell-command-to-string "hostname |sed 's/\\.local//' |tr -d '\n'"))
(fset 'startup-echo-area-message (lambda () ""))


(setq-default indent-tabs-mode      nil
              fill-column           80
              comint-process-echoes t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-unset-key (kbd "C-/"))  ;; evil sets undo to 'u'
(global-unset-key (kbd "C-?"))  ;; evil sets redo to 'C-r'
(when window-system
  (global-unset-key (kbd "C-z"))
  (blink-cursor-mode 0)
  (setq confirm-kill-emacs 'y-or-n-p))


(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(set-face-attribute 'fill-column-indicator nil :height 1.1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'latex-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)

(when (eq system-type 'darwin)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package auto-compile
  :defer nil
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(when (eq system-type 'darwin)
  (when (memq window-system '(mac ns x))
    (use-package exec-path-from-shell
      :demand t
      :config
      (exec-path-from-shell-initialize))))

(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package gotham-theme
  :demand
  :if (or (daemonp) window-system)
  :config
  (load-theme 'gotham t)

  (set-face-attribute 'line-number-current-line nil
                      :inherit 'line-number
                      :foreground "#CAE682"
                      :background "#444444"
                      :weight 'bold)

  (eval-after-load 'flymake
    (progn
      (require 'flymake)
    (set-face-attribute
     'flymake-error nil
     :underline `(:style wave :color ,(face-attribute 'error :foreground)))

    (set-face-attribute
     'flymake-warning nil
     :underline `(:style wave :color ,(face-attribute 'warning :foreground)))

    (set-face-attribute
     'flymake-note nil
     :underline `(:style wave :color ,(face-attribute 'success :foreground))))))


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :demand
  :init
  (setq doom-modeline-height 18)
  :config

  (defun my-get-python-version ()
    '("python" "--version"))
  (setq doom-modeline-env-python-command #'my-get-python-version))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; Evil
(defconst my/leader "SPC")
(use-package general
  :config
  (general-define-key
   :states 'motion
   "SPC" nil)
  (general-define-key
   :keymaps '(diff-mode-map)
   :states '(motion normal)
   "SPC" nil)
  (general-create-definer leader-def-key
    :states 'motion
    :prefix my/leader
    :prefix-map 'leader-map)



  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

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
    (general-define-key
     :keymaps '(evil-motion-state-map evil-normal-state-map)
     key nil))

  ;; (general-define-key :keymaps '(evil-motion-state-map)
  ;;                     "<tab>" nil)

  ;; Disable arrow key movement
  (dolist (key '("<left>" "<right>" "<up>" "<down>"))
    (general-define-key :keymaps '(evil-motion-state-map) key nil)
    (global-unset-key (kbd key)))
  (global-set-key (kbd "M-?") 'help-command)
  (global-set-key (kbd "M-?") nil)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)

  (general-define-key "<f11>" nil) ; Disable toggling fullscreen with f11

  (global-set-key (kbd "C-S-u") 'universal-argument)
  (define-key universal-argument-map (kbd "C-S-u") 'universal-argument-more)

  ;; Disable SPC bindings in modes where it interferes with the leader key
  (general-def
    :keymaps '(compilation-mode-map dired-mode-map)
    "SPC" nil)

  (defun my-dired-here ()
    (interactive)
    (dired (f-dirname (buffer-file-name))))

  (leader-def-key
    :keymaps 'override
    :states 'normal
    "A"   'describe-symbol
    "b"   'switch-to-buffer
    "x"   'execute-extended-command
    "'"   'project-switch-project
    ";"   'find-file
    ":"   'project-find-file
    "a a" 'align-regexp
    "a f" 'describe-function
    "a s" 'describe-symbol
    "a v" 'describe-variable
    "a k" 'describe-key
    "s w" 'whitespace-mode
    "e"   'eval-last-sexp
    "D"   'kill-this-buffer
    "Y"   'my-put-file-name-on-clipboard
    "f f" 'my-dired-here
    "l p" 'package-list-packages
    "s l" 'sort-lines
    "r"   'my-reload-file
    "i"   'indent-region
    "I"   (lambda () (interactive) (find-file user-init-file))
    "S"   'delete-trailing-whitespace))

(defun my-reload-file ()
  "Reopen current file, preserving location of point."
  (interactive)
  (let ((p (point)))
    (find-alternate-file buffer-file-name)
    (goto-char p)))

(defun my-open-lower-third (command &rest args)
  "Open a buffer and run a COMMAND with ARGS in the lower third of the window."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t))))
         (bufname (concat (symbol-name command) " " name))
         (firstp t))
    (while (get-buffer (concat bufname "*"))
      ;; Buffer with name already exists
      (message "buffer already taken")
      (setq bufname (if firstp
                        (progn (setq firstp nil)
                               (concat bufname "<2>"))

                      ;; Increment the number
                      (let* ((regex "\\(.*<\\)\\([0-9]+\\)\\(>\\)" )
                             (num (string-to-number
                                   (replace-regexp-in-string regex "\\2" bufname))))
                        (replace-regexp-in-string
                         regex (concat "\\1" (number-to-string (1+ num)) "\\3") bufname)))))
    (split-window-vertically (- height))
    (other-window 1)
    (apply command args)
    (rename-buffer (concat bufname "*"))))

(use-package direnv
  :defer nil
  ;; :after lsp-mode
  :config
  (direnv-mode)
  (advice-add 'eglot-ensure :before #'direnv-update-environment)
  )

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump nil)
  (setq evil-want-fine-undo nil)
  (setq evil-want-keybinding nil)

  :custom ((evil-undo-system 'undo-tree)
           (evil-ex-search-persistent-highlight t))
  :config
  (general-define-key
   :keymaps '(evil-normal-state-map)
   "M-." nil)

  (evil-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :defer nil
  :general
  (leader-def-key
    "u" 'undo-tree-visualize))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package key-chord
  :hook (after-init . (lambda () (key-chord-mode t)))
  :custom
  ;; To overcome regression from:
  ;; https://github.com/emacsorphanage/key-chord/commit/e724def60fdf6473858f2962ae276cf4413473eb
  (key-chord-safety-interval-forward 0)
  (key-chord-safety-interval-backward 0)
  :general
  (dolist (chord '("jk" "kj" "JK" "KJ" "jK" "kJ" "Jk" "Kj"))
    (general-define-key
     :keymaps '(evil-insert-state-map evil-visual-state-map)
     (general-chord chord) 'evil-normal-state)))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :functions (global-evil-surround-mode)
  :config
  (global-evil-surround-mode))

(use-package evil-indent-plus
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

(use-package evil-args
  :general
  (general-define-key
   :keymaps '(evil-inner-text-objects-map)
   "a" 'evil-inner-arg)
  (general-define-key
   :keymaps '(evil-outer-text-objects-map)
   "a" 'evil-outer-arg)
  (general-define-key
   :keymaps '(evil-normal-sate-map evil-motion-state-map)
   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)
  (general-define-key
   :keymaps '(evil-normal-sate-map)
   "K" 'evil-jump-out-args))

(use-package evil-exchange
  :general
  (general-define-key
   :keymaps '(evil-normal-state-map evil-visual-state-map)
   "g x" 'evil-exchange
   "g X" 'evil-exchange-cancel))

(use-package evil-numbers
  :general
  (general-define-key
   :states '(normal)
   "C-a" 'evil-numbers/inc-at-pt
   "C-z" 'evil-numbers/dec-at-pt))

(use-package evil-visualstar
  :defines (evil-visualstar/persistent)
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode 1))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-{" . vertico-previous-group)
              ("C-}" . vertico-next-group)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :general
  (leader-def-key
    "G P" 'consult-ripgrep
    "y"   'consult-yank-from-kill-ring))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)
   ) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package windmove
  :ensure nil
  :functions (my-frame-pos-x my-frame-not-current-but-visible-p
                             my-frame-to my-frame-center-pos my-windmove-advice)
  :general
  (leader-def-key
    :keymaps 'override
    "0"   'delete-window
    "1"   'delete-other-windows
    "2"   'split-window-below
    "3"   'split-window-right)
  :bind (("C-S-j" . 'windmove-down)
         ("C-S-k" . 'windmove-up)
         ("C-S-h" . 'windmove-left)
         ("C-S-l" . 'windmove-right)))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  (toml-mode . smartparens-mode)
  :init
  (setq-default sp-escape-quotes-after-insert nil)
  :general
  (general-define-key
   :keymaps '(emacs-lisp-mode-map
              lisp-interaction-mode-map
              python-mode-map
              python-ts-mode-map
              c-mode-map c++-mode-map
              c-ts-mode-map c++-ts-mode-map)
   "C->" 'sp-forward-slurp-sexp)
  :config
  (require 'smartparens-config)

  ;; Fix for tree-sit mode
  (sp-with-modes 'python-ts-mode
    (sp-local-pair "'" "'" :unless '(sp-in-comment-p sp-in-string-quotes-p) :post-handlers '(:add sp-python-fix-tripple-quotes))
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
    (sp-local-pair "'''" "'''")
    (sp-local-pair "\\'" "\\'")
    (sp-local-pair "\"\"\"" "\"\"\"")
    (sp-local-pair "(" ")" :post-handlers '(:add sp-python-maybe-add-colon-python))))

(use-package all-the-icons)


(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)       ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)

  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p nil))


(use-package sideline
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
                                              ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake)

(use-package flymake
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . flymake-mode))

(use-package magit
  :defines (magit-branch-arguments magit-git-executable)
  :init
  (setq magit-branch-arguments nil)
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/bin/git"))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :general
  (general-define-key
   :keymaps 'magit-blob-mode-map
   "p" nil
   "n" nil
   "b" nil
   "r" nil
   "f" nil)
  (leader-def-key
    :keymaps 'override
    "g g"   'magit-status
    "g b"   'magit-blame
    "g f h" 'magit-log-buffer-file))

;; LSP

(use-package xref
  :straight (:type built-in)
  :init
  (setq xref-prompt-for-identifier nil)
  :bind
  (("M-?" . xref-find-references)))

(use-package eglot
  :straight (:type built-in)
  :hook
  (python-ts-mode . eglot-ensure)
  (java-mode . eglot-ensure)
  (rustic-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  :init
  (setq-default eglot-workspace-configuration
                '((pylsp
                   (plugins
                    (pylint (enabled . t))
                    (jedi_completion (fuzzy . t))
                    (pydocstyle
                     (enabled . nil)
                     (addIgnore . "D401"))
                    (mccabe (enabled . nil))
                    (pycodestyle (enabled . nil))
                    (pyls_isort (enabled . t))
                    (autopep8 (enabled . nil))
                    (flake8 (enabled . nil))
                    (pyflakes (enabled . nil))
                    (yapf (enabled . nil))
                    (rope_autoimport (enabled . nil))
                    (rope_completion (enabled . nil))
                    (black (enabled . t))
                    (ruff (enabled . nil))
                    (mypy
                     (enabled . t)
                     (live_mode . t)
                     (strict . nil))))))

  :general
  (leader-def-key
    :keymaps 'eglot-mode-map
    "F F" 'eglot-format))

(use-package markdown-mode)

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(use-package py-isort
  :custom ((py-isort-options '("--sl" "-p" "optofidelity")))
  :general
  (leader-def-key
    :keymaps '(python-mode-map python-ts-mode-map)
    "s i"   'py-isort-buffer))


;; C/C++
(setq-default c-basic-offset 4)
(use-package clang-format)
(use-package clang-format+
  :hook
  (c-mode-common . clang-format+-mode)
  (c-ts-mode . clang-format+-mode)
  (c++-ts-mode . clang-format+-mode))
(use-package glsl-mode)
(use-package cmake-mode)


;; RUST

(use-package rust-mode)

(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
  :config
  (define-derived-mode rustic-mode rust-ts-mode "Rustic"
    "Major mode for Rust code.
\\{rustic-mode-map}"
    :group 'rustic

    ;; (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
    ;;   (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t))

    (add-hook 'before-save-hook 'rustic-before-save-hook nil t)
    (add-hook 'after-save-hook 'rustic-after-save-hook nil t))

  (require 'rust-ts-mode)
  ;; (require 'rustic-ts-mode)
  (setq auto-mode-alist (remove '("\\.rs\\'" . rustic-mode) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  )


(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts"))

;; (use-package js-ts-mode
;;   :ensure nil
;;   :mode ("\\.js"))

(use-package prettier-js
  :hook ((js2-mode rsjx-mode) . prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all")))

;; C#

(use-package lua-mode)

;; EShell
(use-package eshell
  :straight (:type built-in)
  :defines (eshell-banner-message eshell-cmpl-cycle-completions
                                  eshell-modify-global-environment eshell-prompt-regexp
                                  eshell-prompt-function)
  :functions (eshell-life-is-too-much eshell/whoami eshell/pwd esh-prompt-func
                                      my-eshell-hook)
  :commands (my-eshell-here)
  :init
  (setq eshell-banner-message            ""
        eshell-cmpl-cycle-completions    nil
        pcomplete-cycle-completions      nil
        eshell-modify-global-environment t)
  :config
  (defun my-eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (my-open-lower-third 'eshell "new"))

  (defun my-quit-eshell ()
    (interactive)
    (eshell-life-is-too-much)
    (delete-window))

  (defun esh-prompt-func ()
    (concat
     "┌─" (eshell/whoami) "@" user-hostname " " (abbreviate-file-name (eshell/pwd)) "\n"
     "└─$ "))
  (setq eshell-prompt-regexp "└─\\$ ")   ; or "└─> "
  (setq eshell-prompt-function #'esh-prompt-func)

  ;; Only trigger company manually inside eshell
  (defun my-eshell-hook ()
    (setq-local company-idle-delay nil))
  (add-hook 'eshell-mode-hook #'my-eshell-hook)

  (defun my-eshell-up ()
    "Go one directory up in the directory tree and print a new prompt."
    (interactive)
    (eshell/cd "..")
    (eshell-kill-input)
    (eshell-send-input)  ;; Send an empty input to get a new prompt
    (yank))

  :general
  (general-define-key
   :keymaps 'eshell-mode-map
   "C-." 'my-eshell-up
   "C-S-x" 'my-quit-eshell)
  (leader-def-key
    :keymaps 'override
    "s e" 'my-eshell-here))

;; (use-package lsp-java)
(use-package eglot-java
  :init
  (setq eglot-java-server-install-dir (expand-file-name "~/.emacs.d/share/eclipse.jdt.ls")))

(use-package web-mode
  :mode ("\\.jsp"))

(use-package systemd)
(use-package jinja2-mode)
(use-package apt-sources-list)

(use-package package-lint)
;; (use-package flycheck-package)

(use-package http)
(use-package qml-mode
  :mode ("\\.qmlproject"))

(use-package json-mode)

(use-package wdired)

(use-package dired+)

(use-package systemd)
(use-package meson-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package dockerfile-mode)
(use-package csv-mode)

(use-package powershell)

(use-package groovy-mode)
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package ediff
  :straight (:type built-in)

  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))


(use-package org
  :ensure org
  :straight (:type built-in)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-capture-mode . evil-insert-state)
  :init
  (defun my-org-mode-hook ()
    (setq evil-shift-width 2))
  (add-hook 'org-mode-hook #'my-org-mode-hook)

  (defun my-pop-to-temp-org-buffer ()
    (interactive)
    (pop-to-buffer "*scratch.org*")
    (unless (derived-mode-p 'org-mode)
      (org-mode)))

  (setq org-export-async-init-file             (concat user-emacs-directory
                                                       "/org-async-init.el")
        org-confirm-babel-evaluate             nil
        org-tags-column                        -100
        org-clock-history-length               23
        org-agenda-restore-windows-after-quit  t
        org-clock-in-resume                    t
        org-drawers                            '("PROPERTIES" "LOGBOOK")
        org-clock-into-drawer                  t
        org-clock-out-remove-zero-time-clocks  t
        org-clock-out-when-done                t
        org-clock-persist                      t
        org-clock-persist-query-resume         nil
        org-clock-auto-clock-resolution        'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-time-stamp-rounding-minutes        '(1 1)
        org-use-fast-todo-selection            t

        org-indirect-buffer-display            'other-window

        org-lowest-priority    ?G
        org-priority-faces     '((?A . (:foreground "#c23127" :weight 'bold))
                                 (?B . (:foreground "#c23127"))
                                 (?C . (:foreground "#d26937"))
                                 (?E . (:foreground "#2aa889"))
                                 (?F . (:foreground "gray"))
                                 (?G . (:foreground "gray")))
        ;; org-agenda-files       '("~/JIRA" "~/org")
        org-agenda-files (append (file-expand-wildcards "~/org")
                                 (file-expand-wildcards "~/[a-zA-Z]*/org")
                                 (file-expand-wildcards "~/[a-zA-Z]*/org/.jira-data"))


        org-agenda-sticky      t
        org-todo-keywords
        '((sequence "BLOG(b)" "TODO(t)" "NEXT(p)" "TEST" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "|" "CANCELLED(c@/!)" "MEET(m)" "NOTE(n)"))

        org-todo-keyword-faces '(("BLOG" :foreground "#343a40"    :weight bold)
                                 ("TODO" :foreground "#c23127"    :weight bold)
                                 ;; ("NEXT" :foreground "#d26937e"   :weight bold)
                                 ("NEXT" :foreground "white"      :weight bold)
                                 ("TEST" :foreground "light blue" :weight bold)
                                 ("DONE" :foreground "#2aa889"    :weight bold))
        org-capture-templates
        '(("t" "task" entry (file my-misc-org-file)
           "* TODO %?\n  %u\n" )
          ("m" "Meeting" entry (file my-misc-org-file)
           "* MEET %? \n  %t" :clock-in t :clock-resume t)
          ("n" "Note" entry (file my-misc-org-file)
           "* NOTE %?\n  %t" :clock-in t :clock-resume t)
          ("D" "Daily" entry (file my-misc-org-file)
           "* MEET Daily Scrum\n  %t" :clock-in t :clock-resume t)
          ("W" "Weekly" entry (file my-misc-org-file)
           "* MEET Weekly - Week %(format-time-string \"%W\")\n  %t"
           :clock-in t :clock-resume t)
          ("M" "Monthly" entry (file my-misc-org-file)
           "* MEET Monthly - %(format-time-string \"%B %Y\")\n  %t"
           :clock-in t :clock-resume t)
          ("g" "General" entry (file my-misc-org-file)
           "* %? %t\n" :clock-in t :clock-resume t)))
  :config
  (defun my-org-pdf-async ()
    "Perform an async pdf export."
    (interactive)
    (save-buffer)
    (message "Exporting PDF...")
    (org-latex-export-to-pdf t))

  :general
  ;; (leader-def-key
  ;;   :keymaps '(bibtex-mode-map)
  ;;   "i" 'org-ref-clean-bibtex-entry)

  (leader-def-key
    :keymaps '(org-mode-map)
    "E"     'my-org-pdf-async
    "s a"   'outline-show-all

    "o t c" 'org-table-create
    "p c"   'my-org-compile

    ;; Task management keybindings.
    "o t i" 'org-clock-in
    "o s"   'org-todo
    "o e"   'org-edit-special
    "o t s" 'org-clock-display
    "o n s" 'org-narrow-to-subtree
    "o n w" 'widen)
  (leader-def-key
    :keymaps '(org-src-mode-map)
    "o e"   'org-edit-src-exit)


  (general-define-key :keymaps '(dired-mode-map) "SPC" nil)

  (general-define-key
   :keymaps '(org-agenda-mode-map)
   "j"     'evil-next-line
   "k"     'evil-previous-line
   "g"     nil
   "SPC"   nil
   "g g"   'evil-goto-first-line
   "g r"   'org-agenda-redo-all
   "G"     'evil-goto-line)
  ;; Global org bindings
  (leader-def-key
    :keymaps 'override
    "o a"   'org-agenda
    "o c"   'org-capture
    "o o"   'my-pop-to-temp-org-buffer
    "o t r" 'org-clock-in-last
    "o t o" 'org-clock-out
    "o t t" 'org-clock-goto
    "o t i" 'org-clock-in))

(use-package org-agenda

  :straight (:type built-in)
  :commands (org-add-agenda-custom-command))

(use-package org-ref)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "◆" "✸" "◇")))

(use-package org-bars
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :hook  (org-mode . org-bars-mode))

(use-package fish-mode)

(use-package dash)
(use-package ox-jira)
(use-package f)
(use-package s)
(use-package request)
(use-package language-detection)
(use-package jiralib2)
(use-package ejira
  :straight (ejira :type git :host github :repo "nyyManni/ejira" :branch "ejira-completion")
  :commands (ejira-guess-epic-sprint-fields)
  :functions (ejira-guess-epic-sprint-fields my-jiralib2-login-remember-credentials
                                             my-add-ejira-kanban-board)
  :init


  (defun my-clock-fn ()
    (cond ((org-before-first-heading-p) "???")
          (t (let ((heading (replace-regexp-in-string
	                     org-bracket-link-analytic-regexp "\\5"
	                     (org-no-properties (org-get-heading t t t t)))))
               (cond ((s-starts-with-p "ejira-" (or (org-entry-get (point-marker) "TYPE") ""))
                      (concat
                       (propertize (org-entry-get (point-marker) "ID") 'face 'font-lock-type-face)
                       ": "
                       (if (> (length heading) 30)
                           (concat (substring heading 0 27) "...")
                         heading)))
                     (t heading))))))

  (eval-after-load "org-clock"
    (setq org-clock-heading-function #'my-clock-fn))

  (setq ejira-org-directory my-jira-directory)

  (setq request--curl-cookie-jar ""
        jiralib2-user-login-name my-ejira-username
        jiralib2-url             my-ejira-server
        jiralib2-auth            'token

        ejira-priorities-alist   '(("Blocker" . ?A)
                                   ("Highest" . ?B)
                                   ("High"    . ?C)
                                   ("Medium"  . ?D)
                                   ("Low"     . ?E)
                                   ("Lowest"  . ?F)
                                   ("Minor"   . ?G))
        ejira-todo-states-alist  '(("To Do"                    . 2)
                                   ("Backlog"                  . 1)
                                   ("Selected for Development" . 2)
                                   ("In Progress"              . 3)
                                   ("Quality Check"            . 4)
                                   ("Ready for QA"             . 4)
                                   ("Verify"                   . 4)
                                   ("Testing"                  . 4)
                                   ("Closed"                   . 5)
                                   ("Done"                     . 5))
        ejira-projects           my-ejira-projects)
  :config
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)
  (ejira-guess-epic-sprint-fields)

  :general
  (leader-def-key
    "j j"   'ejira-focus-item-under-point
    "o j l" 'ejira-insert-link-to-current-issue
    "o j j" 'ejira-focus-on-clocked-issue
    "o j m" 'ejira-mention-user
    "o j U" 'ejira-update-my-projects
    "o b"   'ejira-agenda-board

    "J"     'ejira-completion-focus-issue
    "K"     'ejira-completion-focus-issue-active-sprint
    "L"     'ejira-completion-focus-issue-assigned)

  (leader-def-key
    :keymaps '(org-mode-map)
    "o j t" 'ejira-set-issuetype
    "o j c c" 'ejira-add-comment
    "o j c d" 'ejira-delete-comment
    "o j a" 'ejira-assign-issue
    "o j P" 'ejira-push-item-under-point
    "o j u" 'ejira-pull-item-under-point
    "o j p" 'ejira-progress-issue)

  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states '(emacs motion normal)
   "C-j u"  'ejira-agenda-pull-item
   "C-j s"  'ejira-agenda-progress-item)

  (general-define-key
   :keymaps 'ejira-mode-map
   "C-S-x"  'ejira-close-buffer))

;; (use-package ejira-agenda
;;   :ensure nil
;;   :straight (ejira-agenda :type git :host github :repo "nyyManni/ejira")
;;   :config

;;   (defun my-add-ejira-kanban-board (key board-name &optional title)
;;     (setq title (or title board-name))
;;     (org-add-agenda-custom-command
;;      `(,key ,title
;;             ((ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and resolution = unresolved "
;;                                 "and assignee = currentUser()")
;;                         ((org-agenda-overriding-header
;;                           ,(concat title "\n\nAssigned to me"))))
;;              (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and resolution = unresolved "
;;                                 "and assignee is EMPTY")
;;                         ((org-agenda-overriding-header "Unassigned")))
;;              (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and resolution = unresolved "
;;                                 "and assignee != currentUser()")
;;                         ((org-agenda-overriding-header "Others")))))))

;;   (defun my-add-ejira-scrum-board (key board-name &optional title)
;;     (setq title (or title board-name))
;;     (org-add-agenda-custom-command
;;      `(,key ,title
;;             ((ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and sprint in openSprints() "
;;                                 "and assignee = currentUser()")
;;                         ((org-agenda-overriding-header
;;                           ,(concat title "\n\nAssigned to me"))))
;;              (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and sprint in openSprints() "
;;                                 "and assignee is EMPTY")
;;                         ((org-agenda-overriding-header "Unassigned")))
;;              (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
;;                                 "and sprint in openSprints() "
;;                                 "and assignee != currentUser()")
;;                         ((org-agenda-overriding-header "Others")))))))

;;   ;; my-ejira-kanban-boards is of form (("key" "name") ("key" "name") ...)
;;   (mapc (-partial #'apply #'my-add-ejira-kanban-board) my-ejira-kanban-boards)
;;   (mapc (-partial #'apply #'my-add-ejira-scrum-board) my-ejira-scrum-boards))


;; Tree-sitter

(add-to-list
 'treesit-language-source-alist
 '(python "https://github.com/tree-sitter/tree-sitter-python.git"))

(add-to-list
 'treesit-language-source-alist
 '(rust "https://github.com/tree-sitter/tree-sitter-rust.git"))

(add-to-list
 'treesit-language-source-alist
 '(c "https://github.com/tree-sitter/tree-sitter-c.git"))

(add-to-list
 'treesit-language-source-alist
 '(c++ "https://github.com/tree-sitter/tree-sitter-cpp.git"))

(add-to-list
 'treesit-language-source-alist
 '(javascript "https://github.com/tree-sitter/tree-sitter-javascript.git"))

(add-to-list
 'treesit-language-source-alist
 '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "typescript/src")))

(add-to-list 'major-mode-remap-alist
             '(python-mode . python-ts-mode))

;; (add-to-list 'major-mode-remap-alist
;;              '(rust-mode . rust-ts-mode))

(add-to-list 'major-mode-remap-alist
             '(c-mode . c-ts-mode))

(add-to-list 'major-mode-remap-alist
             '(c++-mode . c++-ts-mode))

(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))


(provide 'init)
;;; init.el ends here
