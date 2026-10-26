;; init.el --- -*- lexical-binding: t -*-
;; Copyright (c) 2016 - 2025 Henrik Nyman


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

      vc-follow-symlinks                t
      inhibit-startup-screen            t
      initial-scratch-message           ""
      load-prefer-newer                 t

      inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      sentence-end-double-space nil

      read-extended-command-predicate #'command-completion-default-include-p

      backup-directory-alist         '(("." . "~/.local/share/emacs/backups/"))

      custom-file          (concat user-emacs-directory "/customize-ignored.el")
      delete-old-versions  -1
      version-control      t
      display-line-numbers-type 'relative
      vc-make-backup-files t
      tab-width            2
      tab-always-indent    'complete

      ring-bell-function   'ignore

      frame-title-format   '("" "Emacs v" emacs-version))


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

(put 'narrow-to-region 'disabled nil)


(setq straight-repository-branch "develop"
      straight-profiles
      `((nil . ,(expand-file-name "versions/default.el" nyy/emacs-config-directory))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package '(use-package :type built-in))
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

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

(setopt use-short-answers t)

(global-unset-key (kbd "C-/"))  ;; evil sets undo to 'u'
(global-unset-key (kbd "C-?"))  ;; evil sets redo to 'C-r'
(when window-system
  (global-unset-key (kbd "C-z"))
  (blink-cursor-mode 0)
  (setq confirm-kill-emacs 'y-or-n-p))
(when (daemonp)
  (blink-cursor-mode 0))


(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(set-face-attribute 'fill-column-indicator nil :height 1.1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'latex-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)

;; (when (eq system-type 'darwin)
(add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)

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

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))


(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package catppuccin-theme
  :demand
  :if (or (daemonp) window-system)

  :config
  (load-theme 'catppuccin t)

  (add-hook 'server-after-make-frame-hook #'catppuccin-reload)

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

(use-package restart-emacs)

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
  "Put the current file name on the clipboard."
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

  (defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
    "Show lines matching REGEXP in all file-visiting buffers.

Given a prefix argument, search in ALL buffers."
    (interactive (occur-read-primary-args))
    (multi-occur-in-matching-buffers "." regexp allbufs))

  ;; (global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)


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
  (defun my-dired-home ()
    (interactive)
    (dired (expand-file-name "~")))

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
    "D"   'kill-current-buffer
    "Y"   'my-put-file-name-on-clipboard
    "f f" 'my-dired-here
    "f F" 'my-dired-home
    "f i" 'consult-imenu
    "O"   'consult-line
    "l p" 'package-list-packages
    "G O" 'my-multi-occur-in-matching-buffers
    "s l" 'sort-lines
    "r"   'my-reload-file
    "i"   'indent-region
    "I"   (lambda () (interactive) (find-file user-init-file))
    "S"   'delete-trailing-whitespace
    "P s" 'profiler-start
    "P e" 'profiler-stop
    "P r" 'profiler-report))

(use-package sudo-edit)


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

  ;; Allow insert-mode movement keys in the ex-buffer
  (general-define-key
   :keymaps '(evil-ex-completion-map)
   "C-f" nil
   "C-M-f" 'evil-command-window
   "C-b" nil)

  (evil-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.local/share/emacs/undo-tree")))
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
  :general
  (dolist (chord '("jk" "JK" "jK" "Jk"))

    (key-chord-define evil-insert-state-map chord 'evil-normal-state)
    ))


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
   :keymaps '(evil-normal-state-map evil-motion-state-map)
   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)
  (general-define-key
   :keymaps '(evil-normal-state-map)
   "K" 'evil-jump-out-args))

(use-package evil-textobj-tree-sitter
  :after evil
  :straight (evil-textobj-tree-sitter :type git
                      :host github
                      :repo "meain/evil-textobj-tree-sitter"
                      :files (:defaults "queries" "treesit-queries"))

  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner")))

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
  :straight (:type built-in)
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
    "B"   'consult-project-buffer
    "f l" 'consult-focus-lines
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

(use-package wgrep)

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

(use-package winpulse

  :straight (winpulse :type git :host github :repo "xenodium/winpulse")
  :config
  (winpulse-mode +1))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  (toml-ts-mode . smartparens-mode)
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
  (require 'smartparens-config))



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

  :bind (
         ("C-<tab>" . completion-at-point)
         :map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))

  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))


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

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists 'skip) ; prevent extra bars in nested lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode) . indent-bars-mode))

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
  :defines (magit-git-executable)
  :init
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


;; (use-package borg)

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
  (rust-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (lua-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (csharp-ts-mode . eglot-ensure)
  (glsl-ts-mode . eglot-ensure)
  (qml-mode . eglot-ensure)
  (sql-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)

  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

  ;; Disable the progress notification, as it is interfering with other minibuffer
  ;; messages.
  (setq eglot-report-progress nil)

  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 ;; Use the user-installed version of ty and ruff instead of whatever
                 ;; the project defines
                 "rass" "--" ,(expand-file-name "~/.local/bin/ty") "server" "--"
                 ,(expand-file-name "~/.local/bin/ruff") "server"))

  (add-to-list 'eglot-server-programs
               '((glsl-ts-mode)
                 "/usr/bin/glsl_analyzer"))
  (add-to-list 'eglot-server-programs
               '((sql-mode)
                 "sql-language-server" "up" "--method" "stdio"))

  (add-to-list 'eglot-server-programs
               '((qml-mode)
                 "qmlls"))


  (add-to-list 'eglot-server-programs
               '((csharp-mode csharp-ts-mode) . ("csharp-ls")))

  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode) .
                 ("rustup" "run" "stable" "rust-analyzer"
                  :initializationOptions (:check (:command "clippy")))))


  :general
  (leader-def-key
    :keymaps 'eglot-mode-map
    "F F" 'eglot-format))


(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t))))


(use-package project :straight t)

(use-package markdown-mode
  :config
  (add-to-list 'markdown-code-lang-modes '("python" . python-mode))
  (add-to-list 'markdown-code-lang-modes '("rust" . rust-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("typescript" . typescript-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("javascript" . js-ts-mode))
  :custom
  ((markdown-header-scaling t)
   (markdown-fontify-code-blocks-natively t)))
(use-package uml-mode)

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(use-package py-isort
  :custom ((py-isort-options '("--sl")))
  :general
  (leader-def-key
    :keymaps '(python-mode-map python-ts-mode-map)
    "s i"   'py-isort-buffer))


(use-package reformatter
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  (typescript-ts-mode . prettier-on-save-mode)
  (json-ts-mode . prettier-on-save-mode)
  (tsx-ts-mode . prettier-on-save-mode)
  (c-mode-common . clang-format-on-save-mode)
  (c-ts-mode . clang-format-on-save-mode)
  (c++-ts-mode . clang-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-"))

  (reformatter-define prettier
    :program "prettier"
    :args `("--tab-width" "4" "--stdin-filepath" ,buffer-file-name))

  ;; Format whole C-family buffers on save, not only changed definitions.
  ;; With no project style file, leave the buffer unchanged.
  (reformatter-define clang-format
    :program "clang-format"
    :args (list "--style=file" "--fallback-style=none"
                "--assume-filename"
                (or buffer-file-name
                    (expand-file-name "stdin.cpp" default-directory)))))

(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))

;; C/C++
(setq-default c-basic-offset 4)
(use-package glsl-mode
  :straight (glsl-mode :type git :host github :repo "TideSofDarK/glsl-mode" :branch "better-ts-mode")
  )
(use-package cmake-ts-mode
  :straight (:type built-in)
  :mode ("\\(?:\\`\\|/\\)CMakeLists\\.txt\\'" "\\.cmake\\'"))


;; C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))




;; RUST

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode "\\.rs\\'")

(use-package typescript-ts-mode
  :ensure nil
  :mode
  ("\\.ts\\'"))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


;; C#

(use-package lua-ts-mode
  :straight (:type built-in)
  :mode "\\.lua\\'"
  :interpreter "lua")

;; EShell
(use-package eshell
  :straight (:type built-in)
  :defines (eshell-banner-message eshell-cmpl-cycle-completions
                                  eshell-modify-global-environment eshell-prompt-regexp
                                  eshell-prompt-function)
  :functions (eshell-life-is-too-much eshell/whoami eshell/pwd esh-prompt-func)
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

    ;; (concat
    ;;  (eshell/whoami) "@" user-hostname " " (abbreviate-file-name (eshell/pwd)) ""
    (concat
     "┌─" (eshell/whoami) "@" user-hostname " " (abbreviate-file-name (eshell/pwd)) "\n"
     "└─$ "))
  (setq eshell-prompt-regexp "└─\\$ ")   ; or "└─> "
  (setq eshell-prompt-function #'esh-prompt-func)


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


(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))


;; (use-package lsp-java)
;; (use-package eglot-java
;;   :init
;;   (setq eglot-java-server-install-dir (expand-file-name "~/.local/share/emacs/eclipse.jdt.ls")))

(use-package web-mode
  :mode ("\\.jsp\\'"))

(use-package go-ts-mode
  :straight (:type built-in)
  :mode (("\\.go\\'" . go-ts-mode)
         ("\\(?:\\`\\|/\\)go\\.mod\\'" . go-mod-ts-mode)))

(use-package jinja2-mode)
(use-package apt-sources-list)

;; (use-package package-lint)
;; (use-package flycheck-package)

(use-package verb)
(use-package qml-mode
  :mode ("\\.qmlproject\\'" "\\.qml\\'"))

(use-package json-ts-mode
  :mode ("\\.json\\'")
  ;; Set default indent to 4 spaces
  :init
  (setq-default json-ts-mode-indent-offset 4))

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))

(use-package wdired)

(use-package dired+
  :init
  (setq-default diredp-hide-details-initially-flag nil)
  :config
  (set-face-attribute 'diredp-file-name nil :foreground "white"))

(use-package systemd)
(use-package meson-mode)
(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode "\\.ya?ml\\'")
(use-package toml-ts-mode
  :straight (:type built-in)
  :mode ("\\.toml\\'" "\\(?:\\`\\|/\\)uv\\.lock\\'"))
(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode ("\\(?:\\`\\|/\\)\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/]*\\)?\\'"
         "\\.[Dd]ockerfile\\'"))
(use-package csv-mode)
(use-package yuck-mode)

(use-package powershell)

(use-package groovy-mode)
;; (use-package editorconfig
;;   :config
;;   (editorconfig-mode 1))

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

  (setq org-confirm-babel-evaluate             nil
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
          ("X" "Web Capture" entry (file my-misc-org-file)
           "* NOTE %?\n ")


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

  :general

  (leader-def-key
    :keymaps '(org-mode-map)
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


(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-roam)


(use-package org-protocol
  :straight (:type built-in))

(use-package fish-mode)

(use-package dash)
(use-package ox-jira)
(use-package f)
(use-package s)
(use-package request)

(use-package protobuf-mode)

;; Tree-sitter
(use-package treesit-auto
  :demand t
  :config

 ;;  (setq my-js-tsauto-config
 ;;        (make-treesit-auto-recipe
 ;;      :lang 'typescript
 ;;      :ts-mode 'typescript-ts-mode
 ;;      :remap 'typescript-mode
 ;;      :requires 'tsx
 ;;      :url "https://github.com/tree-sitter/tree-sitter-typescript"
 ;;      :revision "master"
 ;;      :source-dir "typescript/src"
 ;;      :ext "\\.tsx\\'"))

 ;; (add-to-list 'treesit-auto-recipe-list my-js-tsauto-config)

  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; typst
(use-package typst-ts-mode)

(with-eval-after-load 'eglot
  (with-eval-after-load 'typst-ts-mode
    (add-to-list 'eglot-server-programs
                 '((typst-ts-mode) . ("tinymist")))))

(use-package xkb-mode
  :straight
  (xkb-mode :type git :host github :repo "captainflasmr/xkb-mode"))

(use-package dart-mode)

;; AI Tool Configuration

(use-package copilot
  ;; Configure copilot configuration to play nicely with auto-complete
  ;; bind accepting completion to Ctrl+shift+tab
  :hook
  (prog-mode . copilot-mode)
  :general
  (general-define-key

   :keymaps '(copilot-mode-map)
   "C-S-<iso-lefttab>" 'copilot-accept-completion)
  (general-define-key
   :keymaps '(copilot-completion-map)
   "TAB" nil
   "<tab>" nil
   )
  )


(use-package gptel
  :config
  (add-to-list 'gptel-prompt-prefix-alist
               `(markdown-mode . ,  (concat (propertize  "[>]" 'font-lock-face `(:foreground ,(face-attribute 'success :foreground) :weight bold))
                                            (propertize  ": " 'font-lock-face `(:foreground ,(face-attribute 'default :foreground))))))

  (defun my-gptel-shell-here ()
    (interactive)
    (gptel "*gptel-shell*")
    (switch-to-buffer-other-window "*gptel-shell*")
    (evil-insert-state))

  (defun my-quit-gptel-shell ()
    (interactive)
    (gptel-abort (current-buffer))
    (delete-window))


  :general
  (leader-def-key
    :keymaps 'override
    "s g" 'my-gptel-shell-here)

  (general-define-key
   :keymaps 'gptel-mode-map
   "C-S-x" 'my-quit-gptel-shell))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install))


(defun my-get-password-from-authinfo (host)
  "Receive a password for HOST in .authinfo."
  (let* ((found (car (auth-source-search :host host :require '(:secret)))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(use-package mcp
  :after gptel
  :custom (mcp-hub-servers
           `(("brave-search" . (:command "npx"
                                :args ("-y" "@brave/brave-search-mcp-server"
                                       "--transport" "stdio"
                                       "--brave-api-key-file"
                                       ,(expand-file-name "~/.local/share/emacs/brave-api-key"))))
             ("githits" . (:command "npx"
                           :args ("-y" "githits@latest" "mcp" "start")))))
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations)

  (setq jsonrpc-default-request-timeout 300)

  :hook (after-init . mcp-hub-start-all-server))

(use-package eca
  :general
  (leader-def-key
    "E" 'eca-transient-menu)

  :custom
  ;; Disable annoying emojies
  (eca-chat-prompt-prefix-loading (propertize "... " 'font-lock-face `(:foreground ,(face-attribute 'info-node :foreground) :weight bold)))
  (eca-chat-mcp-tool-call-loading-symbol (propertize "... " 'font-lock-face `(:foreground ,(face-attribute 'info-node :foreground) :weight bold)))
  (eca-chat-mcp-tool-call-error-symbol (propertize "FAIL" 'font-lock-face `(:foreground ,(face-attribute 'error :foreground) :weight bold)))
  (eca-chat-mcp-tool-call-success-symbol (propertize "OK" 'font-lock-face `(:foreground ,(face-attribute 'success :foreground) :weight bold)))
  (eca-chat-mcp-tool-call-pending-approval-symbol (propertize "PENDING" 'font-lock-face `(:foreground ,(face-attribute 'warning :foreground) :weight bold))))




(use-package agent-shell
  :custom
  (agent-shell-session-restore-verbosity 'full)
  :config
  (require 'nyy-agent-shell-config)
  (nyy/agent-shell-configure))


(use-package agent-shell-manager
  :straight (:type git :host github :repo "jethrokuan/agent-shell-manager"
             :files ("*.el"))
  :config
  (require 'consult-agent-shell-sessions))



(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
             :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :config
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))



(provide 'init)
;;; init.el ends here
