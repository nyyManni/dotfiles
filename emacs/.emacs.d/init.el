;; init.el --- -*- lexical-binding: t -*-
;; Copyright (c) 2016 - 2019 Henrik Nyman


;; Author     : Henrik Nyman <h@nyymanni.com>
;; Created    : 10 Aug 2016
;; Version    : 0.3

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

;; nyyManni's configuration for Emacs, 2021 flavor.

;;; Code:

(setq user-full-name       "Henrik Nyman"
      user-login-name      "hnyman"
      user-mail-address    "h@nyymanni.com"
      user-emacs-directory "~/.emacs.d"

      vc-follow-symlinks                t
      inhibit-startup-screen            t
      initial-scratch-message           ""

      inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      sentence-end-double-space nil

      backup-directory-alist         '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t))

      custom-file          (concat user-emacs-directory "/customize-ignored.el")
      delete-old-versions  -1
      version-control      t
      vc-make-backup-files t
      tab-width            2
      frame-title-format   '("" "Emacs v" emacs-version))

(when (eq system-type 'gnu/linux)
  (defvar ns-command-modifier nil)
  (defvar ns-option-modifier nil)
  (defvar ns-allow-anti-aliasing nil)
  (defvar ns-use-srgb-colorspace nil)
  (setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket")))
(when (eq system-type 'darwin)
  (setq exec-path                     (append exec-path '("/usr/local/bin"))
        default-input-method          "MacOSX"
        default-directory             "/Users/hnyman/"
        ns-command-modifier           'meta
        ns-option-modifier            nil
        ns-allow-anti-aliasing        t
        ns-use-srgb-colorspace        nil
        frame-resize-pixelwise        t
        mouse-wheel-scroll-amount     '(5 ((shift) . 5) ((control)))
        mouse-wheel-progressive-speed nil)

  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "launchctl getenv SSH_AUTH_SOCK | tr -d '\n'")))

(let ((work-config (concat user-emacs-directory "/work-config.el")))
  (when (file-exists-p work-config)
    (load-file (concat user-emacs-directory "/work-config.el"))))

(defvar user-hostname (shell-command-to-string "hostname |sed 's/\\.local//' |tr -d '\n'"))
(fset 'startup-echo-area-message (lambda () ""))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)


(setq package-user-dir (expand-file-name "sitelisp" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

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
  (load-theme 'gotham t))

(use-package doom-modeline
  :demand
  :init
  (setq doom-modeline-height 24)
  :config
  (doom-modeline-mode))

(use-package hlinum
  :hook (after-init . hlinum-activate)
  :config
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'linum
                      :foreground "#CAE682"
                      :background "#444444"
                      :weight 'bold))

;; Evil
(defconst my/leader "SPC")
(use-package general
  :config
  (general-define-key
    :states 'motion
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

  ;; Disable arrow key movement
  (dolist (key '("<left>" "<right>" "<up>" "<down>"))
    (general-define-key :keymaps '(evil-motion-state-map) key nil)
    (global-unset-key (kbd key)))
  ;; (global-set-key (kbd "M-?") 'help-command)
  (global-set-key (kbd "M-?") nil)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)

  ; Disable toggling fullscreen with f11
  (general-define-key "<f11>" nil)

  (global-set-key (kbd "C-S-u") 'universal-argument)
  (define-key universal-argument-map (kbd "C-S-u") 'universal-argument-more)

  (leader-def-key
    :keymaps 'override
    "a a" 'align-regexp
    "s w" 'whitespace-mode
    "e"   'eval-last-sexp
    "D"   'kill-this-buffer
    "F"   'my-dired-here
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

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-fine-undo t)
  (setq evil-want-keybinding nil)

  :custom ((evil-undo-system 'undo-redo))
  :config
  (general-define-key
    :keymaps '(evil-normal-state-map)
    "M-." nil)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package key-chord
  :hook (after-init . (lambda () (key-chord-mode t)))
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

;; Ivy, counsel and swiper

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :general
  (leader-def-key
   ";" 'counsel-find-file
   "x" 'counsel-M-x
   "A" 'counsel-apropos
   "y"  'counse-yank-pop)
  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1)
  :general
  (leader-def-key
   "'" 'counsel-projectile-switch-project
   ":" 'counsel-projectile-find-file))


(use-package ivy
  :diminish
  :demand t

  :general
  (leader-def-key
   "b" 'ivy-switch-buffer)
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("SPC"   . ivy-alt-done-or-space)
              ("C-d"   . ivy-done-or-delete-char)
              ("C-i"   . ivy-partial-or-done)
              ("C-r"   . ivy-previous-line-or-history)
              ("M-r"   . ivy-reverse-i-search))


  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)

  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-immediate-done
       #'ivy-delete-char)))

  (defun ivy-alt-done-or-space ()
    (interactive)
    (call-interactively
     (if (= ivy--length 1)
         #'ivy-alt-done
       #'self-insert-command)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my-ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)

  (defun ivy--switch-buffer-matcher (regexp candidates)
    "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
    (let ((res (ivy--re-filter regexp candidates)))
      (if (or (null ivy-use-ignore)
              (null ivy-ignore-buffers))
          res
        (or (cl-remove-if
             (lambda (buf)
               (cl-find-if
                (lambda (f-or-r)
                  (if (functionp f-or-r)
                      (funcall f-or-r buf)
                    (string-match-p f-or-r buf)))
                ivy-ignore-buffers))
             res)
            (and (eq ivy-use-ignore t)
                 res))))))


(use-package swiper
  :after ivy
  :bind ("C-M-s" . swiper)
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("C-." . swiper-avy)
              ("M-c" . swiper-mc))
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch)))


(use-package projectile
  :hook (after-init . projectile-mode)
  :diminish
  :functions (my-projectile-invalidate-cache)
  :bind* (("C-c TAB" . projectile-find-other-file)
          ("C-c P" . (lambda () (interactive)
                       (projectile-cleanup-known-projects)
                       (projectile-discover-projects-in-search-path))))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config

  (defun my-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'my-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'my-projectile-invalidate-cache))))



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
  :hook (prog-mode . smartparens-mode)
  :init
  (setq-default sp-escape-quotes-after-insert nil)
  :general
  (general-define-key
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map
               python-mode-map
               c-mode-map c++-mode-map)
    "C->" 'sp-forward-slurp-sexp)
  :config
  (require 'smartparens-config))


(use-package company
  :hook
  (prog-mode . company-mode)
  (eshell-mode . company-mode)
  :functions (my-complete-or-indent my-company-hook)
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (defun my-complete-or-indent ()
    "On an empty (only whitespace) line, do an indent, otherwise auto-complete."
    (interactive)
    (if (string-match "^[[:blank:]]*$"
                      (buffer-substring (line-beginning-position)
                                        (point)))
        (indent-for-tab-command)
      (company-complete)))

  ;; TODO: Figure out why this needs a hook
  (defun my-company-hook ()
    (general-define-key
      :keymaps '(company-active-map)
      "<return>" 'company-complete-selection))

  (add-hook 'company-mode-hook #'my-company-hook)
  :general

  (general-define-key
    :states '(insert)
    "<tab>" 'my-complete-or-indent))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

(use-package magit
  :defines (magit-branch-arguments magit-git-executable)
  :init
  (setq magit-branch-arguments nil)
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/local/bin/git"))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :general
  (leader-def-key
    :keymaps 'override
    "g g"   'magit-status
    "g b"   'magit-blame
    "g f h" 'magit-log-buffer-file))


(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (nxml-mode . flycheck-mode)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  ;; Disable other flycheck backends, only use lsp
  (setq-default flycheck-disabled-checkers '(c/c++-gcc c/c++-clang rust-cargo)))

;; LSP

(use-package lsp-mode
  :hook
  (c-mode      . lsp-deferred)
  (c++-mode    . lsp-deferred)
  (objc-mode   . lsp-deferred)
  (python-mode . lsp-deferred)
  (rust-mode   . lsp-deferred)
  (js2-mode    . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom ((lsp-diagnostics-provider :flycheck)
           (lsp-file-watch-threshold             30000)
           (lsp-idle-delay                       0.500)
           (lsp-enable-links                     nil)
           (lsp-pyls-plugins-pylint-enabled      t)
           (lsp-pyls-plugins-pycodestyle-enabled nil))

  :init
  (setq read-process-output-max              (* 1024 1024)
        company-minimum-prefix-length        1
        company-idle-delay                   0.0))

(use-package lsp-ui :commands lsp-ui-mode
  :commands (lsp-ui-doc-mode)
  :init
  (add-hook 'lsp-ui-mode-hook (lambda () (interactive) (lsp-ui-doc-mode 0))))

;; C/C++
(use-package ccls
  :init
  (put 'c-macro-cppflags 'safe-local-variable (lambda (_) t)))
(use-package clang-format)
(use-package clang-format+
  :hook (c-mode-common . clang-format+-mode))
(use-package glsl-mode)
(use-package cmake-mode)


;; JS
(use-package rjsx-mode
  :mode "\\.js\\'"
  :init
  (setq-default js-indent-level 2)
  (general-define-key
    :keymaps '(rjsx-mode-map)
    "M-." #'xref-find-definitions))

(use-package prettier-js
  :hook ((js2-mode rsjx-mode) . prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all")))


;; Python
(use-package pyvenv
  :hook (python-mode . pyvenv-tracking-mode)
  :functions (pyvenv-virtualenvwrapper-supported)
  :commands (pyvenv-virtualenvwrapper-supported)
  :config

  ;; Don't waste time with virtualenvwrapper. It is slow and we don't use it
  (advice-add #'pyvenv-virtualenvwrapper-supported :override (lambda (&rest _))))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save)
  :defines (py-autopep8-options)
  :init
  ;; Set the line-length for autopep to something large so that it
  ;; does not touch too long lines, it usually cannot fix them properly
  (setq py-autopep8-options '("--max-line-length=200" "--ignore=E402" "--ignore=E731"))
  :general
  (space-leader
    :keymaps '(python-mode-map)
    "p f" 'py-autopep8-buffer))

;; DAP

(use-package dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (_) (call-interactively #'dap-hydra))))


;; EShell
(use-package eshell
  :ensure nil
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

  :general
  (general-define-key
    :keymaps 'eshell-mode-map
    "C-S-q" 'my-quit-eshell)
  (leader-def-key
    :keymaps 'override
    "s e" 'my-eshell-here))

(use-package systemd)
(use-package jinja2-mode)
(use-package apt-sources-list)

(use-package package-lint)
(use-package flycheck-package)


(use-package http)
(use-package qml-mode)

(use-package wdired)

(use-package systemd)
(use-package meson-mode)
(use-package yaml-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))



(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(provide 'init)
;;; init.el ends here
