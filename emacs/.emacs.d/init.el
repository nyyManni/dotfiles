;; init.el --- -*- lexical-binding: t -*-
;; Copyright (c) 2016 - 2022 Henrik Nyman


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
      load-prefer-newer                 t

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'latex-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(when (eq system-type 'darwin)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode))


(setq package-user-dir (expand-file-name "sitelisp" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ))

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
  (load-theme 'gotham t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :demand
  :init
  (setq doom-modeline-height 18)
  :config

  (defun my-get-python-version ()
    '("python" "--version"))
  (setq doom-modeline-env-python-command #'my-get-python-version)
  )

(use-package hlinum
  :hook (after-init . hlinum-activate)
  :config
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'line-number
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

  ; Disable toggling fullscreen with f11
  (general-define-key "<f11>" nil)

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
    "a a" 'align-regexp
    "s w" 'whitespace-mode
    "e"   'eval-last-sexp
    "D"   'kill-this-buffer
    "F F"   'my-dired-here
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
   ;;"A" 'counsel-apropos
   "A" 'helm-apropos  ;; helm-apropos is way superior
   "y"  'counse-yank-pop)
  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         )
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))


(use-package helm
  :hook (after-init . helm-mode)
  :init
  (setq helm-candidate-number-limit           100
        helm-idle-delay                       0.0
        helm-input-idle-delay                 0.01
        helm-quick-update                     t
        helm-M-x-requires-pattern             nil
        helm-ff-skip-boring-files             t
        helm-move-to-line-cycle-in-source     t
        helm-split-window-inside-p            t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)
  :config
  (require 'helm-config)
  (helm-autoresize-mode t)
  :general
  (general-define-key
    :keymaps '(helm-map)
    "C-i" 'helm-execute-persistent-action
    "C-k" 'helm-previous-line
    "C-j" 'helm-next-line))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1)
  :general
  (leader-def-key
    "G P" 'counsel-projectile-rg
    "'" 'counsel-projectile-switch-project
    "f" 'counsel-imenu
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

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package amx
  :defer nil
  :config
  (amx-mode t))

;; Projectile

(use-package projectile
  :hook (after-init . projectile-mode)
  :diminish
  :functions (my-projectile-invalidate-cache)
  :init
  (setq projectile-per-project-compilation-buffer t)
  :bind* (("C-c TAB" . projectile-find-other-file)
          ("C-c P" . (lambda () (interactive)
                       (projectile-cleanup-known-projects)
                       (projectile-discover-projects-in-search-path))))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; Allow the compilation to use terminal control characters.
  (ignore-errors
    (require 'ansi-color)
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-install-cmd 'safe-local-variable #'stringp)
  (put 'dap-python-executable 'safe-local-variable #'stringp)

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

(use-package all-the-icons)
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


(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (nxml-mode . flycheck-mode)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  ;; Disable other flycheck backends, only use lsp
  (setq-default flycheck-disabled-checkers '(c/c++-gcc c/c++-clang rust-cargo)))

;; LSP

;; Create a second mode-specific hook to run after .dir-locals have been parsed
(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)
(defun run-local-vars-mode-hook ()
  "Run a hook for the `major-mode' after the local variables have been processed."

  ;; First, reset all the path hacks
  (setenv "PATH" (s-join ":" (-remove
                              (lambda (p) (and p (s-starts-with-p (getenv "WORKON_HOME") p)))
                              (split-string
                               (getenv "PATH") ":"))))

  (setq exec-path (-remove (lambda (p) (and p (s-starts-with-p (getenv "WORKON_HOME") p))) exec-path))

  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)


(use-package lsp-mode
  :config
  (setq lsp-clients-python-command "pylsp"
        ;; lsp-pylsp-server-command '("pylsp", "-v")
        lsp-pylsp-server-command '("pylsp")
        lsp-rust-server            'rust-analyzer
        lsp-log-io                 nil
        lsp-completion-no-cache    nil)


  (if (eq system-type 'darwin)
      (setq lsp-rust-rls-server-command "/usr/local/bin/rust-analyzer")
    (setq lsp-rust-rls-server-command "/home/hnyman/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))

  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-links nil
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        )


  (defun my-python-mode-hook ()
    (when pyvenv-activate (pyvenv-activate pyvenv-activate))

    (let ((venv-paths (when pyvenv-activate (pyvenv--virtual-env-bin-dirs pyvenv-activate))))
      (setq python-shell-exec-path venv-paths)
      (setenv "PATH" (s-join ":" (append venv-paths (split-string (getenv "PATH") ":"))))

      (setq exec-path (append venv-paths exec-path))

      (doom-modeline-env-update-python)
      (lsp)))


  ;; Defer running lsp for python until we have parsed .dir-locals to allow
  ;; setting project-specific virtual environments
  (add-hook 'python-mode-local-vars-hook 'my-python-mode-hook)

  :custom
  ((lsp-pylsp-plugins-pylint-enabled t)
   (lsp-pylsp-plugins-pydocstyle-ignore "D401")
   (lsp-pylsp-plugins-flake8-enabled nil)
   ;; (lsp-pylsp-plugins-)
   (lsp-clients-pylsp-library-directories
    '("/usr/"
      (expand-file-name "/.virtualenvs/")
      (expand-file-name "/.pyenv/versions/")
      )
    )
   ;; lsp-pylsp-get-pyenv-environment
   )
  :hook
  ((c-mode-common . lsp)
   (rjsx-mode . lsp)
   (lua-mode . lsp)
   (typescript-mode . lsp)
   (latex-mode . lsp)
   (rust-mode . lsp)
   (csharp-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map evil-normal-state-map
              ("gh" . lsp-describe-thing-at-point)
              ;; :map md/leader-map
              ;; ("Ff" . lsp-format-buffer)
              ;; ("FR" . lsp-rename)
              )
  :general
  (leader-def-key
    "Ff" 'lsp-format-buffer
    "FR" 'lsp-rename))

(use-package lsp-ui
  :config (setq
           lsp-ui-sideline-show-hover t
           lsp-ui-sideline-delay 0.5
           lsp-ui-doc-delay 5
           lsp-ui-doc-enable t
           lsp-ui-sideline-enable t
           lsp-ui-sideline-ignore-duplicates t
           lsp-ui-doc-position 'bottom
           lsp-ui-doc-alignment 'frame
           ;; lsp-ui-doc-header nil
           lsp-eldoc-enable-hover t
           ;; lsp-ui-doc-include-signature nil
           lsp-lens-enable nil
           lsp-pylsp-plugins-jedi-signature-help-enabled t
           lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
              ("gd" . lsp-ui-peek-find-definitions)
              ("gr" . lsp-ui-peek-find-references)
              ))

(use-package pyvenv
  :demand t
  :config

  ;; Don't waste time with virtualenvwrapper. It is slow and we don't use it
  (setq pyvenv-workon "emacs")  ; Default venv

  (put 'python-shell-virtualenv-root 'safe-local-variable (lambda (_) t))
  (advice-add #'pyvenv-virtualenvwrapper-supported :override (lambda (&rest _)))
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; (use-package lsp-ui :commands lsp-ui-mode
;;   :commands (lsp-ui-doc-mode)
;;   :init
;;   (add-hook 'lsp-ui-mode-hook (lambda () (interactive) (lsp-ui-doc-mode 0))))

;; C/C++
(setq-default c-basic-offset 4)
(use-package ccls
  :init
  (put 'c-macro-cppflags 'safe-local-variable (lambda (_) t)))
(use-package clang-format)
(use-package clang-format+
  :hook (c-mode-common . clang-format+-mode))
(use-package glsl-mode)
(use-package cmake-mode)


;; RUST

(use-package rust-mode
  :bind (:map rust-mode-map
         ("C-c C-c C-c" . projectile-compile-project)
  ))

;; JS
(use-package rjsx-mode
  :mode ("\\.js\\'" "\\.tsx")
  :init
  (setq-default js-indent-level 4)
  (general-define-key
    :keymaps '(rjsx-mode-map)
    "M-." #'xref-find-definitions)
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package prettier-js
  :hook ((js2-mode rsjx-mode) . prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all")))

(use-package typescript-mode)
;; C#

(use-package lua-mode)

(use-package csharp-mode)

;; DAP

(use-package dap-mode
  :custom ((dap-python-debugger 'debugpy))
  :config
  (add-hook 'dap-stopped-hook
            (lambda (_) (call-interactively #'dap-hydra)))
  (dap-ui-controls-mode -1)
  ;; (setq dap-stopped-hook
  ;;       ;; '(dap-ui--show-many-windows)
  ;;       nil
  ;;       )

  (require 'dap-python)

  (when (eq system-type 'darwin)
    (advice-add #'dap-python--pyenv-executable-find :override #'executable-find)))

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

(use-package systemd)
(use-package jinja2-mode)
(use-package apt-sources-list)

(use-package package-lint)
(use-package flycheck-package)

(use-package http)
(use-package qml-mode)
(use-package json-mode)

(use-package wdired)

(use-package systemd)
(use-package meson-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package dockerfile-mode)

(use-package powershell)

(use-package groovy-mode)
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package lunchtime
  :ensure nil
  :load-path "~/.emacs.d/lisp/lunchtime"
  :commands (lunchtime-display-menus)
  :config

  (setq lunchtime-parsers-alist nil)
  ;; TTY
  (lunchtime-define-restaurant
   "https://api.ruoka.xyz/%Y-%m-%d"
   (mapcar
    (lambda (restaurant)
      `((name . ,(assoc-recursive restaurant 'name))
        (subrestaurants
         .
         ,(mapcar
           (lambda (subrestaurant)
             `((name . ,(assoc-recursive subrestaurant 'name))
               (menus . ,(mapcar
                          (lambda (meal)
                            `((name . ,(assoc-recursive meal 'name))
                              (prices . ,(assoc-recursive meal 'prices))
                              (menu . ,(mapcar
                                        (lambda (part)
                                          (assoc-recursive part 'name))
                                        (assoc-recursive meal 'contents)))))
                          (assoc-recursive subrestaurant 'meals)))))
           (assoc-recursive restaurant 'menus)))))

    (assoc-recursive lunchtime-response-data 'restaurants)))

  ;; Hermia 6
  (lunchtime-define-restaurant
   "https://www.sodexo.fi/ruokalistat/output/daily_json/110/%Y-%m-%d"
   `(((name . ,(assoc-recursive lunchtime-response-data 'meta 'ref_title))
      (subrestaurants
       .
       (((name . "Lounas") ;; Sodexo has only one restaurant per menu item
         (menus . ,(mapcar
                    (lambda (item)
                      `((name . ,(assoc-recursive item 'category))
                        (prices . (,(assoc-recursive item 'price)))
                        (menu . (, (concat
                                    (assoc-recursive item 'title_fi)
                                    " (" (assoc-recursive item 'properties) ")")))))
                    (assoc-recursive lunchtime-response-data 'courses)))))))))

  ;; Hermia 5
  (lunchtime-define-restaurant
   "https://www.sodexo.fi/ruokalistat/output/daily_json/107/%Y-%m-%d"
   `(((name . ,(assoc-recursive lunchtime-response-data 'meta 'ref_title))
      (subrestaurants
       .
       (((name . "Lounas") ;; Sodexo has only one restaurant per menu item
         (menus . ,(mapcar
                    (lambda (item)
                      `((name . ,(assoc-recursive item 'category))
                        (prices . (,(assoc-recursive item 'price)))
                        (menu . (, (concat
                                    (assoc-recursive item 'title_fi)
                                    " (" (assoc-recursive item 'properties) ")")))))
                    (assoc-recursive lunchtime-response-data 'courses)))))))))

  :general
  (leader-def-key
    "l l" 'lunchtime-display-menus)
  (general-define-key
    :keymaps '(lunchtime-mode-map)
    :states '(normal)
    "o" 'delete-other-windows
    "l" 'lunchtime-next-day
    "h" 'lunchtime-previous-day
    "j" 'lunchtime-next-restaurant
    "k" 'lunchtime-previous-restaurant
    "q" 'lunchtime-close))


(use-package org
  :ensure org
  :pin gnu
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
        org-agenda-files (append (file-expand-wildcards "~/[a-zA-Z]*/org")
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
    "o C"   'org-ref-helm-insert-cite-link
    "o L"   'org-ref-helm-insert-label-link
    "o R"   'org-ref-helm-insert-ref-link
    "p c"   'my-org-compile

    ;; Task management keybindings.
    "o t i" 'org-clock-in
    "o s"   'org-todo
    "o e"   'org-edit-special
    "o r f" 'helm-ejira-refile
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
    "o r f" 'helm-ejira-refile
    "o t r" 'org-clock-in-last
    "o t o" 'org-clock-out
    "o t t" 'org-clock-goto
    "o t i" 'org-clock-in))

(use-package org-agenda
  :ensure nil
  :commands (org-add-agenda-custom-command))

(use-package org-ref)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "◆" "✸" "◇")))

(use-package org-bars
  :load-path "~/.emacs.d/lisp/org-bars"
  :hook  (org-mode . org-bars-mode))

(use-package fish-mode)

(use-package dash)
(use-package dash-functional)
(use-package ox-jira)
(use-package f)
(use-package s)
(use-package request)
(use-package language-detection)
(use-package jiralib2)
(use-package ejira
  :load-path "~/.emacs.d/lisp/ejira"
  :ensure nil
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

  :general
  (leader-def-key
    "j j"   'ejira-focus-item-under-point
    "o j l" 'ejira-insert-link-to-current-issue
    "o j j" 'ejira-focus-on-clocked-issue
    "o j m" 'ejira-mention-user
    "o j U" 'ejira-update-my-projects
    "o b"   'ejira-agenda-board)

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

(use-package helm-ejira
  :load-path "~/.emacs.d/lisp/ejira"
  :ensure nil
  :config
  (helm-ejira-advice)
  :general
  (leader-def-key
    :keymaps 'override
    "J"     'helm-ejira-focus-issue
    "K"     'helm-ejira-focus-issue-active-sprint
    "L"     'helm-ejira-focus-issue-assigned))

(use-package ejira-agenda
  :ensure nil
  :config

  (defun my-add-ejira-kanban-board (key board-name &optional title)
    (setq title (or title board-name))
    (org-add-agenda-custom-command
     `(,key ,title
            ((ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and resolution = unresolved "
                                "and assignee = currentUser()")
                        ((org-agenda-overriding-header
                          ,(concat title "\n\nAssigned to me"))))
             (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and resolution = unresolved "
                                "and assignee is EMPTY")
                        ((org-agenda-overriding-header "Unassigned")))
             (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and resolution = unresolved "
                                "and assignee != currentUser()")
                        ((org-agenda-overriding-header "Others")))))))

  (defun my-add-ejira-scrum-board (key board-name &optional title)
    (setq title (or title board-name))
    (org-add-agenda-custom-command
     `(,key ,title
            ((ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and sprint in openSprints() "
                                "and assignee = currentUser()")
                        ((org-agenda-overriding-header
                          ,(concat title "\n\nAssigned to me"))))
             (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and sprint in openSprints() "
                                "and assignee is EMPTY")
                        ((org-agenda-overriding-header "Unassigned")))
             (ejira-jql (concat "filter = \"Filter for " ,board-name "\" "
                                "and sprint in openSprints() "
                                "and assignee != currentUser()")
                        ((org-agenda-overriding-header "Others")))))))

  ;; my-ejira-kanban-boards is of form (("key" "name") ("key" "name") ...)
  (mapc (-partial #'apply #'my-add-ejira-kanban-board) my-ejira-kanban-boards)
  (mapc (-partial #'apply #'my-add-ejira-scrum-board) my-ejira-scrum-boards))

(provide 'init)
;;; init.el ends here
