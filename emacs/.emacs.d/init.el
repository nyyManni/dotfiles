;; init.el --- Emacs configuration
;; Copyright (c) 2016 - 2019 Henrik Nyman

;; Author     : Henrik Nyman <h@nyymanni.com>
;; Created    : 10 Aug 2016
;; Version    : 0.1

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

;; nyyManni's configuration for Emacs, 2019 flavor.

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
  ;; Allow me to accidentally hit C-x C-c when using graphical Emacs.
  (global-unset-key (kbd "C-z"))
  (setq confirm-kill-emacs 'y-or-n-p))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (setq exec-path '("/home/hnyman/.cargo/bin"
                    "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims"
                    "/home/hnyman/.pyenv/shims"
                    "/home/hnyman/.pyenv/bin"
                    "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims"
                    "/home/hnyman/.pyenv/shims" "/home/hnyman/.pyenv/bin"
                    "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims"
                    "/home/hnyman/.pyenv/shims" "/home/hnyman/.pyenv/bin"
                    "/usr/local/bin" "/usr/bin" "/bin" "/usr/games"
                    "/usr/local/libexec/emacs/27.0.50/x86_64-pc-linux-gnu")

        load-prefer-newer  t)
  (setenv "PATH" (concat "/home/hnyman/.cargo/bin:"
                         "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims:"
                         "/home/hnyman/.pyenv/shims:/home/hnyman/.pyenv/bin:"
                         "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims:"
                         "/home/hnyman/.pyenv/shims:/home/hnyman/.pyenv/bin:"
                         "/home/hnyman/.pyenv/plugins/pyenv-virtualenv/shims:"
                         "/home/hnyman/.pyenv/shims:/home/hnyman/.pyenv/bin:"
                         "/usr/local/bin:/usr/bin:/bin:/usr/games"))
  (setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket")))

;; OS X specific settings
(when (eq system-type 'darwin)
  ;; TODO: Fix byte compiler warnings
  (setq exec-path                     (append exec-path '("/usr/local/bin"))
        default-input-method          "MacOSX"
        default-directory             "/Users/hnyman/"
        mac-command-modifier          'meta
        mac-option-modifier           nil
        mac-allow-anti-aliasing       t
        frame-resize-pixelwise        t
        ns-use-srgb-colorspace        nil
        mouse-wheel-scroll-amount     '(5 ((shift) . 5) ((control)))
        mouse-wheel-progressive-speed nil)

  ;; Environment variables
  (setenv "PATH" (concat "/usr/local/bin:/usr/local/opt/texinfo/bin:"
                         "/usr/local/texlive/2018/bin/x86_64-darwin:/usr/bin:"
                         "/bin:/usr/sbin:/sbin:/Users/hnyman/bin:"
                         "/Users/hnyman/.cargo/bin"))
  (setenv "SHELL"         "/bin/zsh")
  (setenv "LC_CTYPE"      "UTF-8")
  (setenv "LC_ALL"        "en_US.UTF-8")
  (setenv "LANG"          "en_US.UTF-8")
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "launchctl getenv SSH_AUTH_SOCK | tr -d '\n'")))

;; Default values for configuration that is overridden in the private config.
(defvar my-ejira-projects      '("EJ" "JL2"))
(defvar my-ejira-server        "https://localhost:8080")
(defvar my-ejira-kanban-boards nil)
(let ((work-config (concat user-emacs-directory "/work-config.el")))
  (when (file-exists-p work-config)
    (load-file (concat user-emacs-directory "/work-config.el"))))


(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(require 'package)
(setq package-user-dir   "~/.emacs.d/elpa"
      package-quickstart t
      package-archives   '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/")))
(if (version< emacs-version "27")
    (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure     t
        use-package-check-before-init t))

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
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (apply command args)
    (rename-buffer (concat (symbol-name command) " " name "*"))))

(use-package f
  :functions (f-dirname))
(use-package dash-functional
  :commands (-partial))

(use-package general
  :commands (general-define-key general-chord)
  :functions (space-leader)
  :config
  (function-put #'general-define-key 'lisp-indent-function 'defun)
  (function-put #'general-create-definer 'lisp-indent-function 'defun)

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
  (global-set-key (kbd "M-?") 'help-command)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)

  (general-create-definer space-leader
    :states '(normal visual insert emacs)
    :global-prefix "C-c"
    :non-normal-prefix "M-SPC"
    :prefix "SPC")

  (function-put #'space-leader 'lisp-indent-function 'defun)

  (defun my-dired-here ()
    (interactive)
    (dired (f-dirname (or (buffer-file-name) "~/dummy"))))

  (space-leader
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
    "S"   'delete-trailing-whitespace
    "0"   'delete-window
    "1"   'delete-other-windows
    "2"   'split-window-below
    "3"   'split-window-right))

(use-package evil
  :hook (after-init . evil-mode)
  :commands (evil-set-initial-state evil-select-search-module)
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil
        evil-search-module    'evil-search
        evil-want-C-d-scroll  t
        evil-want-C-u-scroll  t
        evil-want-C-i-jump    t)
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (general-define-key
    :keymaps '(evil-normal-state-map)
    "M-." nil)
  :general
  (general-define-key
    :states '(normal insert visual)
    "C-S-z" 'evil-emacs-state)
  (general-define-key
    :states '(emacs)
    "C-S-z" 'evil-normal-state)
  (space-leader
    :keymaps 'override
    "h" 'evil-ex-nohighlight))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package key-chord
  :hook (after-init . (lambda () (key-chord-mode t)))
  :config
  (dolist (chord '("jk" "kj" "JK" "KJ" "jK" "kJ" "Jk" "Kj"))
    (general-define-key
      :keymaps '(evil-insert-state-map evil-visual-state-map)
      (general-chord chord) 'evil-normal-state)))

(use-package helm
  :hook (after-init . helm-mode)
  :commands (helm-autoresize-mode helm-get-selection helm-preselect)
  :functions (helm-skip-dots helm-ff-move-to-first-real-candidate)
  :defines (helm-idle-delay helm-quick-update helm-M-x-requires-pattern
                            helm-ff-skip-boring-files helm-ff-search-library-in-sexp
                            helm-ff-file-name-history-use-recentf)
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
  :general
  (general-define-key
    :keymaps '(helm-map)
    "C-i" 'helm-execute-persistent-action
    "C-k" 'helm-previous-line
    "C-j" 'helm-next-line)
  (general-define-key
    "M-x" 'helm-M-x)
  (space-leader
    :keymaps 'override
    "x" 'helm-M-x
    "O" 'helm-occur
    "A" 'helm-apropos
    "y" 'helm-show-kill-ring
    "f" 'helm-imenu
    ";" 'helm-find-files
    "b" 'switch-to-buffer))

(use-package helm-xref)
(use-package helm-ag)

(use-package projectile
  :functions (projectile-register-project-type)
  :commands (projectile-register-project-type)
  :init
  (setq projectile-completion-system 'default
        projectile-enable-caching    t
        projectile-use-git-grep      t)
  :config
  (projectile-register-project-type 'python  '("setup.py")
                                    :compile "python setup.py bdist_wheel"
                                    :test    "pytest tests")
  (projectile-mode))

(use-package helm-projectile
  :general
  (space-leader
    "G P" 'helm-projectile-ag
    "G r" 'rgrep
    "'"   'helm-projectile-switch-project
    ":"   'helm-projectile-find-file))

(use-package smartparens
  :defines (sp--special-self-insert-commands)
  :hook
  (after-init . smartparens-global-mode)
  (after-init . show-smartparens-global-mode)
  :init
  (setq-default sp-escape-quotes-after-insert nil)
  :commands (sp-pair)
  :config

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))

  (require 'smartparens-config)
  (general-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "C->" 'sp-forward-slurp-sexp)
  (sp-pair "{%" "%}"))

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

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

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

;; TODO: evil-ediff
;;       evil-textobj-anyblock evil-collection

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (nxml-mode . flycheck-mode)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :general
  (general-define-key
    :states '(normal visual)
    "[ e" 'flycheck-previous-error
    "] e" 'flycheck-next-error))

(use-package posframe)
;; (use-package flymake-posframe
;;   :load-path "~/.emacs.d/lisp/flymake-posframe"
;;   :hook (flymake-mode . flymake-posframe-mode))

(use-package undo-tree
  :general
  (general-define-key
    :keymaps '(undo-tree-map)
    "C-/" nil
    "C-?" nil
    )
  (space-leader
    :keymaps 'override
    "u" 'undo-tree-visualize))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-capture-mode . evil-insert-state)
  :init
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

        my-misc-org-file                       "~/org/MISC.org"
        org-indirect-buffer-display            'other-window

        org-lowest-priority    ?G
        org-priority-faces     '((?A . (:foreground "#c23127" :weight 'bold))
                                 (?B . (:foreground "#c23127"))
                                 (?C . (:foreground "#d26937"))
                                 (?E . (:foreground "#2aa889"))
                                 (?F . (:foreground "gray"))
                                 (?G . (:foreground "gray")))
        org-agenda-files       '("~/org")
        org-agenda-sticky      t
        org-todo-keywords
        '((sequence "BLOG(b)" "TODO(t)" "NEXT(p)" "TEST" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "|" "CANCELLED(c@/!)" "MEET(m)" "NOTE(n)"))

        org-todo-keyword-faces '(("TODO" :foreground "#c23127"    :weight bold)
                                 ("BLOG" :foreground "#343a40"    :weight bold)
                                 ("NEXT" :foreground "#d26937e"   :weight bold)
                                 ("IMPL" :foreground "light blue" :weight bold)
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
  (space-leader
    :keymaps '(bibtex-mode-map)
    "i" 'org-ref-clean-bibtex-entry)

  (space-leader
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
  (space-leader
    :keymaps '(org-src-mode-map)
    "o e"   'org-edit-src-exit)

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
  (space-leader
    :keymaps 'override
    "o a"   'org-agenda
    "o c"   'org-capture
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
  (setq request--curl-cookie-jar ""
        jiralib2-user-login-name "hnyman"
        jiralib2-url              my-ejira-server
        jiralib2-auth            'cookie

        ejira-org-directory      "/home/hnyman/org"
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
                                   ("Testing"                  . 4)
                                   ("Closed"                   . 5)
                                   ("Done"                     . 5))
        ejira-projects           my-ejira-projects)
  :config
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  :general
  (space-leader
    "j j"   'ejira-focus-item-under-point
    "o j l" 'ejira-insert-link-to-current-issue
    "o j j" 'ejira-focus-on-clocked-issue
    "o j m" 'ejira-mention-user
    "o j U" 'ejira-update-my-projects
    "o b"   'ejira-agenda-board)
  (space-leader
    :keymaps '(org-mode-map)
    "o j t" 'ejira-set-issuetype
    "o j c c" 'ejira-add-comment
    "o j c d" 'ejira-delete-comment
    "o j a" 'ejira-assign-issue
    "o j P" 'ejira-push-item-under-point
    "o j u" 'ejira-pull-item-under-point
    "o j p" 'ejira-progress-issue)

  (general-define-key
    :keymaps 'ejira-mode-map
    "C-S-q"  'ejira-close-buffer))

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

  ;; my-ejira-kanban-boards is of form (("key" "name") ("key" "name") ...)
  (mapc (-partial #'apply #'my-add-ejira-kanban-board) my-ejira-kanban-boards))

(use-package helm-ejira
  :load-path "~/.emacs.d/lisp/ejira"
  :ensure nil
  :general
  (space-leader
    :keymaps 'override
    "J"     'helm-ejira-focus-issue
    "K"     'helm-ejira-focus-issue-active-sprint
    "L"     'helm-ejira-focus-issue-assigned))

(use-package ejira-hourmarking
  :load-path "~/.emacs.d/lisp/ejira"
  :ensure nil
  :general
  (general-define-key
    :states '(normal)
    :keymaps '(ejira-hourlog-mode-map)
    "q" 'ejira-hourlog-quit)
  (space-leader
    :keymaps 'override
    "o j h" 'ejira-hourmarking-get-hourlog))

(use-package windmove
  :ensure nil
  :functions (my-frame-pos-x my-frame-not-current-but-visible-p
                             my-frame-to my-frame-center-pos my-windmove-advice)
  :config
  (defun my-frame-pos-x (frame)
    "Get the x position of the FRAME on display.
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

  (defun my-frame-center-pos (&optional frame)
    `(,(/ (frame-pixel-width frame) 2)
      ,(/ (frame-pixel-height frame) 2)))

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
               (user-error nil))
             ;; Move the mouse to the middle of the new frame. The frame switch
             ;; may have moved the focus into a new monitor, but all of the
             ;; keyboard shortcuts work on the monitor that currently has the
             ;; mouse.
             (apply 'set-mouse-pixel-position (cons (selected-frame)
                                                    (my-frame-center-pos))))
         (signal (car err) (cdr err))))))

  (advice-add 'windmove-do-window-select :around #'my-windmove-advice)

  :general
  (general-define-key
    "C-S-j"  'windmove-down
    "C-S-k"  'windmove-up
    "C-S-h"  'windmove-left
    "C-S-l"  'windmove-right))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
;; Show pretty icons
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

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
  (space-leader
    :keymaps 'override
    "g g"   'magit-status
    "g b"   'magit-blame
    "g f h" 'magit-log-buffer-file))

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (set-face-attribute 'git-gutter:deleted nil
                      :height 10
                      :width 'ultra-condensed
                      :foreground (face-attribute 'error :foreground)
                      :background (face-attribute 'error :foreground))
  (set-face-attribute 'git-gutter:added nil
                      :height 10
                      :width 'ultra-condensed
                      :foreground (face-attribute 'font-lock-string-face :foreground)
                      :background (face-attribute 'font-lock-string-face :foreground))
  (set-face-attribute 'git-gutter:modified nil
                      :height 10
                      :width 'ultra-condensed
                      :foreground (face-attribute 'warning :foreground)
                      :background (face-attribute 'warning :foreground))
  (set-face-attribute 'git-gutter:unchanged nil
                      :width 'ultra-condensed
                      :height 10)
  (set-face-attribute 'git-gutter:separator nil
                      :width 'ultra-condensed
                      :height 10)
  :general
  (space-leader
    "g d" 'git-gutter:popup-hunk
    "g u" 'git-gutter:revert-hunk))

(use-package yasnippet
  :functions (yas-reload-all)
  :commands (yas-reload-all)
  :config
  (setq yas-verbosity          1
        yas-wrap-around-region t)

  (yas-reload-all)
  (dolist (keymap '(yas-minor-mode-map yas-keymap))
    (define-key (eval keymap) (kbd "<tab>") nil)
    (define-key (eval keymap) [(tab)] nil)
    (define-key (eval keymap) (kbd "S-<tab>") nil)
    (define-key (eval keymap) [(shift tab)] nil)
    (define-key (eval keymap) [backtab] nil))

  (yas-global-mode)
  :general
  (general-define-key
    "C-;" 'yas-expand)
  (general-define-key
    :keymaps '(yas/keymap)
    "<tab>" nil
    "S-<iso-lefttab>" nil

    "C-;" 'yas-next-field
    "C-:" 'yas-prev-field))

(use-package yasnippet-snippets
  :ensure t)

(use-package eshell
  :ensure nil
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
    (concat
     "┌─" (eshell/whoami) "@" user-hostname " " (abbreviate-file-name (eshell/pwd)) "\n"
     "└─$ "))
  (setq eshell-prompt-regexp "└─\\$ ")   ; or "└─> "
  (setq eshell-prompt-function #'esh-prompt-func)

  :general
  (general-define-key
    :keymaps 'eshell-mode-map
    "C-S-q" 'my-quit-eshell)
  (space-leader
    :keymaps 'override
    "s e" 'my-eshell-here))

(use-package wdired)

(use-package systemd)
(use-package meson-mode)
(use-package yaml-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))


;;; Programming languages

(use-package lsp-mode
  :hook
  (c-mode      . lsp-deferred)
  (c++-mode    . lsp-deferred)
  (objc-mode   . lsp-deferred)
  (python-mode . lsp-deferred)
  (rust-mode   . lsp-deferred)
  (js2-mode    . lsp-deferred)
  :commands (lsp lsp-deferred)
  :defines (lsp-prefer-flymake lsp-enable-links)
  :init
  (setq lsp-prefer-flymake       nil
        lsp-file-watch-threshold 30000
        lsp-enable-links         nil
        lsp-pyls-plugins-pycodestyle-enabled nil)
  :config
  (require 'lsp-clients)
  ;; Override the default pyls client with one aware of pyvenv library files
  (eval-after-load "lsp-pyls"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda () lsp-clients-python-command))
      :major-modes '(python-mode cython-mode)
      :priority -1
      :server-id 'pyls
      :library-folders-fn
      (lambda (workspace)
        (let ((b (nth 0 (lsp--workspace-buffers workspace))))

          ;; If there are no buffers yet, we cannot have a library file for this
          ;; workspace, as we are most likely opening the first project file.
          (when b
            (with-current-buffer b
              (remq nil (list
                         (pyvenv-workon-home)
                         "/usr"
                         pyvenv-activate

                         ;; The python-binary is a symlink if the directory is a
                         ;; virtual environment. Include the libraries in the
                         ;; main env as well.
                         (when pyvenv-activate
                           (f-parent
                            (f-parent
                             (file-truename (f-join pyvenv-activate "bin" "python")))))))))))
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                                            (lsp--set-configuration
                                             (lsp-configuration-section "pyls"))))))))

(use-package hydra)
(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :init
  (setq company-lsp-cache-candidates t))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; C/C++
(use-package ccls :after lsp-mode
  :config

  ;; Disable other flycheck backends, only use lsp
  (setq-default flycheck-disabled-checkers '(c/c++-gcc c/c++-clang))

  ;; Override the client definition with library-folder awareness
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () (cons ccls-executable ccls-args)))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls
    :multi-root nil
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn (lambda (_ws) '("/usr" "/opt")))))

;; Python
(use-package pyvenv
  :hook (python-mode . pyvenv-tracking-mode)
  :functions (pyvenv-virtualenvwrapper-supported)
  :commands (pyvenv-virtualenvwrapper-supported)
  :config

  ;; Don't waste time with virtualenvwrapper. It is slow and we don't use it
  (advice-add #'pyvenv-virtualenvwrapper-supported :override (lambda (&rest a))))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save)
  :defines (py-autopep8-options)
  :init
  ;; Set the line-length for autopep to something large so that it
  ;; does not touch too long lines, it usually cannot fix them properly
  (setq py-autopep8-options '("--max-line-length=200"))
  :general
  (space-leader
    :keymaps '(python-mode-map)
    "p f" 'py-autopep8-buffer))

;; Rust
(use-package rust-mode)

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors    nil))

(use-package rjsx-mode)

;; Debuggers
(use-package dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(use-package systemd)

;;; E-mail

(use-package mu4e
  :ensure nil
  :init
  (setq mu4e-maildir        "~/.mail"
        mu4e-attachment-dir "~/downloads"
        mu4e-sent-folder    "/sent"
        mu4e-drafts-folder  "/drafts"
        mu4e-trash-folder   "/trash"
        mu4e-refile-folder  "/archive"
        mu4e-confirm-quit   nil

        ;; Get mail
        mu4e-get-mail-command             "mbsync protonmail"
        mu4e-change-filenames-when-moving t
        mu4e-update-interval              120

        ;; Send mail
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-user         user-mail-address
        smtpmail-smtp-server       "127.0.0.1"
        smtpmail-smtp-service      1025)

  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :general
  (space-leader
    :keymaps 'override
    "M" 'mu4e))

(use-package mu4e-maildirs-extension
  :after 'mu4e
  :commands (mu4e-maildirs-extension)
  :config
  (mu4e-maildirs-extension))

(use-package mu4e-conversation
  :after 'mu4e)

(use-package lunchtime
  :ensure nil
  :load-path "~/.emacs.d/lisp/lunchtime"
  :commands (lunchtime-display-menus)
  :config

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
   "https://www.sodexo.fi/ruokalistat/output/daily_json/9870/%Y/%m/%d/en"
   `(((name . ,(assoc-recursive lunchtime-response-data 'meta 'ref_title))
      (subrestaurants
       .
       (((name . "Lounas") ;; Sodexo has only one restaurant per menu item
         (menus . ,(mapcar
                    (lambda (item)
                      `((name . ,(assoc-recursive item 'category))
                        (prices . (,(assoc-recursive item 'price)))
                        (menu . (, (concat
                                    (assoc-recursive item 'title_en)
                                    " (" (assoc-recursive item 'properties) ")")))))
                    (assoc-recursive lunchtime-response-data 'courses)))))))))

  ;; Hermia 5
  (lunchtime-define-restaurant
   "https://www.sodexo.fi/ruokalistat/output/daily_json/134/%Y/%m/%d/en"
   `(((name . ,(assoc-recursive lunchtime-response-data 'meta 'ref_title))
      (subrestaurants
       .
       (((name . "Lounas") ;; Sodexo has only one restaurant per menu item
         (menus . ,(mapcar
                    (lambda (item)
                      `((name . ,(assoc-recursive item 'category))
                        (prices . (,(assoc-recursive item 'price)))
                        (menu . (, (concat
                                    (assoc-recursive item 'title_en)
                                    " (" (assoc-recursive item 'properties) ")")))))
                    (assoc-recursive lunchtime-response-data 'courses)))))))))

  :general
  (space-leader
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

;;; init.el ends here
