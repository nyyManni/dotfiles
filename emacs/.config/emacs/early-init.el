;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Filename: early-init.el
;; Description: Early initialization
;; Author: Henrik Nyman
;; Copyright (C) 2021 Henrik Nyman
;; Version: 1.0
;; Keywords:.emacs.d init early-init
;; Compatibility: emacs-version >= 28
;;
;;; Commentary:
;;
;;
;;; Code:

;; Capture the config location before redirecting generated files elsewhere.
;; This works with both a Stow directory symlink and individual init symlinks.
(defconst nyy/emacs-config-directory
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory containing the version-controlled Emacs configuration.")

(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/")
      package-user-dir user-emacs-directory)

;; Set this explicitly before loading packages; changing user-emacs-directory
;; alone does not redirect the native cache until later in startup.
;; The helper can exist even when native compilation is unavailable.
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (let ((cache-home (getenv "XDG_CACHE_HOME")))
    (startup-redirect-eln-cache
     (expand-file-name "emacs/eln-cache/"
                       (if (and cache-home (file-name-absolute-p cache-home))
                           cache-home
                         "~/.cache/")))))

;; Emacs 31 activates this directory between early-init.el and init.el.
;; Our libraries require Straight packages, unavailable at that point, so
;; leave compilation/autoload scraping off and require them explicitly later.
(defvar user-lisp-directory)
(defvar user-lisp-auto-scrape)
(setq user-lisp-directory
      (expand-file-name "user-lisp/" nyy/emacs-config-directory)
      user-lisp-auto-scrape nil)

;; Emacs 30 compatibility: the current libraries are all in the top level.
(unless (fboundp 'prepare-user-lisp)
  (add-to-list 'load-path (directory-file-name user-lisp-directory)))

;; A directory replacement MUST end in a slash; otherwise Emacs puts the
;; flattened auto-save names in its parent directory.  Keep recovery data.
(let ((directory (expand-file-name "var/auto-save/" user-emacs-directory)))
  (make-directory directory t)
  (setq auto-save-file-name-transforms `((".*" ,directory t))))

(when (eq system-type 'gnu/linux)
  (setenv "PATH" (concat (expand-file-name "~/.local/bin")
                         ":"
                         (getenv "PATH")
                         ":"
                         (expand-file-name "~/.local/share/cargo/bin")
                         ":"
                         (expand-file-name "~/.dotnet/tools"))))

(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)

(setq-default display-line-numbers-width 4)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(undecorated-round . t) default-frame-alist))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (eq system-type 'gnu/linux)
  (push '(font . "DejaVu Sans Mono-8") default-frame-alist))
(when (eq system-type 'darwin)
  ;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (push '(inhibit-double-buffering . t) default-frame-alist)
  (push '(font . "Fira Code Nerd Font Mono") default-frame-alist))

;; (set-face-attribute 'default nil 
;;                     :family "DejaVu Sans Mono-8" 
;;                     :height 1) 
(set-frame-font (font-spec :family "DejaVu Sans Mono" :size 8.0) nil t)

;; Seed the startup frame only; later clients must inherit the active theme.
(add-to-list 'initial-frame-alist '(background-color . "#0c1014"))
(add-to-list 'initial-frame-alist '(foreground-color . "#99d1ce"))

;; disable this for now, the performance is absolutely horrible
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(alpha . (100 97))))
;; (add-to-list 'default-frame-alist '(alpha . (97 97))))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(global-hl-line-mode 1)
(column-number-mode)


(provide 'early-init)
;;; early-init.el ends here
