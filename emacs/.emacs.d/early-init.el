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
  (push '(font . "Fira Code") default-frame-alist))

(add-to-list 'default-frame-alist '(background-color . "#0c1014"))
(add-to-list 'default-frame-alist '(foreground-color . "#99d1ce"))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(global-hl-line-mode 1)
(column-number-mode)


(provide 'early-init)
;;; early-init.el ends here
