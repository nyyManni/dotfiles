;;; init.el --- Emacs configuration root

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

;; nyyManni's configuration for Emacs.
;; init.el compiles andloads the configuration from an org-file

;;; Code:

;; Global settings
(setq user-full-name "Henrik Nyman"
      user-login-name "nyman"
      user-mail-address "henrikjohannesnyman@gmail.com"
      user-emacs-directory "~/.emacs.d")

(package-initialize nil)
(setq package-enable-at-startup nil)
(org-babel-load-file (concat user-emacs-directory "/config.org"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-jedi comment-dwim-2 key-chord s evil-nerd-commenter evil-escape evil-commentary evil-magit evil-smartparens powerline-evil evil yaml-mode xquery-mode web-mode virtualenvwrapper vimgolf use-package tagedit systemd smartparens realgud pyenv-mode-auto purty-mode powerline php-mode phi-search-mc nvm nodejs-repl multi-web-mode mc-extras markdown-mode magit lua-mode latex-preview-pane json-rpc json-mode js3-mode js2-mode js-comint java-snippets irony impatient-mode iedit hiwin helm-projectile-all helm-projectile helm-perldoc helm-gtags helm-git helm-flycheck helm-company helm-c-yasnippet handlebars-sgml-mode handlebars-mode groovy-mode gotham-theme google-c-style ggtags fuzzy-match function-args fringe-helper flymake-lua flymake-jshint flymake-google-cpplint flymake-cursor flymake-cppcheck flycheck-google-cpplint fill-column-indicator expand-region emacs-eclim e2wm csv-mode cperl-mode company-tern company-quickhelp company-c-headers company-auctex company-anaconda column-marker coffee-mode clang-format autopair auto-complete-clang auto-complete-c-headers android-mode ace-window ac-helm))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
