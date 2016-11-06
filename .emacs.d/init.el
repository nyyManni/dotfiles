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
