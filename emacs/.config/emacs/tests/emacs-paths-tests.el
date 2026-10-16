;;; emacs-paths-tests.el --- Startup path tests -*- lexical-binding: t -*-

;; Run separately from the normal init: emacs -Q --batch -l this-file
;; -f ert-run-tests-batch-and-exit.  No package downloads are needed.
(require 'ert)
(require 'cl-lib)

(defconst nyy/paths-test-config-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (file-truename (or load-file-name buffer-file-name))))))

(load (expand-file-name "early-init.el" nyy/paths-test-config-directory) nil t)

(ert-deftest nyy/paths-separate-code-and-data ()
  (should (equal user-emacs-directory (expand-file-name "~/.local/share/emacs/")))
  (should (equal user-lisp-directory
                 (expand-file-name "user-lisp/" nyy/paths-test-config-directory)))
  (should-not user-lisp-auto-scrape)
  (dolist (library '("nyy-agent-shell-session" "nyy-agent-shell-mcp"
                     "consult-agent-shell-sessions"))
    (should (file-in-directory-p (locate-library library) user-lisp-directory))))

(ert-deftest nyy/paths-auto-saves-stay-in-their-directory ()
  (let ((directory (expand-file-name "var/auto-save/" user-emacs-directory)))
    (should (file-directory-p directory))
    (should (equal (cadar auto-save-file-name-transforms) directory))
    (dolist (name '("/home/example/project/file.el" "/home/example/other/file.el"))
      (with-temp-buffer
        (setq buffer-file-name name)
        (should (equal (file-name-directory (make-auto-save-file-name)) directory))))))

(ert-deftest nyy/paths-emacs-30-fallback ()
  (let ((load-path (copy-sequence load-path)))
    (setq load-path (delete (directory-file-name user-lisp-directory) load-path))
    (cl-letf (((symbol-function 'prepare-user-lisp) nil))
      (load (expand-file-name "early-init.el" nyy/paths-test-config-directory) nil t)
      (should (member (directory-file-name user-lisp-directory) load-path)))))

(ert-deftest nyy/paths-native-activation-is-left-to-emacs-31 ()
  (let ((load-path (copy-sequence load-path)))
    (setq load-path (delete (directory-file-name user-lisp-directory) load-path))
    (cl-letf (((symbol-function 'prepare-user-lisp)
               (lambda (&rest _) (ert-fail "Early init must not activate user Lisp"))))
      (load (expand-file-name "early-init.el" nyy/paths-test-config-directory) nil t)
      (should-not (member (directory-file-name user-lisp-directory) load-path))
      (should-not user-lisp-auto-scrape))))

(ert-deftest nyy/paths-native-cache-respects-xdg ()
  (dolist (cache-home '(nil "" "relative/cache" "/tmp/opencode/xdg-cache"))
    (let ((process-environment (copy-sequence process-environment))
          redirected)
      (setenv "XDG_CACHE_HOME" cache-home)
      (cl-letf (((symbol-function 'startup-redirect-eln-cache)
                 (lambda (directory) (setq redirected directory))))
        (load (expand-file-name "early-init.el" nyy/paths-test-config-directory) nil t))
      (should (equal redirected
                     (if (and cache-home (file-name-absolute-p cache-home))
                         (expand-file-name "emacs/eln-cache/" cache-home)
                       (expand-file-name "~/.cache/emacs/eln-cache/")))))))

;;; emacs-paths-tests.el ends here
