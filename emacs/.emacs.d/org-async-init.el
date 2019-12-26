(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; This environment is required for pdflatex to find required resource files.
(setenv "TEXINPUTS" (concat ".:" (expand-file-name "~/texmf/tex/latex/local/") "::"))
(setenv "SHELL"     "/bin/zsh")
(setenv "LC_CTYPE"  "UTF-8")
(setenv "LC_ALL"    "en_US.UTF-8")
(setenv "LANG"      "en_US.UTF-8")

(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'org-ref)

(defvar export-finished-notification
      (if (eq system-type 'darwin)
          "osascript -e 'display notification \"exporting %f finished\" with title \"ox-latex\"'"
        "notify-send -t 3000 'Emacs' 'PDF export finished.' --icon=dialog-information"))

(setq org-latex-classes '(("optodoc" "\\documentclass{optodoc}"
                          ("\\section{%s}"       . "\\section*{%s}")
                          ("\\subsection{%s}"    . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

      org-export-async-debug           t
      org-latex-compiler               "xelatex"
      org-latex-listings               'minted
      org-src-preserve-indentation     t
      org-edit-src-content-indentation 0
      org-export-with-todo-keywords    nil
      org-export-with-smart-quotes     nil
      org-export-with-sub-superscripts nil
      org-export-with-latex            t
      org-latex-default-packages-alist nil
      org-export-with-toc              nil

      org-latex-pdf-process
      `("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"

        ;; Enable if a third pass is needed
        ;; "%bibtex %f"
        ;; "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"

        ,export-finished-notification))
