;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)

(setq org-export-html-validation-link nil)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook (lambda ()
                           (auto-complete-mode 1)
                           (ac-ispell-ac-setup)
                           (yas-minor-mode)))
(ac-flyspell-workaround)

;; Org Publishing
(require 'org-publish)
(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Documents/org/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/org/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :auto-preamble t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap")
        ("org-static"
         :base-directory "~/Documents/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/org/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("org-notes" "org-static"))))

;; LaTeX export
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass[11pt,article]{article}
\\setlength{\\parindent}{1cm}
\\setlength{\\parskip}{5pt plus 2pt minus 1pt}
\\renewcommand{\\baselinestretch}{1.15}
\\usepackage{amsfonts}
\\usepackage{amsmath,bm}
\\usepackage{amssymb}
\\usepackage{bbm}
\\usepackage{cancel}
\\usepackage[small]{caption}
\\usepackage{enumerate}
\\usepackage[multiple]{footmisc}
\\usepackage{fullpage}
\\usepackage{mathtools}
\\usepackage{slashed}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require-package 'htmlize)

(provide 'init-org)
;;; init-org.el ends here
