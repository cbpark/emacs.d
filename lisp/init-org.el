;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(eval-after-load 'org
  '(progn
     (setq org-agenda-files (list "~/Documents/org/diary"))
     (setq org-export-html-validation-link nil)
     (setq org-export-with-toc nil)
     (setq org-src-fontify-natively t)
     (add-hook 'org-mode-hook #'(lambda ()
                                  (linum-mode 1)))))

;; Org Publishing
(require-package 'htmlize)
(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Documents/org/notes"
         :base-extension "org"
         :publishing-directory "~/Dropbox/org/notes"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :auto-preamble t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap")
        ("org-static"
         :base-directory "~/Documents/org/notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/org/notes"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("org-notes" "org-static"))))

;; LaTeX export
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
\\setlength{\\parindent}{1cm}
\\setlength{\\parskip}{5pt plus 2pt minus 1pt}
\\renewcommand{\\baselinestretch}{1.15}
\\interfootnotelinepenalty=10000
\\raggedbottom
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

;; Org journal
(require-package 'org-journal)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(eval-after-load 'org-journal
  '(progn
     (setq org-journal-dir "~/Documents/org/diary/")
     (setq org-journal-date-format "%Y-%m-%d (%A)")
     (setq org-journal-file-format "%Y%m%d.org")))

(provide 'init-org)
;;; init-org.el ends here
