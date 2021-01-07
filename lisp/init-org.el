;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)

;; Set the major mode of the initial *scratch* buffer to be org-mode
(setq initial-major-mode 'org-mode)

;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(setq org-directory "~/Documents/org"
      org-agenda-files (directory-files-recursively
                        "~/Documents/org/" "\\.org$")
      org-log-done 'time)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-export-with-toc nil
      org-export-with-timestamps nil
      org-export-with-creator nil
      org-html-validation-link nil)
(setq org-src-fontify-natively t
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0
      org-startup-indented t
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t)

;; turn on visual line mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; turn off auto fill mode
(add-hook 'org-mode-hook 'turn-off-auto-fill)

;; show inline images
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; LaTeX export
(setq org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
(setq org-latex-packages-alist '(("" "amsmath")
                                 ("" "amsfonts")
                                 ("" "amssymb")
                                 ("margin=1in" "geometry")
                                 ("" "mathtools")))
(setq org-highlight-latex-and-related '(latex script entities))
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; syntax highlighted source code for LaTeX
(setq org-latex-listings 'minted)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(require-package 'cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; markdown
(eval-after-load 'org '(require 'ox-md nil t))

(provide 'init-org)
;;; init-org.el ends here
