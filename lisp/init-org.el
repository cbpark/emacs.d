;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(setq org-directory "~/Documents/org")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-export-with-toc nil
      org-export-with-timestamps nil
      org-export-with-creator nil
      org-html-validation-link nil)
(setq org-src-fontify-natively t
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

;; LaTeX export
(setq org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
(setq org-latex-packages-alist '(("" "bbm")
                                 ("" "calrsfs")
                                 ("small" "caption")
                                 ("margin=1in" "geometry")
                                 ("" "mathtools")
                                 ("" "slashed")))
(setq org-highlight-latex-and-related '(latex script entities))

(require-package 'cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; markdown
(eval-after-load 'org '(require 'ox-md nil t))

;; Set the major mode of the initial *scratch* buffer to be org-mode
(setq initial-major-mode 'org-mode)

(provide 'init-org)
;;; init-org.el ends here
