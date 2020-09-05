;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(setq org-directory "~/Documents/org"
      org-agenda-files (file-expand-wildcards "~/Documents/org/*.org")
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
      org-edit-src-content-indentation 0)

;; LaTeX export
(setq org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
(setq org-latex-packages-alist '(("" "amsmath")
                                 ("" "amsfonts")
                                 ("" "amssymb")
                                 ("small" "caption")
                                 ("margin=1in" "geometry")
                                 ("" "mathtools")))
(setq org-highlight-latex-and-related '(latex script entities))

(require-package 'cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; markdown
(eval-after-load 'org '(require 'ox-md nil t))

;; Set the major mode of the initial *scratch* buffer to be org-mode
;; (setq initial-major-mode 'org-mode)

;; Languages
(org-babel-do-load-languages
 'org-babel-load-languages '((C          . t)
                             (emacs-lisp . t)
                             (gnuplot    . t)
                             (haskell    . t)
                             (latex      . t)
                             (lisp       . t)
                             (python     . t)
                             (sql        . t)))

;; show inline images
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(provide 'init-org)
;;; init-org.el ends here
