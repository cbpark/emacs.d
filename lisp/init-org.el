;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(setq org-directory "~/Documents/org")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Set the major mode of the initial *scratch* buffer to be org-mode
(setq initial-major-mode 'org-mode)

;; Turn TOC off
(setq org-export-with-toc nil)

(eval-after-load 'org
  '(require 'ox-md nil t))

(provide 'init-org)
;;; init-org.el ends here
