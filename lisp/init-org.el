;;; init-org.el --- Customizing org mode
;;; Commentary:
;;; Code:

(require 'org)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(setq org-directory "~/Documents/org")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(provide 'init-org)
;;; init-org.el ends here
