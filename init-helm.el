;;; init-helm.el --- Helm
;;; Commentary:
;;; Code:

(require-package 'helm)

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

(provide 'init-helm)
;;; init-helm.el ends here
