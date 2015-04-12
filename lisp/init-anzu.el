;;; init-anzu.el --- anzu.el
;;; Commentary:
;;; Code:

(require-package 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)

(setq anzu-mode-lighter ""
      anzu-deactivate-region t
      anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'init-anzu)
;;; init-anzu.el ends here