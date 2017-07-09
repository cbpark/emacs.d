;;; init-anzu.el --- anzu.el
;;; Commentary:
;;; Code:

(defconst *anzu-dir* (concat *site-lisp-dir* "anzu"))
(if (and *anzu-dir* (file-directory-p *anzu-dir*))
    (require 'anzu)
  (require-package 'anzu))
(global-anzu-mode t)

(set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
(setq anzu-mode-lighter ""
      anzu-deactivate-region t
      anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")

(global-set-key (kbd "M-%")   'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'init-anzu)
;;; init-anzu.el ends here
