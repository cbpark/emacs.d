;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:

(require-package 'projectile)
(setq projectile-keymap-prefix (kbd "C-c p"))

;; Indexing method
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)

;; Switching projects
(setq projectile-switch-project-action 'projectile-find-dir)
(setq projectile-find-dir-includes-top-level t)

;; Tags
(setq projectile-tags-command "ctags -e -R %s")

;; Use `vc-git-grep' in git projects
(setq projectile-use-git-grep t)

(projectile-mode)

;; helm projectile
(when *helm-on*
  (require-package 'helm-projectile)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
