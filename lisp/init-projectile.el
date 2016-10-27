;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:

(require-package 'projectile)
(eval-after-load 'projectile
  '(progn
     ;; Indexing method
     (setq projectile-indexing-method 'alien)
     (setq projectile-enable-caching t)
     ;; Switching projects
     (setq projectile-switch-project-action 'projectile-find-dir)
     (setq projectile-find-dir-includes-top-level t)
     ;; tags
     (setq projectile-tags-command "/usr/bin/ctags -e -R %s")))
(projectile-mode)

;; helm projectile
(require-package 'helm-projectile)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)

(provide 'init-projectile)
;;; init-projectile.el ends here
