;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:

(require-package 'projectile)

;; Indexing method
(setq projectile-indexing-method 'native)

;; Switching projects
;; (setq projectile-switch-project-action 'projectile-dired)
(setq projectile-switch-project-action 'projectile-find-dir)
(setq projectile-find-dir-includes-top-level t)

(projectile-global-mode)

(provide 'init-projectile)
;;; init-projectile.el ends here
