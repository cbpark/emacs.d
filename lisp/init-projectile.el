;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:

(require-package 'projectile)

(eval-after-load 'projectile
  '(progn
     ;; Indexing method
     (setq projectile-indexing-method 'native)
     (setq projectile-enable-caching t)

     ;; Switching projects
     ;; (setq projectile-switch-project-action 'projectile-dired)
     (setq projectile-switch-project-action 'projectile-find-dir)
     (setq projectile-find-dir-includes-top-level t)

     (setq projectile-tags-command "/usr/bin/ctags -e -R %s")))

(projectile-global-mode)

(provide 'init-projectile)
;;; init-projectile.el ends here
