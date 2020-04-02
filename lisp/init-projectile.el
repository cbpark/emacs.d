;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:

(require-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Indexing method
(setq projectile-indexing-method 'alien
      projectile-enable-caching t)

;; Switching projects
(setq projectile-switch-project-action 'projectile-find-dir
      projectile-find-dir-includes-top-level t)

;; Tags
(setq projectile-tags-command "ctags -e -R %s")

;; Use `vc-git-grep' in git projects
(setq projectile-use-git-grep t)

;; helm projectile
(when *helm-on*
  (require-package 'helm-projectile)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))

;; Haskell
(projectile-register-project-type 'haskell-stack '("stack.yaml")
                                  :compile "stack build --fast"
                                  :test "stack build --test"
                                  :test-suffix "Spec")

(provide 'init-projectile)
;;; init-projectile.el ends here
