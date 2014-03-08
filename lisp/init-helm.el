;;; init-helm.el --- Helm
;;; Commentary:
;;; Code:

(require-package 'helm)

(require 'helm-config)
(require 'helm-utils)
(helm-mode 1)

;; Yanking text
(setq helm-yank-symbol-first 't)

;; Buffer File Completion
(setq enable-recursive-minibuffers t)

;; Ignore files
(setq helm-ff-skip-boring-files t)
(setq helm-boring-file-regexp-list '("\\.elc$" "\\.o$"))

(eval-after-load "helm-files"
  '(progn
     (define-key helm-find-files-map (kbd "C-z") nil)
     (define-key helm-find-files-map (kbd "C-h") 'helm-ff-backspace)
     (define-key helm-find-files-map (kbd "C-i") 'helm-execute-persistent-action)))

;; key bindings
(global-set-key (kbd "M-x")         'helm-M-x)
(global-set-key (kbd "C-c m")       'helm-mini)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "C-x C-r")     'helm-recentf)
(global-set-key (kbd "C-c i")       'helm-imenu)
(global-set-key (kbd "C-x b")       'helm-buffers-list)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)

;; helm-swoop
(require-package 'helm-swoop)
(global-set-key (kbd "M-i")     'helm-swoop)
(global-set-key (kbd "M-I")     'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(eval-after-load "helm-swoop"
  '(progn
     ;; Save buffer when helm-multi-swoop-edit complete
     (setq helm-multi-swoop-edit-save t)
     ;; If this value is t, split window inside the current window
     (setq helm-swoop-split-with-multiple-windows nil)
     ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
     (setq helm-swoop-split-direction 'split-window-vertically)
     ;; When doing isearch, hand the word over to helm-swoop
     (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
     ;; From helm-swoop to helm-multi-swoop-all
     (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

;; helm-flycheck
(require-package 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; helm projectile
(require-package 'helm-projectile)
(eval-after-load "projectile"
  '(progn
     (define-key projectile-mode-map (kbd "C-c h") 'helm-projectile)))

;; helm-ls-git
(require-package 'helm-ls-git)
(global-set-key (kbd "C-c g")   'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(provide 'init-helm)
;;; init-helm.el ends here
