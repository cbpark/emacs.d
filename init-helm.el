;;; init-helm.el --- Helm
;;; Commentary:
;;; Code:

(require-package 'helm)

(require 'helm-config)
(require 'helm-utils)

;; Yanking text
(setq helm-yank-symbol-first 't)

;; Buffer File Completion
(setq enable-recursive-minibuffers t)

;; Enable helm pcomplete
(eval-after-load "eshell"
  '(progn
     ;; helm pcomplete
     (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
     ;; helm history
     (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

;; helm projectile
(require-package 'helm-projectile)
(eval-after-load "projectile"
  '(progn
     (define-key projectile-mode-map (kbd "C-c h") 'helm-projectile)))

;; helm-package
(require-package 'helm-package)

;; helm-swoop
(require-package 'helm-swoop)
(global-set-key (kbd "M-i")     'helm-swoop)
(global-set-key (kbd "M-I")     'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
(eval-after-load "helm-swoop"
  '(progn
     ;; When doing isearch, hand the word over to helm-swoop
     (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
     ;; From helm-swoop to helm-multi-swoop-all
     (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

;; key bindings
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-c m")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c i")   'helm-imenu)
(global-set-key (kbd "C-x b")   'helm-buffers-list)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)

(helm-mode 1)

(provide 'init-helm)
;;; init-helm.el ends here
