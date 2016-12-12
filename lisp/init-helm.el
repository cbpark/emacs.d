;;; init-helm.el --- Helm
;;; Commentary:
;;; Code:

(require-package 'helm)
(require 'helm-config)
(require 'helm-utils)
(require 'helm-files)

(helm-autoresize-mode t)
(setq helm-autoresize-max-height 30
      helm-autoresize-min-height 30)
(setq enable-recursive-minibuffers t)  ;; buffer file completion
(setq helm-ff-skip-boring-files t)     ;; ignore files
(setq helm-boring-file-regexp-list
      '("\\.elc$" "\\.o$" "\\.hi$" "\\.pyc$" "\\.localized$" "\\.DS_Store$"
        "\\.git$" "\\.hg$" "\\.svn$"))
(setq helm-scroll-amount 8
      helm-input-idle-delay 0.01
      helm-split-window-default-side 'other
      helm-split-window-in-side-p t
      helm-candidate-number-limit 200
      helm-ff-file-name-history-use-recentf t
      helm-move-to-line-cycle-in-source t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; Save curren position when jumping
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; key bindings
(with-eval-after-load 'helm
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   'helm-select-action))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h r") 'helm-resume)
(global-set-key (kbd "C-c SPC") 'helm-all-mark-rings)

(helm-mode 1)

;; helm-swoop
(require-package 'helm-swoop)
(global-set-key (kbd "M-i")     'helm-swoop)
(global-set-key (kbd "M-I")     'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(with-eval-after-load 'helm-swoop
  (setq helm-multi-swoop-edit-save t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t)

  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line))

;; helm-ls-git
(require-package 'helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-c h g") 'helm-grep-do-git-grep)

;; swiper
(require-package 'swiper-helm)
(global-set-key (kbd "C-s") 'swiper-helm)
(global-set-key (kbd "C-r") 'swiper-helm)

;; helm-descbinds
(require-package 'helm-descbinds)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(helm-descbinds-mode)

(provide 'init-helm)
;;; init-helm.el ends here
