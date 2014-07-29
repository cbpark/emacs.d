;;; init-helm.el --- Helm
;;; Commentary:
;;; Code:

(require-package 'helm)
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-utils)
(require 'helm-files)
(require 'helm-grep)
(helm-mode 1)

(eval-after-load 'helm
  '(progn
     ;; Yanking text
     (setq helm-yank-symbol-first 't)
     ;; Buffer File Completion
     (setq enable-recursive-minibuffers t)
     ;; Ignore files
     (setq helm-ff-skip-boring-files t)
     (setq helm-boring-file-regexp-list '("\\.elc$" "\\.o$" "\\.hi$" "\\.pyc$"
                                          "\\.localized$" "\\.DS_Store$" "\\.git$"
                                          "\\.hg$" "\\.svn$"))
     ;;
     (setq helm-scroll-amount 4
           helm-quick-update t
           helm-idl-delay 0.01
           helm-input-idle-delay 0.01
           helm-split-window-default-side 'other
           helm-split-window-in-side-p t
           helm-candidate-number-limit 200
           helm-M-x-require-pattern 0
           helm-ff-file-name-history-use-recentf t
           helm-move-to-line-cycle-in-source t
           helm-buffers-fuzzy-matching t)

     ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))

     ;; Save curren position when jumping
     (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "C-z") 'helm-select-action)

     (define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
     (define-key helm-grep-mode-map (kbd "n") 'helm-grep-mode-jump-other-window-forward)
     (define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)))

;; key bindings
(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "M-y")       'helm-show-kill-ring)
(global-set-key (kbd "C-x b")     'helm-mini)
(global-set-key (kbd "C-x C-f")   'helm-find-files)
(global-set-key (kbd "C-x C-r")   'helm-recentf)
(global-set-key (kbd "C-c h s")   'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h m")   'helm-man-woman)
(global-set-key (kbd "C-c h f")   'helm-find)
(global-set-key (kbd "C-c h l")   'helm-locate)
(global-set-key (kbd "C-c h g")   'helm-do-grep)
(global-set-key (kbd "C-c h o")   'helm-occur)
(global-set-key (kbd "C-c h r")   'helm-resume)
(global-set-key (kbd "C-c SPC")   'helm-all-mark-rings)

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

;; helm-ls-git
(require-package 'helm-ls-git)
(global-set-key (kbd "C-c g")   'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(provide 'init-helm)
;;; init-helm.el ends here
