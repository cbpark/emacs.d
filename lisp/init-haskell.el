;;; init-haskell.el --- Haskell mode
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)

(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(setq auto-mode-alist
      (append
       '(("\\.hs$"    . haskell-mode)
         ("\\.hsc$"   . haskell-mode)
         ("\\.cpphs$" . haskell-mode)) auto-mode-alist))

(autoload 'haskell-cabal-mode "haskell-cabal-mode" "Haskell Cabal Mode" t)
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

(require-package 'ghc)
(autoload 'ghc-init "ghc" nil t)
(dolist (mode '("haskell-mode" "haskell-cabal-mode"))
  (eval-after-load mode '(ghc-init)))

;; Haskell doc mode
(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))

;; Indentation
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Key bindings
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,")     'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.")     'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(eval-after-load "haskell-cabal-mode"
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-cabal-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)))

;; GHCi and Haskell interactive mode
(require-package 'ghci-completion)
(dolist (hook '(inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook #'(lambda () (turn-on-ghci-completion))))

;; flycheck-haskell
(add-hook 'haskell-mode-hook 'flycheck-mode)
(require-package 'flycheck-haskell)
(eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; flyspell
;; (add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; Customizations and hooks
(eval-after-load "haskell-mode"
  '(setq haskell-program-name "ghci"
         haskell-stylish-on-save t
         haskell-tags-on-save t))

(eval-after-load "haskell-cabal-mode"
  '(setq haskell-program-name "ghci"))

(add-hook 'haskell-mode-hook #'(lambda ()
                                 (linum-mode 1)
                                 (electric-pair-mode 1)
                                 (yas-minor-mode)))

(dolist (hook '(inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook #'(lambda ()
                     (auto-complete-mode 1)
                     (electric-pair-mode 1)
                     (setq global-hl-line-mode nil))))

(provide 'init-haskell)
;;; init-haskell.el ends here
