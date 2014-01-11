;;; init-haskell.el --- Haskell mode
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(autoload 'haskell-mode "haskell-mode" "Haskell mode" t)

(require-package 'ghc)
(autoload 'ghc-init "ghc" nil t)
(add-hook-fn haskell-mode-hook (ghc-init))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; Haskell doc mode
(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))

;; Indentation
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; linum-mode
(add-hook-fn haskell-mode-hook (linum-mode 1))

;; Yasnippet
(add-hook-fn haskell-mode-hook (yas-minor-mode))

;; Interactive Block Indentation
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;; Stylish Haskell
(setq haskell-stylish-on-save t)

;; Compilation
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; GHCi and Haskell interactive mode
(require-package 'ghci-completion)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
(add-hook 'haskell-interactive-mode-hook 'turn-on-ghci-completion)
(add-hook 'haskell-interactive-mode-hook (lambda ()
                                           (auto-complete-mode 1)))
(add-hook-fn haskell-interactive-mode-hook (setq global-hl-line-mode nil))
(setq haskell-program-name "/opt/local/bin/ghci")

;; haskell-interactive-mode
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; flymake haskell
(require-package 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

(provide 'init-haskell)
;;; init-haskell.el ends here
