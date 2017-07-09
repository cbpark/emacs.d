;;; init-haskell.el --- Haskell mode
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)
(add-to-list 'completion-ignored-extensions ".hi")

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook (lambda () (subword-mode 1)))
  (dolist (minor-modes '(haskell-doc-mode
                         haskell-decl-scan-mode
                         interactive-haskell-mode))
    (add-hook 'haskell-mode-hook minor-modes))

  ;; turn off hl-line-mode in interactive modes
  (when (fboundp 'global-hl-line-mode)
    (dolist (hooks '(inferior-haskell-mode-hook
                     haskell-interactive-mode-hook))
      (add-hook hooks (lambda () (setq-local global-hl-line-mode nil))))))

;; Indentation
(with-eval-after-load 'haskell-mode
  (defun my-haskell-style ()
    "Set haskell indentation offsets."
    (interactive)
    (setq haskell-indentation-layout-offset     4
          haskell-indentation-starter-offset    4
          haskell-indentation-left-offset       4
          haskell-indentation-where-pre-offset  2
          haskell-indentation-where-post-offset 2))
  (add-hook 'haskell-mode-hook 'my-haskell-style))

;; stylish-haskell
(when (executable-find "stylish-haskell")
  (with-eval-after-load 'haskell-mode
    (setq haskell-stylish-on-save nil)
    (define-key haskell-mode-map (kbd "C-c l") 'haskell-mode-stylish-buffer)))

(with-eval-after-load 'haskell-mode
  (setq haskell-align-imports-pad-after-name t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-suggest-add-package t
        haskell-ask-also-kill-buffers nil)

  (when *helm-on*
    (setq haskell-completing-read-function 'helm--completing-read-default))

  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`")     'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-session-change-target))

(with-eval-after-load 'haskell-cabal-mode
  (define-key haskell-cabal-mode-map (kbd "C-`")     'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c o")   'haskell-session-change-target))

;; Tags
(when (executable-find "hasktags")
  (with-eval-after-load 'haskell-mode
    (setq haskell-tags-on-save t)
    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

;; flycheck-haskell
(require-package 'flycheck-haskell)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; flyspell
(when *has-aspell*
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'flyspell-prog-mode)))

;; company
(require-package 'company-cabal)
(require-package 'company-ghci)
(with-eval-after-load 'company
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backend)
                   (append '((company-capf company-dabbrev-code))
                           company-backend))))
  (dolist (backends '(company-cabal company-ghci))
    (add-to-list 'company-backends backends)))

;; hlint-refactor
(when (executable-find "refactor")
  (require-package 'hlint-refactor)
  (add-hook 'haskell-mode-hook 'hlint-refactor-mode)
  (with-eval-after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-c , b")
      'hlint-refactor-refactor-buffer)
    (define-key haskell-mode-map (kbd "C-c , r")
      'hlint-refactor-refactor-at-point)))

(defun my-haskell-insert-import ()
  "Insert import."
  (interactive)
  (insert "import "))

(defun my-haskell-insert-import-qual ()
  "Insert qualified import."
  (interactive)
  (insert "import qualified "))

(defun my-haskell-insert-comment ()
  "Insert the comments for the documentation."
  (interactive)
  (insert "-- | "))

(defun my-haskell-insert-nested-comment ()
  "Insert the nested comments for documentation."
  (interactive)
  (insert "{-|  -}")
  (backward-char 3))

(defun my-haskell-insert-pragma ()
  "Insert the pragmas."
  (interactive)
  (insert "{-# LANGUAGE  #-}")
  (backward-char 4))

(defun my-haskell-main-function ()
  "Insert main function."
  (interactive)
  (insert "module Main where")
  (newline) (newline)
  (insert "main :: IO ()")
  (newline)
  (insert "main = "))

(defun my-haskell-module-decl ()
  "Insert module declaration."
  (interactive)
  (insert "module  where")
  (backward-char 6))

(defun my-haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (insert "undefined"))

(defun my-haskell-insert-arrow ()
  "Insert ->."
  (interactive)
  (insert " -> "))

(defun my-haskell-insert-bigarrow ()
  "Insert =>."
  (interactive)
  (insert " => "))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c i")   'my-haskell-insert-import)
  (define-key haskell-mode-map (kbd "C-c q")   'my-haskell-insert-import-qual)
  (define-key haskell-mode-map (kbd "C-c C-a") 'my-haskell-insert-comment)
  (define-key haskell-mode-map (kbd "C-c C-d") 'my-haskell-insert-nested-comment)
  (define-key haskell-mode-map (kbd "C-c C-p") 'my-haskell-insert-pragma)
  (define-key haskell-mode-map (kbd "C-c C-m") 'my-haskell-main-function)
  (define-key haskell-mode-map (kbd "C-c m")   'my-haskell-module-decl)
  (define-key haskell-mode-map (kbd "C-c u")   'my-haskell-insert-undefined)
  (define-key haskell-mode-map (kbd "C-c n")   'my-haskell-insert-arrow)
  (define-key haskell-mode-map (kbd "C-c C-n") 'my-haskell-insert-bigarrow))

(provide 'init-haskell)
;;; init-haskell.el ends here
