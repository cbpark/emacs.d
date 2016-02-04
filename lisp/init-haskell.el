;;; init-haskell.el --- Haskell mode
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)

(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(setq auto-mode-alist (append
                       '(("\\.hs\\'"    . haskell-mode)
                         ("\\.hsc\\'"   . haskell-mode)
                         ("\\.cpphs\\'" . haskell-mode)) auto-mode-alist))

(autoload 'haskell-cabal-mode "haskell-cabal-mode" "Haskell Cabal Mode" t)
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; Indentation
(defun my-haskell-style ()
  "Set haskell indentation offsets."
  (interactive)
  (setq haskell-indentation-layout-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2))
(add-hook 'haskell-mode-hook 'my-haskell-style)

;; Module templates
;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; stylish-haskell
(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-stylish-on-save nil)
     (define-key haskell-mode-map (kbd "C-c l") 'haskell-mode-stylish-buffer)))

;; Haksell interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-align-imports-pad-after-name t
           haskell-process-auto-import-loaded-modules nil
           haskell-process-log t
           haskell-process-suggest-add-package t
           haskell-process-suggest-remove-import-lines nil
           haskell-process-use-presentation-mode t
           haskell-cabal-list-comma-position 'before)
     (if (executable-find "stack")
         (setq haskell-process-type 'stack-ghci)
       (setq haskell-process-type 'cabal-repl))

     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-`")     'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-session-change-target)))

(eval-after-load "haskell-cabal-mode"
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-`")     'haskell-interactive-bring)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)
     (define-key haskell-cabal-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c o")   'haskell-session-change-target)))

;; Tags
(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-tags-on-save t)
     (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

;; Haskell doc mode
(eval-after-load "haskell-mode"
  '(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
     (add-hook hook 'turn-on-haskell-doc-mode)))

;; flycheck-haskell
(require-package 'flycheck-haskell)
(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (add-hook 'haskell-mode-hook 'flycheck-mode)))

;; flyspell
(eval-after-load "haskell-mode"
  '(add-hook 'haskell-mode-hook 'flyspell-prog-mode))

;; company
(require-package 'company-cabal)
(require-package 'company-ghci)
(eval-after-load "company"
  '(progn
     (dolist (backend '(company-cabal company-ghci))
       (add-to-list 'company-backends backend))))

(eval-after-load "haskell-mode"
  '(when (fboundp 'global-hl-line-mode)
     (dolist (hook '(inferior-haskell-mode-hook haskell-interactive-mode-hook))
       (add-hook hook #'(lambda () (setq global-hl-line-mode nil))))))

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

;; Key bindings
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c i")   'my-haskell-insert-import)
     (define-key haskell-mode-map (kbd "C-c q")   'my-haskell-insert-import-qual)
     (define-key haskell-mode-map (kbd "C-c C-a") 'my-haskell-insert-comment)
     (define-key haskell-mode-map (kbd "C-c C-d") 'my-haskell-insert-nested-comment)
     (define-key haskell-mode-map (kbd "C-c C-p") 'my-haskell-insert-pragma)
     (define-key haskell-mode-map (kbd "C-c C-m") 'my-haskell-main-function)
     (define-key haskell-mode-map (kbd "C-c m")   'my-haskell-module-decl)
     (define-key haskell-mode-map (kbd "C-c u")   'my-haskell-insert-undefined)
     (define-key haskell-mode-map (kbd "C-c n")   'my-haskell-insert-arrow)
     (define-key haskell-mode-map (kbd "C-c C-n") 'my-haskell-insert-bigarrow)))

(provide 'init-haskell)
;;; init-haskell.el ends here
