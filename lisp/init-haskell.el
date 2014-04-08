;;; init-haskell.el --- Haskell mode
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)

(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(setq auto-mode-alist
      (append
       '(("\\.hs\\'"    . haskell-mode)
         ("\\.hsc\\'"   . haskell-mode)
         ("\\.cpphs\\'" . haskell-mode)) auto-mode-alist))

(autoload 'haskell-cabal-mode "haskell-cabal-mode" "Haskell Cabal Mode" t)
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; Haskell doc mode
(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))

;; Indentation
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defun haskell-insert-comment ()
  "Insert the comments for the documentation."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-nested-comment ()
  "Insert the nested comments for documentation."
  (interactive)
  (insert "{-|  -}")
  (backward-char 3))

(defun haskell-insert-pragma ()
  "Insert the pragmas."
  (interactive)
  (insert "{-# LANGUAGE  #-}")
  (backward-char 4))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (insert "undefined"))

;; Key bindings
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-g") 'haskell-hoogle)
     (define-key haskell-mode-map (kbd "C-c l")   'haskell-mode-stylish-buffer)
     (define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-comment)
     (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-insert-nested-comment)
     (define-key haskell-mode-map (kbd "C-c C-p") 'haskell-insert-pragma)
     (define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(eval-after-load "haskell-cabal-mode"
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-cabal-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)))

;; flycheck-haskell
(require-package 'flycheck-haskell)
(eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'haskell-mode-hook 'flycheck-mode)

;; flyspell
;; (add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; auto-complete
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-mode))

(dolist (hook '(inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook #'(lambda ()
                     (auto-complete-mode 1)
                     (setq global-hl-line-mode nil))))

;; Customizations and hooks
(eval-after-load "haskell-mode"
  '(setq haskell-program-name "ghci"
         haskell-stylish-on-save nil
         haskell-tags-on-save t))

(eval-after-load "haskell-cabal-mode"
  '(setq haskell-program-name "ghci"))

(add-hook 'haskell-mode-hook #'(lambda () (linum-mode 1)))

(provide 'init-haskell)
;;; init-haskell.el ends here
