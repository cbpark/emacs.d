;;; init-misc.el --- Misc
;;; Commentary:
;;; Code:

;; markown mode
(require-package 'markdown-mode)
(setq auto-mode-alist
      (append '(("README\\.md$" . gfm-mode)
                ("\\.markdown$" . markdown-mode)
                ("\\.md$"       . markdown-mode)) auto-mode-alist))
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'gfm-mode "markdown-mode" "GitHub Flavored Markdown mode" t)
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill))

(require-package 'yaml-mode)
(setq auto-mode-alist
      (append '(("\\.yaml$" . yaml-mode)
                ("\\.yml$"  . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
      '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; syntax highlighting for sysmtemd files
(add-to-list 'auto-mode-alist '("\\.service\\'"   . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'"    . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'"    . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'"    . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'"   . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))

;; PKGBUILD files
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . shell-script-mode))

;; display-line-numbers
(setq-default display-line-numbers-width 4
              display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(dolist (hook '(text-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(provide 'init-misc)
;;; init-misc.el ends here
