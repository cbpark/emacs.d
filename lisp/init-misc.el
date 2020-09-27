;;; init-misc.el --- Misc
;;; Commentary:
;;; Code:

;; html and js modes
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(dolist (hook '(html-mode-hook js-mode-hook))
  (add-hook hook (lambda ()
                   (when *has-aspell* (flyspell-prog-mode))
                   (company-mode 1))))

;; gnuplot mode
(require-package 'gnuplot)
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(setq auto-mode-alist
      (append '(("\\.plt$" . gnuplot-mode)
                ("\\.gp$"  . gnuplot-mode)) auto-mode-alist))

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

;; rainbow-delimeter
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; treemacs
(require-package 'treemacs)
(require-package 'treemacs-projectile)
(setq treemacs-follow-mode t
      treemacs-filewatch-mode t)
(global-set-key (kbd "M-0")       'treemacs-select-window)
(global-set-key (kbd "C-x t 1")   'treemacs-delete-other-windows)
(global-set-key (kbd "C-x t t")   'treemacs)
(global-set-key (kbd "C-x t C-t") 'treemacs-find-file)

;; display-line-numbers
(setq-default display-line-numbers-width 4
              display-line-numbers-widen t)

;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
(dolist (hook '(text-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(provide 'init-misc)
;;; init-misc.el ends here
