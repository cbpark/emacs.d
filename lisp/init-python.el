;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require-package 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

;; Flycheck
(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode)))

;; ipython.el
(require-package 'ipython)
(setq ipython-command "ipython")
(setq py-python-command "ipython")
(setq-default py-python-command-args '("--pylab"))
(add-hook 'py-shell-hook
          (lambda ()
            (setq global-hl-line-mode nil)
            (linum-mode -1)))

;; Jedi: Python auto-completion package
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:ac-setup)

(provide 'init-python)
;;; init-python.el ends here
