;;; init-lsp.el --- Language Server Protocol Support for Emacs
;;; Commentary:
;;; Code:

(require-package 'lsp-mode)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable nil
      lsp-modeline-code-actions-segments '(count name icon)
      lsp-enable-snippet t
      lsp-eldoc-render-all t
      lsp-enable-imenu t)

(require-package 'yasnippet)
(require-package 'which-key)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
