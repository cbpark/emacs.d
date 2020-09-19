;;; init-lsp.el --- Language Server Protocol Support for Emacs
;;; Commentary:
;;; Code:

(require-package 'lsp-mode)
(require-package 'lsp-ui)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable t)

(require-package 'yasnippet)
(require-package 'which-key)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (lsp-ui-mode)
  (yas-global-mode))

(with-eval-after-load 'lsp-mode
  (define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-u") 'lsp-ui-imenu))

(provide 'init-lsp)
;;; init-lsp.el ends here
