;;; init-rust.el --- Rust mode
;;; Commentary:
;;; Code:

(require-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run-clippy))

;; rustfmt
(when (executable-find "rustfmt")
  (with-eval-after-load 'rust-mode
    (setq rust-format-on-save nil)
    (define-key rust-mode-map (kbd "C-c l") 'rust-format-buffer)))

;; racer
;; (when (executable-find "racer")
;;   (require-package 'racer)
;;   (with-eval-after-load 'rust-mode
;;     (add-hook 'rust-mode-hook 'racer-mode)
;;     (dolist (minor-modes '(eldoc-mode company-mode))
;;       (add-hook 'racer-mode-hook minor-modes))))

;; lsp
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'lsp)
  (when (executable-find "rls")
    (setq lsp-rust-server 'rls)))

;; flycheck
(require-package 'flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; flyspell
(when *has-aspell*
  (with-eval-after-load 'rust-mode
    (add-hook 'rust-mode-hook 'flyspell-prog-mode)))

(provide 'init-rust)
;;; init-rust.el ends here
