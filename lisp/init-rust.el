;;; init-rust.el --- Rust mode
;;; Commentary:
;;; Code:

(require-package 'rust-mode)
(require-package 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; flycheck
(require-package 'flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; rustfmt
(when (executable-find "rustfmt")
  (with-eval-after-load 'rust-mode
    (setq rust-format-on-save nil)
    (add-hook 'rust-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c l") 'rust-format-buffer)))))

;; racer
(require-package 'racer)
(when (executable-find "racer")
  (with-eval-after-load 'rust-mode
    (add-hook 'rust-mode-hook 'racer-mode)
    (dolist (minor-modes '(eldoc-mode company-mode))
      (add-hook 'racer-mode-hook minor-modes))))

(provide 'init-rust)
;;; init-rust.el ends here
