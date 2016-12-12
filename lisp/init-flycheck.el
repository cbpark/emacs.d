;;; init-flycheck.el --- Flycheck
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(with-eval-after-load 'flycheck
  (setq flycheck-highlighting-mode 'lines
        flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list))

(add-hook 'after-init-hook #'global-flycheck-mode)

(when *helm-on*
  (require-package 'helm-flycheck)
  (with-eval-after-load 'flycheck
    (define-key flycheck-mode-map (kbd "C-c h !") 'helm-flycheck)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
