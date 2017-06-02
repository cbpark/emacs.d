;;; init-flycheck.el --- Flycheck
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(setq flycheck-indication-mode nil
      flycheck-highlighting-mode 'lines
      flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list
      flycheck-check-syntax-automatically '(save mode-enabled))
(add-hook 'after-init-hook #'global-flycheck-mode)

(when *helm-on*
  (require-package 'helm-flycheck)
  (define-key flycheck-mode-map (kbd "C-c h !") 'helm-flycheck))

(when (executable-find "shellcheck")
  (setq flycheck-sh-shellcheck-executable "shellcheck")
  (setq flycheck-shellcheck-excluded-warnings '("SC2046" "SC2086")))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
