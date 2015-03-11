;;; init-flycheck.el --- Flycheck
;;; Commentary:
;;; Code:

(require-package 'flycheck)

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-highlighting-mode 'lines)))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
