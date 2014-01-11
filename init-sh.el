;;; init-sh.el
;;; Commentary:
;;; Code:

;; (add-hook 'sh-mode-hook (lambda ()
;;                           (linum-mode 1)))

(require-package 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(provide 'init-sh)
;;; init-sh.el ends here
