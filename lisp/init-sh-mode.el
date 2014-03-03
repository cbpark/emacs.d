;;; init-sh.el -- Customizing shell mode
;;; Commentary:
;;; Code:

(add-hook 'sh-mode-hook (lambda ()
                          (linum-mode 1)
                          (flycheck-mode)))

(provide 'init-sh-mode)
;;; init-sh.el ends here
