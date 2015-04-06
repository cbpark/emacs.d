;;; init-misc-packages.el --- packages required to load.
;;; Commentary:
;;; Code:

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

;; magit
(require-package 'magit)

;; rainbow-delimiters - mode for coloring parentheses
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
