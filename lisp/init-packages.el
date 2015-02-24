;;; init-packages.el --- packages required to load.
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

;; yasnippet
(require-package 'yasnippet)

(provide 'init-packages)
;;; init-packages.el ends here
