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

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
