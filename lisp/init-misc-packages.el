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

(when (string-equal system-type "gnu/linux")
  (require-package 'systemd))

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
