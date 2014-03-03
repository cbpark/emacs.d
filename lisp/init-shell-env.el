;;; init-shell-env.el --- setup environment variables from the user's shell.
;;; Commentary:
;;; Code:

(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

(provide 'init-shell-env)
;;; init-shell-env.el ends here
