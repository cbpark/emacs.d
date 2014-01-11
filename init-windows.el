;;; init-windows.el --- Move between windows easily
;;; Commentary:
;;; Code:

(require-package 'switch-window)
(require-package 'window-number)
(after-load 'window-number
  (window-number-mode 1)
  (window-number-meta-mode 1))

(provide 'init-windows)
;;; init-window.el ends here
