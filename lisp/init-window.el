;;; init-window.el --- Move between windows easily
;;; Commentary:
;;; Code:

(when (fboundp 'winner-mode)
      (winner-mode 1)
      (global-set-key (kbd "C-x 0") 'winner-undo)
      (global-set-key (kbd "C-x 9") 'winner-redo))

(provide 'init-window)
;;; init-window.el ends here
