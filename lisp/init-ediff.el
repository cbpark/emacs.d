;;; init-ediff.el --- Ediff mode
;;; Commentary:
;;; Code:

(eval-after-load 'ediff
  '(progn
     (add-hook 'ediff-quit-hook 'delete-frame)
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     (setq ediff-split-window-function (if (< (frame-width) 150)
                                           'split-window-horizontally
                                         'split-window-vertically))))

(provide 'init-ediff)
;;; init-ediff.el ends here
