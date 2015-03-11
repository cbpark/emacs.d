;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(eval-after-load "python"
  '(progn
     (setq python-shell-interpreter "ipython")
     (setq python-shell-interpreter-args "--pylab")))

(provide 'init-python)
;;; init-python.el ends here
