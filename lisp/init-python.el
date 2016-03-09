;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(eval-after-load "python"
  '(progn
     (when (executable-find "ipython")
       (setq python-shell-interpreter "ipython"
             python-shell-interpreter-args "-i"))))

(provide 'init-python)
;;; init-python.el ends here
