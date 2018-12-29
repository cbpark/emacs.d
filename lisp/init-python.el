;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(setq python-indent-offset 4)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (linum-mode -1)
            (setq-local global-hl-line-mode nil)
            (undo-tree-mode -1)))

(require-package 'elpy)
(elpy-enable)
(with-eval-after-load 'elpy
  (defalias 'workon 'pyvenv-workon))

(provide 'init-python)
;;; init-python.el ends here
