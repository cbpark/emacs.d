;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(setq python-indent-offset 4)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1"))

(require-package 'elpy)
(elpy-enable)
(with-eval-after-load 'elpy
  (defalias 'workon 'pyvenv-workon))

(provide 'init-python)
;;; init-python.el ends here
