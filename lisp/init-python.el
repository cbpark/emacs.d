;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(eval-after-load "python"
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")))

;; jedi
(when (featurep 'company)
  (require-package 'company-jedi)
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; (add-hook 'python-mode-hook #'(lambda () (linum-on)))

(provide 'init-python)
;;; init-python.el ends here
