;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python)
(setq python-indent-offset 4)

;; company-jedi
(require-package 'company-jedi)
(with-eval-after-load 'company
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'company-backends 'company-jedi))))

(provide 'init-python)
;;; init-python.el ends here
