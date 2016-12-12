;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(with-eval-after-load 'python
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")))

;; company-jedi
(require-package 'company-jedi)
(with-eval-after-load 'company
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'company-backends 'company-jedi))))

(provide 'init-python)
;;; init-python.el ends here
