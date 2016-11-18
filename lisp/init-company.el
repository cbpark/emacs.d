;;; init-company.el --- Company mode
;;; Commentary:
;;; Code:

(require-package 'company)
(eval-after-load 'comapny
  '(progn
     (define-key company-active-map (kbd "C-n")   'company-select-next)
     (define-key company-active-map (kbd "C-p")   'company-select-previous)
     (define-key company-active-map (kbd "C-d")   'company-show-doc-buffer)
     (define-key company-active-map (kbd "<tab>") 'company-complete)))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
;;; init-company.el ends here
