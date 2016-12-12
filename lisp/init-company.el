;;; init-company.el --- Company mode
;;; Commentary:
;;; Code:

(require-package 'company)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 3
      company-selection-wrap-around t
      company-show-numbers t
      company-require-match 'never)

(with-eval-after-load 'comapny
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
;;; init-company.el ends here
