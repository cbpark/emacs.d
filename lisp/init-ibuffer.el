;;; init-ibuffer.el --- Customizing ibuffer-mode
;;; Commentary:
;;; Code:

(require-package 'ibuffer-vc)

(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
