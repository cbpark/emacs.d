;;; init-csv.el --- CSV mode
;;; Commentary:
;;; Code:

(require-package 'csv-mode)
(require-package 'csv-nav)
(setq auto-mode-alist (append '(("\\.[Cc][Ss][Vv]\\'" . csv-mode)) auto-mode-alist))
(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
;;; init-csv.el ends here
