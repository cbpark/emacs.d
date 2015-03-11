;;; init-gnuplot.el --- Gnuplot mode
;;; Commentary:
;;; Code:

(require-package 'gnuplot)

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(setq auto-mode-alist
  (append
    '(("\\.plt$" . gnuplot-mode)
      ("\\.gnu$" . gnuplot-mode)
      ("\\.gpi$" . gnuplot-mode)
      ("\\.gih$" . gnuplot-mode)
      ("\\.gp$"  . gnuplot-mode)) auto-mode-alist))

(provide 'init-gnuplot)
;;; init-gnuplot.el ends here
