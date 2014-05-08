;;; init-maxima.el --- Maxima mode
;;; Commentary:
;;; Code:

(add-to-list 'load-path "/usr/local/share/maxima/5.32.1/emacs")

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)

(setq auto-mode-alist
      (append
       '(("\\.ma[cx]\\'" . maxima-mode)
         ("\\.mc\\'"     . maxima-mode)) auto-mode-alist))

(add-hook 'maxima-mode-hook #'(lambda ()
                                (linum-mode 1)))

;; Imaxima
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(eval-after-load 'imaxima
  '(progn
     (setq imaxima-maxima-program "maxima")
     (setq imaxima-use-maxima-mode-flag t)
     (setq imaxima-pt-size 9)
     (setq imaxima-fnt-size "large")))

(provide 'init-maxima)
;;; init-maxima.el ends here
