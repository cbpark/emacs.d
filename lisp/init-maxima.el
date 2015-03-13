;;; init-maxima.el --- Maxima mode
;;; Commentary:
;;; Code:

(add-to-list 'load-path
             (car (file-expand-wildcards "/usr/local/share/maxima/*/emacs")))

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)

(setq auto-mode-alist
      (append
       '(("\\.ma[cx]\\'" . maxima-mode)
         ("\\.mc\\'"     . maxima-mode)) auto-mode-alist))

;; Imaxima
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(eval-after-load 'imaxima
  '(progn
     (setq imaxima-maxima-program "maxima")
     (setq imaxima-use-maxima-mode-flag t)
     (setq imaxima-pt-size 9)
     (setq imaxima-fnt-size "large")))

(dolist (hook '(maxima-mode-hook inferior-maxima-mode-hook))
  (add-hook hook #'(lambda ()
                     (rainbow-delimiters-mode-enable)
                     (my-paredit-nonlisp))))

(provide 'init-maxima)
;;; init-maxima.el ends here
