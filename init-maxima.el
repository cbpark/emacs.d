;;; init-maxima.el --- Maxima mode
;;; Commentary:
;;; Code:

(unless (eq window-system nil)
  (setq imaxima-maxima-program "maxima")
  (add-to-list 'load-path "/usr/local/share/maxima/5.32.1/emacs")
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (setq auto-mode-alist
        (cons '("\\.mac" . maxima-mode) auto-mode-alist))
  (add-hook 'maxima-mode-hook (lambda () (linum-mode 1)))

  ;; Imaxima
  (autoload 'imaxima "imaxima" "Image support for Maxima." t)
  (setq imaxima-fnt-size "Large"))

(provide 'init-maxima)
;;; init-maxima.el ends here
