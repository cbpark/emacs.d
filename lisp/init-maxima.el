;;; init-maxima.el --- Maxima mode
;;; Commentary:
;;; Code:

(defconst *maxima-dir* (concat *site-lisp-dir* "maxima"))

(when (and *maxima-dir* (file-directory-p *maxima-dir*))
  (add-to-list 'load-path *maxima-dir*)
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)

  (setq auto-mode-alist
        (append '(("\\.ma[cx]\\'" . maxima-mode)
                  ("\\.mc\\'"     . maxima-mode)) auto-mode-alist))

  (with-eval-after-load 'maxima
    (define-key maxima-mode-map (kbd "C-c C-r") 'maxima-send-region)
    (define-key maxima-mode-map (kbd "C-c C-b") 'maxima-send-buffer)
    (define-key maxima-mode-map (kbd "C-c C-c") 'maxima-send-line)
    (define-key maxima-mode-map (kbd "C-c C-l") 'maxima-load-file)
    (define-key maxima-mode-map (kbd "C-c C-k") 'maxima-stop))

  (add-hook 'inferior-maxima-mode-hook (lambda () (undo-tree-mode -1)))

  ;; Imaxima
  (require 'cl)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (with-eval-after-load 'imaxima
    (setq imaxima-maxima-program "maxima")
    (setq imaxima-use-maxima-mode-flag t)
    (setq imaxima-pt-size 12)))

(provide 'init-maxima)
;;; init-maxima.el ends here
