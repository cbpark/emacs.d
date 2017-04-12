;;; init-maxima.el --- Maxima mode
;;; Commentary:
;;; Code:

(defconst *maxima-dir*
  (car (file-expand-wildcards "/usr/*/share/emacs/site-lisp/maxima")))

(when (and *maxima-dir* (file-directory-p *maxima-dir*))
  (add-to-list 'load-path *maxima-dir*)
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)

  (setq auto-mode-alist
        (append '(("\\.ma[cx]\\'" . maxima-mode)
                  ("\\.mc\\'"     . maxima-mode)) auto-mode-alist))

  (defun open-imaxima ()
    "Open `imaxima' after splitting the window."
    (interactive)
    (save-buffer)
    (split-window-sensibly)
    (imaxima)
    (my-swap-two-windows))

  (with-eval-after-load 'maxima
    (define-key maxima-mode-map (kbd "C-c C-r") 'maxima-send-region)
    (define-key maxima-mode-map (kbd "C-c C-b") 'maxima-send-buffer)
    (define-key maxima-mode-map (kbd "C-c C-c") 'maxima-send-line)
    (define-key maxima-mode-map (kbd "C-c C-l") 'maxima-load-file)
    (define-key maxima-mode-map (kbd "C-c C-i") 'open-imaxima)
    (define-key maxima-mode-map (kbd "C-c C-k") 'maxima-stop))

  ;; Imaxima
  (autoload 'imaxima "imaxima" "Image support for Maxima." t)
  (with-eval-after-load 'imaxima
    (setq imaxima-maxima-program "maxima")
    (setq imaxima-use-maxima-mode-flag t)
    (setq imaxima-pt-size 12)
    (if *is-darwin*
        (setq imaxima-fnt-size "large")
      (setq imaxima-fnt-size "LARGE"))))

(provide 'init-maxima)
;;; init-maxima.el ends here
