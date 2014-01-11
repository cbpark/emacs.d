;;; init-themes.el -- Custom theme
;;; Commentary:
;;; Code:

;; tomorrow theme
(when (require 'color-theme-tomorrow nil 'noerror)
  (load-theme 'tomorrow-night t))

;; hl-line foreground color
(set-face-foreground 'highlight nil)

;; set fringe width
(set-fringe-mode '(0 . 0))

;; transparenent frame
(when window-system
  ;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
  ;; (set-frame-parameter (selected-frame) 'alpha '(98 95))
  ;; (add-to-list 'default-frame-alist '(alpha 98 95))

  ;; togglet transparency
  (defun toggle-transparency ()
    (interactive)
    (if (/= (cadr (frame-parameter nil 'alpha)) 100)
	(set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(90 50))))
  (global-set-key (kbd "C-c t") 'toggle-transparency)

  ;; Set transparency of emacs
  (defun transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter (selected-frame) 'alpha value)))

(provide 'init-themes)
;;; init-themes.el ends here
