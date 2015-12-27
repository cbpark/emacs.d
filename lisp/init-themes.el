;;; init-themes.el -- Custom theme
;;; Commentary:
;;; Code:

;; zenburn theme
;; (require-package 'zenburn-theme)
;; (load-theme 'zenburn t)
;; monokai theme
;; (require-package 'monokai-theme)
;; (load-theme 'monokai t)
;; base16 theme
(require-package 'base16-theme)
(load-theme 'base16-default-dark t)

;; Transparent background if inactive
(when (and (string-equal system-type "gnu/linux") window-system)
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95)))

(defun on-frame-open (&optional frame)
  "If the FRAME created in the terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'on-frame-open)
(add-hook 'window-setup-hook 'on-frame-open)

(provide 'init-themes)
;;; init-themes.el ends here
