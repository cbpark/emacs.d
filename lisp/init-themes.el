;;; init-themes.el -- Custom theme
;;; Commentary:
;;; Code:

;; zenburn theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)
;; monokai theme
;; (require-package 'monokai-theme)
;; (load-theme 'monokai t)
;; base16 theme
;; (require-package 'base16-theme)
;; (load-theme 'base16-default t)

;; Transparent background if inactive
;; (unless (eq window-system nil)
;;   (set-frame-parameter (selected-frame) 'alpha '(100 85))
;;   (add-to-list 'default-frame-alist '(alpha 100 85)))

(provide 'init-themes)
;;; init-themes.el ends here
