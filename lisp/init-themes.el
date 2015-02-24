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
(load-theme 'base16-default t)

;; hl-line foreground color
(set-face-foreground 'highlight nil)

;; set fringe width
(when window-system
  (set-fringe-mode '(0 . 0)))

(provide 'init-themes)
;;; init-themes.el ends here
