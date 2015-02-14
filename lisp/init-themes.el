;;; init-themes.el -- Custom theme
;;; Commentary:
;;; Code:

;; zenburn theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; hl-line foreground color
(set-face-foreground 'highlight nil)

;; set fringe width
(when window-system
  (set-fringe-mode '(0 . 0)))

(provide 'init-themes)
;;; init-themes.el ends here
