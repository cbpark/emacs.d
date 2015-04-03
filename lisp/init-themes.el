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

;; smart-mode-line
(require-package 'smart-mode-line)
(setq sml/no-confirm-load-theme t
      sml/theme 'respectful
      sml/shorten-directory t
      sml/shorten-modes t)
(sml/setup)

(provide 'init-themes)
;;; init-themes.el ends here
