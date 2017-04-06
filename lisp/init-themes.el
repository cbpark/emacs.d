;;; init-themes.el -- Custom theme
;;; Commentary:
;;; Code:

(require-package 'zenburn-theme)
(defvar zenburn-override-colors-alist '(("zenburn-bg" . "#1f1f1f")))
(load-theme 'zenburn t)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; Transparent background.
(when (and *is-darwin* window-system)
  (set-frame-parameter (selected-frame) 'alpha '(96 96))
  (add-to-list 'default-frame-alist '(alpha 96 96)))

;; (defun on-frame-open (&optional frame)
;;   "If the FRAME created in the terminal don't load background color."
;;   (unless (display-graphic-p frame)
;;     (set-face-background 'default "unspecified-bg" frame)))
;; (add-hook 'after-make-frame-functions 'on-frame-open)
;; (add-hook 'window-setup-hook 'on-frame-open)

;; Font
(when (member "Source Code Pro" (font-family-list))
  (if *is-darwin*
      (progn
        (setq initial-frame-alist '((font . "Source Code Pro-12")))
        (setq default-frame-alist '((font . "Source Code Pro-12"))))
    (progn
      (setq initial-frame-alist '((font . "Source Code Pro-9")))
      (setq default-frame-alist '((font . "Source Code Pro-9"))))))

(provide 'init-themes)
;;; init-themes.el ends here
