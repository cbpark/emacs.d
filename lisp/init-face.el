;;; init-face.el -- Custom faces
;;; Commentary:
;;; Code:

;; zenburn theme
(require-package 'zenburn-theme)
;; (setq zenburn-override-colors-alist '(("zenburn-bg" . "#1f1f1f")))
(setq zenburn-override-colors-alist '(("zenburn-bg" . "#232629")))
(load-theme 'zenburn t)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
;; base16-tomorrow-night theme
;; (require-package 'base16-theme)
;; (setq base16-theme-256-color-source "colors"
;;       base16-highlight-mode-line 'contrast
;;       base16-distinct-fringe-background nil)
;; (load-theme 'base16-default-dark t)
;; (setq default-frame-alist '((cursor-color . "#ab4642")))

;; doom-modeline
(require-package 'doom-modeline)
(require-package 'all-the-icons)
(setq doom-modeline-icon t)
(doom-modeline-mode 1)

;; Transparent background.
(set-frame-parameter (selected-frame) 'alpha '(96 96))
(add-to-list 'default-frame-alist '(alpha 96 96))

(defun on-frame-open (&optional frame)
  "If the FRAME created in the terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'on-frame-open)

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-9"))
(defun set-font-hangul (frame)
  "If the FRAME created in GUI, set the font for hangul."
  (if (display-graphic-p frame)
      (progn
        (set-fontset-font t 'hangul (font-spec :name "NanumGothic"))
        (setq face-font-rescale-alist '(("NanumGothic" . 1.1))))))
(mapc 'set-font-hangul (frame-list))
(add-hook 'after-make-frame-functions 'set-font-hangul)

(provide 'init-face)
;;; init-face.el ends here
