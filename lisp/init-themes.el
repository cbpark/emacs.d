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

;; Font
(unless (eq window-system nil)
  (if (string-equal system-type "darwin")
      (progn
        (setq initial-frame-alist '((font . "Source Code Pro 12")))
        (setq default-frame-alist '((font . "Source Code Pro 12"))))
    (progn
      (setq initial-frame-alist '((font . "Ubuntu Mono 10")))
      (setq default-frame-alist '((font . "Ubuntu Mono 10")))))
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                    '("NanumGothicCoding" . "iso10646-1"))
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                    '("NanumGothicCoding" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'kana
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'han
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0208
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0213.2004-1
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0213-2
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  ;; half-width katakana
  (set-fontset-font "fontset-default" 'katakana-jisx0201
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (setq face-font-rescale-alist
        '((".*hiragino.*" . 1.1)
          (".*nanum.*" . 1.1))))

(provide 'init-themes)
;;; init-themes.el ends here
