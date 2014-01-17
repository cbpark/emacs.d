;;; init-fontss.el -- Fonts
;;; Commentary:
;;; Code:
(unless (eq window-system nil)
  (setq initial-frame-alist '((font . "Inconsolata-g-14")))
  (setq default-frame-alist '((font . "Inconsolata-g-14")))
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

(provide 'init-fonts)
;;; init-fonts.el ends here
