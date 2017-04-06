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
(defconst *default-font* "Source Code Pro")

(defun my-set-default-font (frame)
  "Set default font on FRAME creation."
  (select-frame frame)
  (when (display-graphic-p)
    (when (member *default-font* (font-family-list))
      (set-frame-font (concat *default-font* "-9")))))

(cond (*is-darwin*
       (when (member *default-font* (font-family-list))
         (let ((*default-font-darwin* (concat *default-font* "-12")))
           (setq initial-frame-alist '((font . *default-font-darwin*)))
           (setq default-frame-alist '((font . *default-font-darwin*))))))
      (t
       (add-hook 'after-make-frame-functions 'my-set-default-font)))

(provide 'init-themes)
;;; init-themes.el ends here
