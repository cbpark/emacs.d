;;; init-face.el -- Custom faces
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

(when (member *default-font* (font-family-list))
  (cond (*is-darwin*
         (set-face-attribute 'default nil :font *default-font* :height 120))
        (t
         (set-face-attribute 'default nil :font *default-font* :height 90))))

(defun my-set-default-font (frame)
  "Set default font on FRAME creation."
  (with-selected-frame frame
    (when (and (member *default-font* (font-family-list)) (display-graphic-p))
      (set-frame-font (concat *default-font* "-9")))))

(when *is-linux* (add-hook 'after-make-frame-functions 'my-set-default-font))

(provide 'init-face)
;;; init-face.el ends here
