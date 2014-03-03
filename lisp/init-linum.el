;;; init-linum.el --- Linum mode
;;; Commentary:
;;; Code:

(global-linum-mode -1)

(require-package 'hlinum)
(after-load 'hlinum
  (hlinum-activate))

;; separating line numbers from text
(defvar my-linum-format-string "  %4d  ")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "  %" (number-to-string width) "d  ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number)
              'face 'linum))

;; add hook to some major modes
;; (add-hook 'makefile-mode-hook (lambda () (linum-mode 1)))
;; (add-hook 'fortran-mode-hook (lambda () (linum-mode 1)))

(provide 'init-linum)
;;; init-linum.el ends here
