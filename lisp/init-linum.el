;;; init-linum.el --- Linum mode
;;; Commentary:
;;; Code:

;; hlinum
(require-package 'hlinum)
(hlinum-activate)

;; separating line numbers from text
(eval-after-load 'linum
  '(progn
     (defvar my-linum-format-string "  %4d  ")

     (defun my-linum-get-format-string ()
       "Get linum format string."
       (let* ((width (length (number-to-string
                              (count-lines (point-min) (point-max)))))
              (format (concat "  %" (number-to-string width) "d  ")))
         (setq my-linum-format-string format)))

     (defun my-linum-format (line-number)
       "Linum format for the LINE-NUMBER."
       (propertize (format my-linum-format-string line-number)
                   'face 'linum))

     (add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
     (setq linum-format 'my-linum-format)))

;; add hook to some major modes
;; (dolist (hook '(makefile-mode-hook fortran-mode-hook sql-mode-hook))
;;   (add-hook hook #'(lambda () (linum-mode 1))))

(provide 'init-linum)
;;; init-linum.el ends here
