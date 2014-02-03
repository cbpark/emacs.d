;;; init-scheme.el --- Scheme modes
;;; Commentary:
;;; Code:

;; Scheme mode
(setq auto-mode-alist
      (append
       '(("\\.rkt$" . scheme-mode)
         ("\\.scm$" . scheme-mode)) auto-mode-alist))
(autoload 'scheme-mode "scheme" "Major mode for Scheme." t)
(add-hook 'scheme-mode-hook
          (lambda ()
             ;; (linum-mode 1)
             (enable-paredit-mode)
             (flycheck-mode)))

;; Chicken Scheme with SLIME
;; (add-to-list 'load-path "/opt/local/lib/chicken/6/")
;; (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (slime-mode 1)))
;; (setq slime-csi-path "/opt/local/bin/csi")

;; Geiser
(require-package 'geiser)
(setq geiser-active-implementations '(racket))
(setq geiser-repl-query-on-kill-p nil)

;; ac-geiser
(require-package 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; Quack
;; (require-package 'quack)
;; (defun scheme-mode-quack-hook ()
;;   (require 'quack))
;; (add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)

(provide 'init-scheme)
;;; init-scheme.el ends here
