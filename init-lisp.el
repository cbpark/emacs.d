;;; init-lisp.el --- Lisp mdoes
;;; Commentary:
;;; Code:

(require-package 'hl-sexp)

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting
;; before each command
(after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;; Lisp mode
(setq auto-mode-alist
      (append
       '(("\\.lisp$" . lisp-mode)) auto-mode-alist))
(autoload 'lisp-mode "lisp" "Major mode for Lisp." t)

(add-hook 'lisp-mode-hook
          (lambda ()
            (linum-mode 1)
            (enable-paredit-mode)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (linum-mode -1)))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (linum-mode 1)
            (enable-paredit-mode)))

(provide 'init-lisp)
;;; init-lisp.el ends here
