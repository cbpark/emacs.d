;;; init-lisp-mode.el --- Lisp modes
;;; Commentary:
;;; Code:

(autoload 'lisp-mode "lisp" "Major mode for Lisp." t)
(setq auto-mode-alist
      (append '(("\\.lisp\\'" . lisp-mode)
                ("\\.asd\\'"  . lisp-mode)) auto-mode-alist))

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook))
  (add-hook hook (lambda () (enable-paredit-mode))))

(when *has-aspell*
  (dolist (hook '(lisp-mode-hook scheme-mode-hook))
    (add-hook hook 'flyspell-prog-mode)))

(add-hook 'lisp-interaction-mode-hook (lambda () (linum-mode -1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))

(provide 'init-lisp-mode)
;;; init-lisp-mode.el ends here
