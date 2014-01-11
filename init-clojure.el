;;; init-clojure.el --- Clojure mode
;;; Commentary:
;;; Code:

(require-package 'clojure-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; (linum-mode 1)
            (enable-paredit-mode)))
(setq clojure-defun-style-default-indent t)

;; cider and ac-nrepl
(require-package 'cider)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (linum-mode -1)
            (enable-paredit-mode)))
(setq nrepl-hide-special-buffers t)

;; ac-nrepl
(require-package 'ac-nrepl)

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
 (add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'cider-repl-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(provide 'init-clojure)
;;; init-clojure.el ends here
