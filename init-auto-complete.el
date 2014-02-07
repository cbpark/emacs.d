;;; init-auto-complete.el --- Auto complete
;;; Commentary:
;;; Code:

(require-package 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)
(setq ac-auto-start 4)
(setq ac-ignore-case t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
(ac-config-default)

;; Finish completion by TAB
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; ac-ispell
(custom-set-variables
  '(ac-ispell-requires 4))

(require-package 'ac-ispell)
(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(add-hook 'message-mode-hook 'ac-ispell-ac-setup)

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
