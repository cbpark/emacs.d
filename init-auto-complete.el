;;; init-auto-complete.el --- Auto complete
;;; Commentary:
;;; Code:

(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-ignore-case t)
(setq ac-use-menu-map t)
(ac-config-default)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; trigger auto-complete using TAB
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
