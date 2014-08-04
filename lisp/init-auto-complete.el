;;; init-auto-complete.el --- Auto complete
;;; Commentary:
;;; Code:

(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-ignore-case t)
(setq ac-delay 0.2)
(setq ac-use-quick-help nil)
;; (setq ac-quick-help-delay 0.3)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-use-fuzzy t)
(setq tab-always-indent 'complete)

;; Finish completion by TAB
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

;; Select candidates
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; add hook to some major modes
(dolist (hook '(makefile-mode-hook sql-mode-hook))
  (add-hook hook #'(lambda () (auto-complete-mode 1))))

;; ac-ispell
(custom-set-variables
  '(ac-ispell-requires 4))

(require-package 'ac-ispell)
(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))

(dolist (hook '(git-commit-mode-hook text-mode-hook message-mode-hook))
  (add-hook hook 'ac-ispell-ac-setup))

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
