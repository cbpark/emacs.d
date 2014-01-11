;;; init-ruby.el --- Ruby mode
;;; Commentary:
;;; Code:

(require-package 'ruby-mode)

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t)
(add-hook 'ruby-mode-hook
          (lambda ()
            ;; (linum-mode 1)
            (local-set-key "\r" 'newline-and-indent)))

;; inf-ruby
(require-package 'inf-ruby)
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

;; flymake-ruby
(require-package 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(provide 'init-ruby)
;;; init-ruby.el ends here
