;;; init-c-mode.el --- C mode
;;; Commentary:
;;; Code:

(autoload 'c++-mode "cc-mode" "C++ mode" t)
(autoload 'c-mode "cc-mode" "C mode" t)
(autoload 'java-mode "cc-mode" "Java mode" t)
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
     (setq c-default-style "k&r"
           c-basic-offset 4)
     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
     ;; (add-hook 'c-mode-hook (lambda () (linum-mode 1)))
     ;; (add-hook 'c++-mode-hook (lambda () (linum-mode 1)))
     ))

(setq auto-mode-alist
  (append
    '(("\\.C$"    . c++-mode )
      ("\\.cc$"   . c++-mode )
      ("\\.cpp$"  . c++-mode )
      ("\\.cxx$"  . c++-mode )
      ("\\.c$"    . c-mode   )
      ("\\.h$"    . c++-mode )
      ("\\.java$" . java-mode)) auto-mode-alist))

(provide 'init-c-mode)
;;; init-c-mode.el ends here
