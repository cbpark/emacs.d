;;; init-cc-mode.el --- CC mode
;;; Commentary:
;;; Code:

;; c-eldoc
(require-package 'c-eldoc)
(defvar c-eldoc-cpp-command "cpp")

(autoload 'c++-mode "cc-mode" "C++ mode" t)
(autoload 'c-mode "cc-mode" "C mode" t)
(autoload 'java-mode "cc-mode" "Java mode" t)
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
     (setq c-default-style "k&r"
           c-basic-offset 4)

     (dolist (hook '(c-mode-hook c++-mode-hook))
       (add-hook hook #'(lambda ()
                          (electric-pair-mode 1)
                          (linum-mode 1)
                          (auto-complete-mode 1)
                          (c-turn-on-eldoc-mode)
                          (flycheck-mode))))))

(setq auto-mode-alist
  (append
    '(("\\.C$"    . c++-mode )
      ("\\.cc$"   . c++-mode )
      ("\\.cpp$"  . c++-mode )
      ("\\.cxx$"  . c++-mode )
      ("\\.c$"    . c-mode   )
      ("\\.h$"    . c++-mode )
      ("\\.java$" . java-mode)) auto-mode-alist))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
