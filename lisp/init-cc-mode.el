;;; init-cc-mode.el --- CC mode
;;; Commentary:
;;; Code:

;; c-eldoc
(require-package 'c-eldoc)
(defvar c-eldoc-cpp-command "cpp")

(autoload 'c++-mode "cc-mode" "C++ mode" t)
(autoload 'c-mode "cc-mode" "C mode" t)
(autoload 'java-mode "cc-mode" "Java mode" t)

(setq auto-mode-alist
  (append
    '(("\\.C$"    . c++-mode )
      ("\\.cc$"   . c++-mode )
      ("\\.cpp$"  . c++-mode )
      ("\\.cxx$"  . c++-mode )
      ("\\.c$"    . c-mode   )
      ("\\.h$"    . c++-mode )
      ("\\.java$" . java-mode)) auto-mode-alist))

(defun cc-insert-comment ()
  "Insert the comments for documentation."
  (interactive)
  (insert "/*  */")
  (backward-char 3))

(defun cc-insert-include ()
  "Insert the include."
  (interactive)
  (insert "#include "))

(defun cc-insert-define ()
  "Insert the define."
  (interactive)
  (insert "#define "))

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "RET")     'c-context-line-break)
     (define-key c-mode-base-map (kbd "C-c C-a") 'cc-insert-comment)
     (define-key c-mode-base-map (kbd "C-c C-i") 'cc-insert-include)
     (define-key c-mode-base-map (kbd "C-c C-d") 'cc-insert-define)
     (define-key c-mode-base-map (kbd "C-C C-l") 'compile)

     (setq c-default-style "k&r"
           c-basic-offset 4)

     (add-hook 'c-mode-common-hook #'(lambda ()
                                       (c-toggle-hungry-state 1)
                                       (linum-mode 1)
                                       (auto-complete-mode 1)
                                       (c-turn-on-eldoc-mode)
                                       (flycheck-mode)))))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
