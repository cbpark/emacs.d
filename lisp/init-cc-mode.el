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
         ("\\.hh$"   . c++-mode )
         ("\\.java$" . java-mode)) auto-mode-alist))

(defun cc-insert-comment ()
  "Insert the comments for documentation."
  (interactive)
  (insert "/*  */")
  (backward-char 3))

(defun cc-insert-include ()
  "Insert the #include."
  (interactive)
  (insert "#include "))

(defun cc-insert-define ()
  "Insert the #define."
  (interactive)
  (insert "#define "))

(defun cc-lookup-man ()
  "Look up man page."
  (interactive)
  (manual-entry (current-word)))

(defun cc-insert-std ()
  "Insert std::."
  (interactive)
  (insert "std::"))

(defun cc-insert-cout ()
  "Insert std::cout <<."
  (interactive)
  (cc-insert-std)
  (insert "cout << "))

(defun flycheck-c-setup ()
  "Set clang language standard."
  (setq flycheck-clang-language-standard "c++11"
        flycheck-clang-standard-library "libc++"))

(defun company-clang-args ()
  "Set company-clang-arguments."
  (setq company-clang-arguments '("-std=c++11"
                                  "-stdlib=libc++")))

;; flycheck
(require-package 'flycheck-google-cpplint)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-clang
                                '(warning . c/c++-googlelint))))

;; company
(require-package 'company-c-headers)
(eval-after-load "company"
  '(progn
     (add-to-list 'company-backends 'company-c-headers)))

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "RET")     'c-context-line-break)
     (define-key c-mode-base-map (kbd "C-c C-a") 'cc-insert-comment)
     (define-key c-mode-base-map (kbd "C-c i")   'cc-insert-include)
     (define-key c-mode-base-map (kbd "C-c d")   'cc-insert-define)
     (define-key c-mode-base-map (kbd "C-h d")   'cc-lookup-man)
     (define-key c-mode-base-map (kbd "C-c s")   'cc-insert-std)
     (define-key c-mode-base-map (kbd "C-c c")   'cc-insert-cout)
     (define-key c-mode-base-map (kbd "C-C C-l") 'compile)

     (setq c-default-style "k&r"
           c-basic-offset 4)

     (add-hook 'c-mode-common-hook #'(lambda ()
                                       (c-toggle-hungry-state 1)
                                       (linum-mode 1)
                                       (c-turn-on-eldoc-mode)
                                       (flycheck-mode)
                                       (flycheck-c-setup)
                                       (company-mode 1)
                                       (company-clang-args)
                                       (ggtags-mode 1)
                                       (rainbow-delimiters-mode-enable)))))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
