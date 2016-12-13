;;; init-cc-mode.el --- CC mode
;;; Commentary:
;;; Code:

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default c-basic-offset 4)
(setq c-default-style "k&r")

(defun cc-insert-comment ()
  "Insert the comments for documentation."
  (interactive)
  (insert "/*  */")
  (backward-char 3))

(defun cc-insert-include ()
  "Insert the #include."
  (interactive)
  (insert "#include "))

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

(define-key c-mode-base-map (kbd "RET")     'c-context-line-break)
(define-key c-mode-base-map (kbd "C-c C-a") 'cc-insert-comment)
(define-key c-mode-base-map (kbd "C-c i")   'cc-insert-include)
(define-key c-mode-base-map (kbd "C-h d")   'cc-lookup-man)
(define-key c-mode-base-map (kbd "C-c s")   'cc-insert-std)
(define-key c-mode-base-map (kbd "C-c c")   'cc-insert-cout)
(define-key c-mode-base-map (kbd "C-c C-l") 'compile)

(require-package 'c-eldoc)
(if (executable-find "clang")
    (setq c-eldoc-cpp-command "clang")
  (setq c-eldoc-cpp-command "cpp"))

(add-hook 'c-mode-common-hook (lambda ()
                                (c-toggle-hungry-state 1)
                                (subword-mode 1)
                                (when *has-aspell* (flyspell-prog-mode))
                                (c-turn-on-eldoc-mode)))

(defun flycheck-cpp-setup ()
  "Set clang language standard."
  (setq flycheck-clang-language-standard "c++14")
  (when (executable-find "root-config")
    (setq flycheck-clang-include-path
          (list (substring
                 (shell-command-to-string "root-config --incdir") 0 -1))))
  (when *is-darwin* (setq flycheck-clang-standard-library "libc++")))

(defun company-clang-args ()
  "Set company-clang-arguments."
  (setq company-clang-arguments '("-std=c++14"))
  (when *is-darwin*
    (setq company-clang-arguments
          (append company-clang-arguments '("-stdlib=libc++")))))

(add-hook 'c++-mode-hook (lambda ()
                           (c-set-offset 'innamespace 0)
                           (flycheck-cpp-setup)
                           (company-clang-args)))

(require-package 'company-c-headers)
(with-eval-after-load 'company
  (setq company-c-headers-path-system '("/usr/include" "/usr/local/include"))
  (when *is-darwin*
    (add-to-list 'company-c-headers-path-system
                 (car (file-expand-wildcards "/usr/include/c++/4.*"))))
  (add-to-list 'company-backends 'company-c-headers))

(when (executable-find "clang-format")
  (require 'clang-format)
  (define-key c-mode-base-map (kbd "C-M-\\") 'clang-format-region)
  (define-key c-mode-base-map (kbd "C-c l")  'clang-format-buffer))

;; GNU Global
(when (executable-find "global")
  (require-package 'ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
