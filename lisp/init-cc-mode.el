;;; init-cc-mode.el --- CC mode
;;; Commentary:
;;; Code:

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default c-basic-offset 4)
(setq c-default-style "k&r")

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
(define-key c-mode-base-map (kbd "C-h d")   'cc-lookup-man)
(define-key c-mode-base-map (kbd "C-c s")   'cc-insert-std)
(define-key c-mode-base-map (kbd "C-c c")   'cc-insert-cout)
(define-key c-mode-base-map (kbd "C-c C-l") 'compile)

;; do not override the key binding
(define-key c-mode-base-map (kbd "M-j") nil)

;; use line comments by default
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; (require-package 'c-eldoc)
;; (if (executable-find "clang")
;;     (setq c-eldoc-cpp-command "clang")
;;   (setq c-eldoc-cpp-command "cpp"))

(add-hook 'c-mode-common-hook (lambda ()
                                (c-toggle-hungry-state 1)
                                (subword-mode 1)
                                (when *has-aspell* (flyspell-prog-mode))
                                (c-turn-on-eldoc-mode)))

(defun flycheck-cpp-setup ()
  "Set clang language standard."
  (setq flycheck-clang-language-standard "c++17")
  (setq flycheck-clang-include-path
        (mapcar (lambda (dir)
                  (concat "/usr/local/include/" dir))
                (cddr (directory-files "/usr/local/include"))))
  (when (executable-find "root-config")
    (setq flycheck-clang-include-path
          (append (list
                   (substring
                    (shell-command-to-string "root-config --incdir") 0 -1))
                  flycheck-clang-include-path))))

(defun company-clang-args ()
  "Set company-clang-arguments."
  (setq company-clang-arguments '("-std=c++17")))

(add-hook 'c++-mode-hook (lambda ()
                           (c-set-offset 'innamespace 0)
                           (flycheck-cpp-setup)
                           (company-clang-args)))

(require-package 'company-c-headers)
(with-eval-after-load 'company
  (setq company-c-headers-path-system '("/usr/include" "/usr/local/include"))
  (add-to-list 'company-backends 'company-c-headers))

(when (executable-find "clang-format")
  (require 'clang-format)
  (define-key c-mode-base-map (kbd "C-M-\\") 'clang-format-region)
  (define-key c-mode-base-map (kbd "C-c l")  'clang-format-buffer))

;; lsp
(when (executable-find "ccls")
  (require-package 'ccls)
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (add-hook 'c-mode-common-hook #'lsp))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
