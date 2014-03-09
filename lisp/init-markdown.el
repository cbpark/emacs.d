;;; init-markdown.el --- Markdown mode
;;; Commentary:
;;; Code:

(require-package 'markdown-mode)
(setq auto-mode-alist
      (append
       '(("\\.markdown$". markdown-mode)
         ("\\.md$"      . markdown-mode)) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(eval-after-load "markdown-mode"
  '(progn
     (setq markdown-command "multimarkdown")
     (add-hook 'markdown-mode-hook 'turn-off-auto-fill)))

(add-hook 'markdown-mode-hook #'(lambda () (linum-mode 1)))

(provide 'init-markdown)
;;; init-markdown.el ends here
