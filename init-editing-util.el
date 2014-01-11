;;; init-editing-util.el ---
;;; Commentary:
;;; Code:

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Expand region
(when (require 'expand-region nil 'noerror)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Save point position between sessions
(when (require 'saveplace nil 'noerror)
      (setq-default save-place t)
      (setq save-place-file (expand-file-name
                             ".places" user-emacs-directory)))

;; Highlight parentheses
(require-package 'highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Auto indent
(require-package 'auto-indent-mode)
(setq auto-indent-on-visit-file t)

;; RainbowDelimiters - mode for coloring parentheses
(require-package 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Pretty lambda
(require-package 'pretty-lambdada)
(pretty-lambda-for-modes)

(provide 'init-editing-util)
;;; init-editing-util.el ends here
