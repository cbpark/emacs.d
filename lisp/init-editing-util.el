;;; init-editing-util.el --- Utils for editing
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

;;if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max))) nil))

(provide 'init-editing-util)
;;; init-editing-util.el ends here
