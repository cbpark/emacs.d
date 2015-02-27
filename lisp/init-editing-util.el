;;; init-editing-util.el --- Utils for editing
;;; Commentary:
;;; Code:

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Paredit
(require-package 'paredit)
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp)))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Save point position between sessions
(when (require 'saveplace nil 'noerror)
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; Highlight parentheses
(require-package 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; If indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max))) nil))

;; Recompile elisp when saving
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(provide 'init-editing-util)
;;; init-editing-util.el ends here
