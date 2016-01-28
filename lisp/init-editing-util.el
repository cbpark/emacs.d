;;; init-editing-util.el --- Utils for editing
;;; Commentary:
;;; Code:

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)

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

;; If indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          #'(lambda () (if (not indent-tabs-mode)
                           (untabify (point-min) (point-max))) nil))

;; Recompile elisp when saving
(defun byte-compile-current-buffer ()
  "Byte-compile current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;; Semantic mode
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(setq default-input-method "korean-hangul")

(provide 'init-editing-util)
;;; init-editing-util.el ends here
