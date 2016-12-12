;;; init-editor.el --- Utils for editing
;;; Commentary:
;;; Code:

;; Flyspell
(when *has-aspell* (add-hook 'text-mode-hook 'turn-on-flyspell))

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Paredit
(require-package 'paredit)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp))

;; multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; If indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks (lambda () (when (not indent-tabs-mode)
                                         (untabify (point-min) (point-max))) nil))

;; Recompile elisp when saving
(defun my-byte-compile-current-buffer ()
  "Byte-compile current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'my-byte-compile-current-buffer)

;; Semantic mode
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; Clipboard in darwin
(when *is-darwin*
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-paste-function 'copy-from-osx)
  (setq interprogram-cut-function 'paste-to-osx))

;; Input method
(setq default-input-method "korean-hangul")

(provide 'init-editor)
;;; init-editor.el ends here
