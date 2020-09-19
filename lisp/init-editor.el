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
(global-set-key (kbd "C-=")   'er/expand-region)

;; browse-kill-ring
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Paredit
(require-package 'paredit)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp))

;; go to matching parenthesis
(global-set-key (kbd "C-%") 'my-match-paren)

(defun my-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t                   (self-insert-command (or arg 1)))))

;; If indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (when (not indent-tabs-mode)
                       (untabify (point-min) (point-max))) nil))

;; anzu
(defconst *anzu-dir* (concat *site-lisp-dir* "anzu"))
(if (and *anzu-dir* (file-directory-p *anzu-dir*))
    (require 'anzu)
  (require-package 'anzu))
(global-anzu-mode t)

(set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
(setq anzu-mode-lighter ""
      anzu-deactivate-region t
      anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")

(global-set-key (kbd "M-%")   'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Recompile elisp when saving
(defun my-byte-compile-current-buffer ()
  "Byte-compile current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'my-byte-compile-current-buffer)

;; Move text up/down.
(defun my-move-text-internal (arg)
  "Move text ARG lines."
  (cond ((and mark-active transient-mark-mode)
         (when (> (point) (mark))
           (exchange-point-and-mark))
         (let ((column (current-column))
               (text (delete-and-extract-region (point) (mark))))
           (forward-line arg)
           (move-to-column column t)
           (set-mark (point))
           (insert text)
           (exchange-point-and-mark)
           (setq deactivate-mark nil)))
        (t (beginning-of-line)
           (when (or (> arg 0) (not (bobp)))
             (forward-line)
             (when (or (< arg 0) (not (eobp)))
               (transpose-lines arg))
             (forward-line -1)))))

(defun my-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (my-move-text-internal arg))

(defun my-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (my-move-text-internal (- arg)))

(global-set-key (kbd "C-M-<up>")   'my-move-text-up)
(global-set-key (kbd "C-M-<down>") 'my-move-text-down)

;; Input method
(setq default-input-method "korean-hangul")

;; hl-line mode
;; (dolist (hook '(prog-mode-hook
;;                 ;; text-mode-hook
;;                 ))
;;   (add-hook hook 'hl-line-mode))
;; (add-hook 'message-mode-hook (lambda () (hl-line-mode -1)))

(provide 'init-editor)
;;; init-editor.el ends here
