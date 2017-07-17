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
(defconst *expand-region-dir* (concat *site-lisp-dir* "expand-region"))
(if (and *expand-region-dir* (file-directory-p *expand-region-dir*))
    (progn
      (add-to-list 'load-path *expand-region-dir*)
      (require 'expand-region))
  (require-package 'expand-region))
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Paredit
(require-package 'paredit)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp))

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
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)

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
  (setq interprogram-cut-function   'paste-to-osx))

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
;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook 'hl-line-mode))
;; (add-hook 'message-mode-hook (lambda () (hl-line-mode -1)))

(provide 'init-editor)
;;; init-editor.el ends here
