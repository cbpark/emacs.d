;;; init-terms.el --- Customizing the term modes
;;; Commentary:
;;; Code:

;; Eshell

;; Read the shell environment
(add-hook-fn eshell-mode-hook
             (let ((shellpath (shell-command-to-string
                               "/bin/zsh -l -c 'printenv PATH'")))
               (let ((pathlst (split-string shellpath ":")))
                 (setq exec-path pathlst))
               (setq eshell-path-env shellpath)
               (setenv "PATH" shellpath)))

;; Turn off hl-line in eshell
(add-hook-fn eshell-mode-hook (setq global-hl-line-mode nil))

(defun eshell/clear ()
  "Clear the screen."
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point)))
  nil)

;; Add color to a shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Multi-term
(require-package 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
(setq multi-term-program shell-file-name)
(global-set-key (kbd "C-c m") 'multi-term-next)
;; create a new multi-term
(global-set-key (kbd "C-c M") 'multi-term)
;; term-line and term-char-mode
(global-set-key (kbd "C-c C-j") 'term-line-mode)
(global-set-key (kbd "C-c C-k") 'term-char-mode)
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; for zsh
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "M-p")
              (lambda ()
                "history-beginning-search-backward-end"
                (interactive)
                (term-send-raw-string "\ep")))
            (define-key term-raw-map (kbd "M-n")
              (lambda ()
                "history-beginning-search-forward-end"
                (interactive)
                (term-send-raw-string "\en")))))

;; disable autopair
(add-hook-fn term-mode-hook (autopair-mode -1))
;; disable hl-line
(add-hook-fn term-mode-hook (setq global-hl-line-mode nil))

(provide 'init-terms)
;;; init-terms.el ends here
