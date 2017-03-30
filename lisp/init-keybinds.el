;;; init-keybinds.el -- Key bindings and shortening of commands
;;; Commentary:
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

;; shortening of commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'srr 'replace-string)
(defalias 'fb  'flyspell-buffer)
(defalias 'dw  'delete-trailing-whitespace)

;; key bindings
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-c o")   'occur)
(global-set-key (kbd "M-o")     'other-window)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x w")   'delete-frame)
(global-set-key (kbd "M-SPC")   'cycle-spacing)

;; scrolling without moving the point
(global-set-key (kbd "M-p")  (lambda () (interactive) (scroll-up   4)))
(global-set-key (kbd "M-n")  (lambda () (interactive) (scroll-down 4)))

;; help command (instead of C-h)
(global-set-key (kbd "<f1>") 'help-command)

;; toggle fullscreen
;; (defun toggle-fullscreen ()
;;   "Toggle full screen."
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (when (not (frame-parameter nil 'fullscreen))
;;                          'fullboth)))
;; (global-set-key (kbd "M-RET") 'toggle-fullscreen)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; join the following line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; unset C-z in Darwin
(when *is-darwin* (global-unset-key (kbd "C-z")))

;; enable upcase-region
(put 'upcase-region 'disabled nil)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
