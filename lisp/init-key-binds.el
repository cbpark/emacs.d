;;; init-key-binds.el -- Key bindings and shortening of commands
;;; Commentary:
;;; Code:

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map (kbd "<return>") 'act)
(define-key query-replace-map (kbd "C-m") 'act)

;; Shortening of commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'srr 'replace-string)
(defalias 'fb  'flyspell-buffer)
(defalias 'dtw 'delete-trailing-whitespace)

;; Key bindings
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-x a r") 'align-regexp)

;; invoke M-x without the Alt key
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Scrolling without moving the point
(global-set-key (kbd "M-p")  #'(lambda () (interactive) (scroll-up   4)))
(global-set-key (kbd "M-n")  #'(lambda () (interactive) (scroll-down 4)))

;; backward delete
(global-set-key (kbd "C-h") 'delete-backward-char)

;; help command (instead of C-h)
(global-set-key (kbd "<f1>") 'help-command)

;; toggle fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "M-RET") 'toggle-fullscreen)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; join the following line
(global-set-key (kbd "M-j") #'(lambda ()
                                (interactive)
                                (join-line -1)))

;; Unset C-z
(global-unset-key (kbd "C-z"))

(provide 'init-key-binds)
;;; init-key-binds.el ends here
