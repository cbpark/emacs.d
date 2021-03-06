;;; init-terms.el --- Customizing the term modes
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change comint keys
(require 'comint)
(define-key comint-mode-map (kbd "M-p")
  'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "M-n")
  'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "C-M-p")
  'comint-previous-input)
(define-key comint-mode-map (kbd "C-M-n")
  'comint-next-input)

(dolist (mode '(eshell-mode-hook term-mode-hook))
  (add-hook mode (lambda ()
                   (setq line-spacing 0)
                   (linum-mode -1)
                   (setq-local global-hl-line-mode nil)
                   (undo-tree-mode -1))))

;; Add color to a shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "C-c e") 'eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'term)

;; (global-set-key (kbd "C-c t") (lambda ()
;;                                 (interactive)
;;                                 (ansi-term (getenv "SHELL"))))
(defun my-toggle-term ()
  "This opens and closes a terminal in a buffer."
  (interactive)
  (if (get-buffer "*terminal*")
      (kill-buffer "*terminal*")
    (progn (split-window-below)
           (other-window 1)
           (enlarge-window -5)
           ;; (term "/usr/bin/zsh")
           (term (getenv "SHELL"))
           (set-window-dedicated-p (selected-window) t))))
(global-set-key (kbd "C-c t") 'my-toggle-term)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

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

(add-hook 'term-exec-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(defun my-term-paste (&optional string)
  "Send pasted STRING to the process."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer))
                       (if string string (current-kill 0))))
(add-hook 'term-mode-hook (lambda ()
                            (goto-address-mode)
                            (define-key term-raw-map (kbd "C-y") 'my-term-paste)))

(defun my-term-finish ()
  "Kill buffer automatically when finished."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel proc `(lambda (process event)
                                  (when (string= event "finished\n")
                                    (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'my-term-finish)

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(provide 'init-terms)
;;; init-terms.el ends here
