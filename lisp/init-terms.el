;;; init-terms.el --- Customizing the term modes
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read the shell environment
(add-hook 'eshell-mode-hook
          #'(lambda () (let ((shellpath (shell-command-to-string
                                         "/bin/zsh -l -c 'printenv PATH'")))
                         (let ((pathlst (split-string shellpath ":")))
                           (setq exec-path pathlst))
                         (setq eshell-path-env shellpath)
                         (setenv "PATH" shellpath))))

;; Turn off hl-line in eshell
(add-hook 'eshell-mode-hook #'(lambda () (setq global-hl-line-mode nil)))

;; key binding
(global-set-key (kbd "C-c e") 'eshell)

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

;; Helm eshell
(when (featurep 'helm)
  ;; helm pcomplete
  (add-hook 'eshell-mode-hook
            #'(lambda () (define-key eshell-mode-map
                           [remap eshell-pcomplete] 'helm-esh-pcomplete)))
  ;; helm eshell history
  (add-hook 'eshell-mode-hook
            #'(lambda () (define-key eshell-mode-map
                           (kbd "M-p") 'helm-eshell-history))))

;; Add color to a shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; shell-switcher
(require-package 'shell-switcher)
(setq shell-switcher-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(global-set-key (kbd "C-c t") 'multi-term-next)
;; create a new multi-term
(global-set-key (kbd "C-c T") 'multi-term)
;; term-line and term-char-mode
(global-set-key (kbd "C-c C-j") 'term-line-mode)
(global-set-key (kbd "C-c C-k") 'term-char-mode)

(eval-after-load "multi-term"
  '(progn
     (setq multi-term-program shell-file-name)
     ;; Use Emacs terminfo, not system terminfo
     (setq system-uses-terminfo nil)

     ;; for zsh
     (setq multi-term-program "/bin/zsh")
     (add-hook 'term-load-hook
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
                     (term-send-raw-string "\en")))))))

(add-hook 'term-load-hook #'(lambda () (setq global-hl-line-mode nil)))

(provide 'init-terms)
;;; init-terms.el ends here
