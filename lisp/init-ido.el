;;; init-ido.el --- Ido
;;; Commentary:
;;; Code:

(require 'ido)
(ido-mode 1)

(setq ido-confirm-unique-completion t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-last-directory-history t
      ido-everywhere t
      ido-use-virtual-buffers t)

(setq ido-ignore-buffers '("\\` " "^\\*.*buffer\\*$" "^\\*GNU Emacs\\*$"
                           "\\.log$" "^.*Help\\*$" "^\\*Warnings\\*$"
                           "^\\*Flycheck.*$" "^.*messages\\*$" "^.*Log\\*$"
                           "\\.projectile$" "^.*compilation\\*$"
                           "^\\*Completions\\*$" "^\\*magit-process\\*$"))
(setq ido-ignore-files (quote (".DS_Store" ".localized" "Thumbs.db"
                               "desktop.ini" "*.aux")))

;; icomplete
(icomplete-mode t)

(global-set-key (kbd "C-x C-m") (lambda ()
                                  (interactive)
                                  (call-interactively
                                   (intern
                                    (ido-completing-read
                                     "M-x "
                                     (all-completions "" obarray 'commandp))))))

(provide 'init-ido)
;;; init-ido.el ends here
