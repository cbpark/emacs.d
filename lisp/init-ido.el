;;; init-ido.el --- Ido
;;; Commentary:
;;; Code:

(require 'ido)
(ido-mode 'file)

(setq ido-case-fold t)
(setq ido-confirm-unique-completion t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history t)
(setq ido-everywhere t)
(setq ido-ignore-buffers '("\\` " "^\\*.*buffer\\*$" "^\\*GNU Emacs\\*$"
                           "\\.log$" "^.*Help\\*$" "^\\*Warnings\\*$"
                           "^\\*Flycheck.*$" "^.*messages\\*$" "^.*Log\\*$"
                           "\\.projectile$" "^.*compilation\\*$"
                           "^\\*Completions\\*$" "^\\*magit-process\\*$"))
(setq ido-ignore-extensions t)
(setq ido-ignore-files (quote (".DS_Store" ".localized" "Thumbs.db"
                               "desktop.ini" "*.aux")))
(setq ido-max-prospects 10)
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)

(global-set-key (kbd "C-x C-f") 'ido-find-file)

(provide 'init-ido)
;;; init-ido.el ends here
