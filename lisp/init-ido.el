;;; init-ido.el --- Ido
;;; Commentary:
;;; Code:

(require 'ido)
(ido-mode 1)
;; (ido-everywhere 1)

;; (require-package 'flx-ido)
;; (flx-ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; (require-package 'ido-vertical-mode)
;; (ido-vertical-mode 1)

(eval-after-load 'ido
  '(progn
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

     ;; icomplete
     (icomplete-mode t)))

(provide 'init-ido)
;;; init-ido.el ends here
