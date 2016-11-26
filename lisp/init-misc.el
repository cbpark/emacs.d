;;; init-misc.el --- Misc
;;; Commentary:
;;; Code:

(when *is-linux*
  (require-package 'systemd))

;; pkgbuild-mode for ArchLinux
(require-package 'pkgbuild-mode)
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; unison mode
(require-package 'unison-mode)
(autoload 'unison-mode "unison-mode" "my unison mode" t)
(setq auto-mode-alist (append '(("\\.prf$" . unison-mode)) auto-mode-alist))
(defvar unison-command "unison -ui text")

;; yaml mode
(require-package 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(setq auto-mode-alist (append '(("\\.yaml$" . yaml-mode)
                                ("\\.yml$"  . yaml-mode)) auto-mode-alist))

;; markown mode
(setq auto-mode-alist
      (append '(("\\.markdown$" . markdown-mode)
                ("\\.md$"       . markdown-mode)) auto-mode-alist))
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(eval-after-load "markdown-mode"
  '(progn
     (setq markdown-command "multimarkdown")
     (add-hook 'markdown-mode-hook 'turn-off-auto-fill)))

(provide 'init-misc)
;;; init-misc.el ends here
