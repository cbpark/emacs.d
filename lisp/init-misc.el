;;; init-misc.el --- Misc
;;; Commentary:
;;; Code:

;; pkgbuild-mode for ArchLinux
(when *is-linux*
  (require-package 'pkgbuild-mode)
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "ArchLinux PKGBUILD mode." t)
  (setq auto-mode-alist
        (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist)))

;; yaml mode
(require-package 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(setq auto-mode-alist (append '(("\\.yaml$" . yaml-mode)
                                ("\\.yml$"  . yaml-mode)) auto-mode-alist))

;; markown mode
(require-package 'markdown-mode)
(setq auto-mode-alist
      (append '(("README\\.md$" . gfm-mode)
                ("\\.markdown$" . markdown-mode)
                ("\\.md$"       . markdown-mode)) auto-mode-alist))
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'gfm-mode "markdown-mode" "GitHub Flavored Markdown mode" t)
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (setq markdown-command "multimarkdown"))

;; cmake mode
(if (file-exists-p (concat *site-lisp-dir* "cmake-mode.el"))
    (require 'cmake-mode)
  (require-package 'cmake-mode))

;; nix mode
(when (file-directory-p "~/.nix-profile/share/emacs")
  (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")
  (require 'nix-mode))

(provide 'init-misc)
;;; init-misc.el ends here
