;;; init-misc-packages.el --- packages required to load.
;;; Commentary:
;;; Code:

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

;; magit
(require-package 'magit)

(when (string-equal system-type "gnu/linux")
  (require-package 'systemd))

;; pkgbuild-mode for ArchLinux
(require-package 'pkgbuild-mode)
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

;; unison mode
(require-package 'unison-mode)
(autoload 'unison-mode "unison-mode" "my unison mode" t)
(setq auto-mode-alist (append '(("\\.prf$" . unison-mode)) auto-mode-alist))
(defvar unison-command "unison -ui text")

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
