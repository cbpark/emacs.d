;;; init-nix.el --- Nix mode
;;; Commentary:
;;; Code:

(require-package 'nix-mode)
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(setq auto-mode-alist (append '(("\\.nix\\'"    . nix-mode)
                                ("\\.nix.in\\'" . nix-mode)) auto-mode-alist))

(require-package 'company-nixos-options)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-nixos-options))

(provide 'init-nix)
;;; init-nix.el ends here
