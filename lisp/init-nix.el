;;; init-nix.el --- Nix mode
;;; Commentary:
;;; Code:

(require-package 'nix-mode)

(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(setq auto-mode-alist (append
                       '(("\\.nix\\'"    . nix-mode)
                         ("\\.nix.in\\'" . nix-mode)) auto-mode-alist))

(when (featurep 'rainbow-delimiters)
  (add-hook 'nix-mode-hook #'(lambda () (rainbow-delimiters-mode-enable))))

(provide 'init-nix)
;;; init-nix.el ends here
