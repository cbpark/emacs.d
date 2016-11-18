;;; custom.el --- Emacs customizations
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" default)))
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(display-buffer-reuse-frames t)
 '(doc-view-continuous t)
 '(electric-pair-inhibit-predicate (quote electric-pair-conservative-inhibit))
 '(electric-pair-mode t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(eshell-cmpl-ignore-case t)
 '(eshell-error-if-no-glob t)
 '(eshell-glob-show-progress t)
 '(eshell-plain-echo-behavior t)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-highlight-changes-mode t)
 '(hourglass-delay 2)
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (csv-nav csv-mode yaml-mode unison-mode pkgbuild-mode systemd magit company-jedi nix-mode slime hlint-refactor company-ghci company-cabal flycheck-haskell haskell-mode gnuplot cmake-mode company-c-headers c-eldoc latex-preview-pane auctex multi-term multiple-cursors paredit expand-region undo-tree anzu company flycheck ggtags helm-projectile projectile swiper-helm helm-ls-git helm-swoop helm base16-theme)))
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tags-case-fold-search t)
 '(text-mode-hook
   (quote
    (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator " • "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; custom.el ends here
