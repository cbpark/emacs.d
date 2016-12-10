;;; custom.el --- Emacs customizations
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t))))
 '(auto-save-interval 20)
 '(auto-save-list-file-prefix "~/.emacs.d/autosave/")
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(blink-cursor-mode nil)
 '(bookmark-default-file "~/.emacs.d/etc/bookmarks")
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer t)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(display-buffer-reuse-frames t)
 '(doc-view-continuous t)
 '(echo-keystrokes 0.1)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 0.1)
 '(electric-pair-inhibit-predicate (quote electric-pair-conservative-inhibit))
 '(electric-pair-mode t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(eshell-cmpl-ignore-case t)
 '(eshell-error-if-no-glob t)
 '(eshell-plain-echo-behavior t)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(hourglass-delay 2)
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(kept-new-versions 10)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (helm-ls-git helm-flycheck haskell-mode company flycheck projectile helm yaml-mode undo-tree swiper-helm pkgbuild-mode paredit multiple-cursors markdown-mode magit latex-preview-pane htmlize hlinum hlint-refactor helm-swoop helm-projectile gnuplot ggtags flycheck-haskell expand-region exec-path-from-shell company-math company-jedi company-ghci company-cabal company-c-headers cmake-mode c-eldoc auctex anzu zenburn-theme)))
 '(read-file-name-completion-ignore-case t)
 '(ring-bell-function (quote ignore))
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/etc/places")
 '(save-place-mode t)
 '(savehist-file "~/.emacs.d/etc/history")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-always-indent (quote complete))
 '(tags-case-fold-search t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator " • ")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(vc-make-backup-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; custom.el ends here
