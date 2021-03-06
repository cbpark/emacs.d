;;; custom.el --- Emacs customizations
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
 '(auto-save-interval 20)
 '(auto-save-list-file-prefix "~/.emacs.d/autosave/")
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup")))
 '(blink-cursor-mode nil)
 '(bookmark-default-file "~/.emacs.d/etc/bookmarks")
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(display-buffer-reuse-frames t)
 '(doc-view-continuous t)
 '(echo-keystrokes 0.1)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 1.0)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-compare-entry-function 'string-lessp)
 '(eshell-cmpl-ignore-case t)
 '(eshell-error-if-no-glob t)
 '(eshell-plain-echo-behavior t)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(hourglass-delay 2)
 '(ibuffer-expert t)
 '(icomplete-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(message-kill-buffer-on-exit t)
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(read-file-name-completion-ignore-case t)
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/etc/places")
 '(save-place-mode t)
 '(savehist-file "~/.emacs.d/etc/history")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style 'mixed)
 '(tab-always-indent 'complete)
 '(tags-case-fold-search t)
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator " • ")
 '(use-dialog-box nil)
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; custom.el ends here
