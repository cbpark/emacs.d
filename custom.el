;;; custom.el --- Emacs custom settings
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(auto-revert-verbose nil)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-kill-emacs nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(display-buffer-reuse-frames t)
 '(display-hourglass t)
 '(doc-view-continuous t)
 '(electric-pair-mode t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(eshell-cmpl-ignore-case t)
 '(eshell-error-if-no-glob t)
 '(eshell-glob-case-insensitive t)
 '(eshell-glob-show-progress nil)
 '(eshell-hist-ignoredups t)
 '(eshell-plain-echo-behavior t)
 '(file-name-shadow-mode t)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(fortran-comment-region "*")
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-font-lock-mode t)
 '(global-highlight-parentheses-mode t)
 '(gnus-directory "~/Documents/Gnus/")
 '(hourglass-delay 2)
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(lisp-body-indent 2)
 '(make-backup-files nil)
 '(mark-even-if-inactive t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier (quote meta))
 '(ns-antialias-text t)
 '(ns-command-modifier (quote super))
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(size-indication-mode t)
 '(split-width-threshold nil)
 '(sql-product (quote sqlite))
 '(tab-width 4)
 '(tags-case-fold-search t)
 '(text-mode-hook
   (quote
    (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-make-backup-files nil)
 '(version-control nil)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; custom.el ends here
