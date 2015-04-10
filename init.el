;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs init file for use of Emacs

;;; Code:

;; Turn off mouse interface in startup
(unless window-system
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; Variables configured via the interactive customize interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/custom-theme" user-emacs-directory))

(dolist (init-files '(init-elpa
                      epa-file
                      init-encoding
                      init-themes
                      init-fonts
                      init-key-binds
                      init-editing-util
                      init-anzu
                      init-clipboard
                      init-window
                      init-linum
                      init-undo-tree
                      init-helm
                      init-terms
                      init-dired
                      init-recentf
                      init-tramp
                      init-tags
                      init-projectile
                      init-flycheck
                      init-company
                      init-ediff
                      init-misc-packages
                      init-auctex
                      init-html-js
                      init-org
                      init-cc-mode
                      init-lisp-mode
                      init-maxima
                      init-gnuplot
                      init-haskell
                      init-python
                      init-cmake
                      init-markdown
                      init-mu4e
                      init-nix))
  (require init-files))

;; Start editing server
(unless (file-exists-p
         (concat (getenv "TMPDIR") "emacs"
                 (number-to-string (user-real-uid)) "/server"))
  (server-start))

;; All done!
(message "All done, %s%s" (user-login-name) ".")
;;; init.el ends here
