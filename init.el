;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs init file for use of Emacs

;;; Code:

;; Turn off mouse interface in startup
(unless window-system
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"))
(add-to-list 'load-path user-emacs-directory)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/custom-theme" user-emacs-directory))

(require 'init-elpa)
(require 'init-shell-env)
(require 'epa-file)
(require 'init-encoding)
(require 'init-themes)
(require 'init-fonts)
(require 'init-key-binds)
(require 'init-clipboard)
(require 'init-window)
(require 'init-linum)
(require 'init-undo-tree)
(require 'init-ido)
;; (require 'init-smex)
(require 'init-helm)
(require 'init-terms)
(require 'init-dired)
(require 'init-direx)
(require 'init-recentf)
(require 'init-tramp)
(require 'init-ibuffer)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-editing-util)
(require 'init-ediff)
(require 'init-auto-complete)
(require 'init-yasnippet)
(require 'init-auctex)
(require 'init-org)
(require 'init-cc-mode)
(require 'init-lisp-mode)
(require 'init-clojure)
(require 'init-maxima)
(require 'init-gnuplot)
(require 'init-haskell)
(require 'init-python)
(require 'init-cmake)
(require 'init-sh-mode)
(require 'init-markdown)
(require 'init-mu4e)

;; Variables configured via the interactive customize interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Start editing server
(unless (file-exists-p
         (concat (getenv "TMPDIR") "emacs"
                 (number-to-string (user-real-uid)) "/server"))
  (server-start))

;; All done!
(message "All done, %s%s" (user-login-name) ".")
;;; init.el ends here
