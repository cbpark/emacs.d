;;; init.el --- Emacs init file

;;; Commentary:
;;;   Init file for using Emacs

;;; Code:

;; Temporarily reduce garbage collection during startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst *initial-gc-cons-threshold* gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold *initial-gc-cons-threshold*)))

;; Constants.
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-darwin* (eq system-type 'darwin))
(defconst *is-gui* (not (eq window-system 'nil)))

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-elpa)
(require 'epa-file)
(require 'init-encoding)
(require 'init-window)
(require 'init-themes)

(require 'init-helm)
(defconst *helm-on* (featurep 'helm))

(require 'init-projectile)
(require-package 'ggtags)

(require 'init-flycheck)
(require 'init-company)
(require 'init-anzu)
(require 'init-undo-tree)
(require 'init-recentf)
(require 'init-editor)
(require 'init-ediff)
(when *is-darwin*
  (require 'init-clipboard))

(global-linum-mode -1)
(require 'init-linum)

(require 'init-terms)
(require 'init-dired)

(require 'init-mu4e)

(dolist (init-files '(init-auctex
                      init-cc-mode
                      init-cmake
                      init-csv
                      init-gnuplot
                      init-haskell
                      init-html-js
                      init-lisp-mode
                      init-maxima
                      init-nix
                      init-python))
  (require init-files))

(require 'init-misc)
(require 'init-keybinds)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; All done.
(message "All done, %s%s" (user-login-name) ".")

;;; init.el ends here
