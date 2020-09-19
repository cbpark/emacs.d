;;; init.el --- Emacs init file

;;; Commentary:
;;;   Init file for using Emacs

;;; Code:

(setq package-enable-at-startup nil)
(package-initialize)

;; Temporarily reduce garbage collection during startup
(defconst *initial-gc-cons-threshold* gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold *initial-gc-cons-threshold*)))

;; Constants.
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-darwin* (eq system-type 'darwin))
(defconst *is-gui* (not (eq window-system 'nil)))
(defconst *has-aspell* (executable-find "aspell"))

(defconst *site-lisp-dir*
  (cond (*is-linux* "/usr/share/emacs/site-lisp/")
        ((and *is-darwin* (executable-find "port"))
         "/opt/local/share/emacs/site-lisp/")
        (t "/usr/local/emacs/site-lisp/")))
(add-to-list 'load-path *site-lisp-dir*)

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; Disable package-selected-packages.
(defun package--save-selected-packages (&rest opt) nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; to fix a security vulnerability
(when (version< emacs-version "25.3")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))

(require 'init-elpa)
(require 'init-encoding)
(require 'init-window)
(require 'init-face)

;; (require 'init-helm)
(require 'init-ido)
(defconst *helm-on* (featurep 'helm))

(require 'init-lsp)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-company)
(require 'init-undo-tree)
(require 'init-recentf)
(require 'init-editor)
(require 'init-magit)
(require 'init-terms)
(require 'init-dired)

(require-package 'htmlize)
(require 'init-mu4e)

;; (require 'init-slime)

(dolist (init-files '(init-auctex
                      init-cc-mode
                      init-haskell
                      init-rust
                      init-lisp-mode
                      init-maxima
                      init-python
                      init-org))
  (require init-files))

;; (when (executable-find "nix-env") (require 'init-nix))

(require 'init-misc)
(require 'init-keybinds)

;; Start server
(require 'server)
(unless (server-running-p) (server-start))

;; All done.
(message "All done, %s%s" (user-login-name) ".")

;;; init.el ends here
