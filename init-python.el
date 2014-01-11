;;; init-python.el --- Python mode settings
;;; Commentary:
;;; Code:

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-hook 'python-mode-hook (lambda () (linum-mode 1)))

;; pyflakes
(require-package 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "/opt/local/bin/pyflakes")

;; ipython.el
(setq ipython-command "/opt/local/bin/ipython")
(setq py-python-command "/opt/local/bin/ipython")
(setq-default py-python-command-args '("--pylab"))
(add-hook 'py-shell-hook
          (lambda ()
            (setq global-hl-line-mode nil)
            (linum-mode -1)))

;; Change comint keys
(require 'comint)
(define-key comint-mode-map [(meta p)]
  'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
  'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
  'comint-next-input)
(define-key comint-mode-map [(control meta p)]
  'comint-previous-input)

;; ;; pylookup to search python documents
;; (setq pylookup-dir "~/.emacs.d/site-lisp/pylookup")
;; (add-to-list 'load-path pylookup-dir)
;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))
;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
;; ;; set search option if you want
;; ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
;; ;; default browser
;; (when (string-equal system-type "darwin")
;;     (setq browse-url-default-browser "/usr/bin/open -a Safari"))

;; Jedi: Python auto-completion package
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:ac-setup)

(provide 'init-python)
;;; init-python-mode.el ends here
