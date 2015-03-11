;;; init-clojure.el --- Clojure mode
;;; Commentary:
;;; Code:

(require-package 'clojure-mode)

(autoload 'clojure-mode "clojure" "Major mode for Clojure." t)
(setq auto-mode-alist (append '(("\\.clj\\'" . lisp-mode)) auto-mode-alist))

(add-hook 'clojure-mode-hook (lambda () (enable-paredit-mode)))

(provide 'init-clojure)
;;; init-clojure.el ends here
