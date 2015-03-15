;;; init-yasnippet.el --- Yasnippet
;;; Commentary:
;;; Code:

(require-package 'yasnippet)

(eval-after-load "yasnippet"
  '(progn
    (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets")))
    (yas-reload-all)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
