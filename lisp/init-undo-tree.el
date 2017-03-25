;;; init-undo-tree.el --- Undo tree mode
;;; Commentary:
;;; Code:

(require-package 'undo-tree)
(run-with-idle-timer 1 nil (lambda () (global-undo-tree-mode t)))

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-c C-z") 'undo)
(global-set-key (kbd "C-c M-z") 'redo)

(setq undo-limit (* 4 undo-limit))
(setq undo-strong-limit (* 4 undo-strong-limit))
(setq undo-tree-visualizer-diff t)
(setq undo-tree-auto-save-history nil)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/etc/undo")))

(add-hook 'doc-view-mode-hook (lambda () (undo-tree-mode -1)))

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
