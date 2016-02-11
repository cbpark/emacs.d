;;; init-undo-tree.el --- Undo tree mode
;;; Commentary:
;;; Code:

(require-package 'undo-tree)

(global-undo-tree-mode)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-c C-z") 'undo)
(global-set-key (kbd "C-c M-z") 'redo)
(setq undo-limit (* 4 undo-limit))
(setq undo-strong-limit (* 4 undo-strong-limit))
(setq undo-tree-visualizer-diff t)
(add-hook 'doc-view-mode-hook #'(lambda () (undo-tree-mode -1)))

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
