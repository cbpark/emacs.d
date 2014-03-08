;;; init-popwin.el --- popwin
;;; Commentary:
;;; Code:

(require-package 'popwin)
(require 'popwin)
(popwin-mode 1)

(push '(dired-mode :position top)                  popwin:special-display-config)
(push "*Shell Command Output*"                     popwin:special-display-config)
(push '(compilation-mode :noselect t)              popwin:special-display-config)
(push "*slime-apropos*"                            popwin:special-display-config)
(push "*slime-macroexpansion*"                     popwin:special-display-config)
(push "*slime-description*"                        popwin:special-display-config)
(push '("*slime-compilation*" :noselect t)         popwin:special-display-config)
(push "*slime-xref*"                               popwin:special-display-config)
(push '(sldb-mode :stick t)                        popwin:special-display-config)
(push 'slime-repl-mode                             popwin:special-display-config)
(push 'slime-connection-list-mode                  popwin:special-display-config)
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
(push '("*helm-M-x*")                              popwin:special-display-config)

(provide 'init-popwin)
;;; init-popwin.el ends here
