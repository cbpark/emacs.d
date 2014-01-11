;;; init-direx.el --- direx
;;; Commentary:
;;; Code:

(require-package 'direx)

(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

(require-package 'popwin)
(require 'popwin)
(popwin-mode 1)
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(provide 'init-direx)
;;; init-direx.el ends here
