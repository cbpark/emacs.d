;;; init-sage.el --- sage mode
;;; Commentary:
;;; Code:

(if (string-equal system-type "darwin")
    (add-to-list 'load-path "/Applications/sage/local/share/emacs/site-lisp/sage-mode"))

(when (require 'sage "sage" 'noerror)
  (setq sage-command "/Applications/sage/sage")

  (when window-system
    (require 'sage-view "sage-view")
    (add-hook 'sage-startup-after-prompt-hook 'sage-view)
    (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-output)
    (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-plots)))

(provide 'init-sage)
;;; init-sage.el ends here
