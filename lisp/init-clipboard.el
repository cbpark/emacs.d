;;; init-clipboard.el --- Share the clipboard with Emacs
;;; Commentary:
;;; Code:

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-paste-function 'copy-from-osx)
(setq interprogram-cut-function 'paste-to-osx)

(provide 'init-clipboard)
;;; init-clipboard.el ends here
