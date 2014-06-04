;;; init-tramp.el --- Tramp mode
;;; Commentary:
;;; Code:

(require 'tramp)
(setq tramp-verbose 10)
(setq tramp-default-method "sshx")

(defun th-rename-tramp-buffer ()
  "Open file as root."
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Open FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(provide 'init-tramp)
;;; init-tramp.el ends here
