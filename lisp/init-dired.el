;;; init-dired.el --- Customizing Dired mode
;;; Commentary:
;;; Code:

(require 'dired)

;; omit dot files
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

(setq dired-listing-switches "-alh")

;; Open files in external applications
(when (string-equal system-type "darwin")
  ;; Open files by default programs
  (define-key dired-mode-map "o" 'dired-open-mac)
  (defun dired-open-mac ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (if (file-exists-p file-name)
          (shell-command (concat "/usr/bin/open '" file-name "'" nil )))))

  ;; Open current directory in default file manager
  (defun dired-open-current-directory-in-finder ()
    "Open the current directory in Finder"
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "/usr/bin/open .")))
  (define-key dired-mode-map (kbd "s-O")
    'dired-open-current-directory-in-finder))

;; Make dired less verbose
(when (require 'dired-details nil 'noerror)
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

;; Reuse directory buffer
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init-dired)
;;; init-dired.el ends here
