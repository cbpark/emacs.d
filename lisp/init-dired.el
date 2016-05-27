;;; init-dired.el --- Customizing Dired mode
;;; Commentary:
;;; Code:

(require 'dired)

;; omit dot files
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-listing-switches "-alh")
;; reuse directory buffer
(put 'dired-find-alternate-file 'disabled nil)
;; make dired use the same buffer
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
  #'(lambda () (interactive) (find-alternate-file "..")))

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(defun my-toggle-ls-lisp-verbosity ()
  "Toggle Ls LISP verbosity."
  (interactive)
  (if (memq 'uid ls-lisp-verbosity)
      (progn
        (setq ls-lisp-verbosity (delq 'uid ls-lisp-verbosity))
        (setq ls-lisp-verbosity (delq 'gid ls-lisp-verbosity))
        (revert-buffer)
        (message "uid & gid hidden"))
    (progn
      (add-to-list 'ls-lisp-verbosity 'uid)
      (add-to-list 'ls-lisp-verbosity 'gid)
      (revert-buffer)
      (message "uid & gid visible"))))
(add-hook 'dired-mode-hook
          #'(lambda ()
              (define-key dired-mode-map (kbd "C-p")
                'my-toggle-ls-lisp-verbosity)))

(defun open-in-external-app (&optional file)
  "Open the current FILE or dired marked files in external application."
  (interactive)
  (let* ((file-list
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file))))
         (confirm (or (<= (length file-list) 5)
                      (y-or-n-p "Open more than 5 files? "))))
    (when confirm
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda (file-path)
           (shell-command (format "open \"%s\"" file-path))) file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (file-path)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" file-path))) file-list))))))

(define-key dired-mode-map "o" 'open-in-external-app)

(defun my-create-non-existent-directory ()
  "Create parent directories when creating a new file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p
                (format "Directory `%s' does not exist.  Create it? "
                        parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(provide 'init-dired)
;;; init-dired.el ends here
