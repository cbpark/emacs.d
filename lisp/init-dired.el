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
  (lambda () (interactive) (find-alternate-file "..")))

;; to remove uid and gid
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity nil)

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
       (*is-darwin*
        (mapc
         (lambda (file-path)
           (shell-command (format "open \"%s\"" file-path))) file-list))
       (*is-linux*
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
                (format "Directory `%s' does not exist. Create it? "
                        parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(provide 'init-dired)
;;; init-dired.el ends here
