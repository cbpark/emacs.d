;;; init-magit.el --- magit
;;; Commentary:
;;; Code:

(defconst *magit-dir* (concat *site-lisp-dir* "magit"))
(if (and *magit-dir* (file-directory-p *magit-dir*))
    (progn
      (add-to-list 'load-path *magit-dir*)
      (autoload 'magit-status "magit" nil t))
  (require-package 'magit))

(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g x") 'magit-checkout)
(global-set-key (kbd "C-x g c") 'magit-commit)
(global-set-key (kbd "C-x g p") 'magit-push)
(global-set-key (kbd "C-x g u") 'magit-pull)
(global-set-key (kbd "C-x g l") 'magit-log-all)
(global-set-key (kbd "C-x g e") 'magit-ediff-resolve)
(global-set-key (kbd "C-x g r") 'magit-rebase-interactive)

(provide 'init-magit)
;;; init-magit.el ends here
