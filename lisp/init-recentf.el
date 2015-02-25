;;; init-recentf.el -- recentf
;;; Commentary:
;;; Code:

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 20)

(setq recentf-exclude
      '("\\.ido.last$" "^autoloads.el$" "^archive-contents$" "^.*\/bookmarks$"
        "^.*\/cookies$" "\\.html$" "\\.cache$" "^.*\/elpa\/.*$"
        "^.*\/Maildir\/.*$" "^.*\/\.git\/.*$" "\\.ps$" "\\.pdf$"))

(recentf-mode 1)

(when (and (featurep 'ido) (not (featurep 'helm)))
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))

  (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file))

(provide 'init-recentf)
;;; init-recentf.el ends here
