;;; init-recentf.el -- recentf
;;; Commentary:
;;; Code:

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/etc/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 25)
(setq recentf-exclude
      '("\\.ido.last$" "^autoloads.el$" "^archive-contents$" "^.*\/bookmarks$"
        "^.*\/cookies$" "\\.html$" "\\.cache$" "^.*\/elpa\/.*$"  "\\.jpg$"
        "\\.png$" "^.*\/Maildir\/.*$" "^.*\/\.git\/.*$" "\\.ps$" "\\.pdf$"
        "TAGS" "/tmp/" "/ssh:"))

;; Periodically saving the list of files
(run-at-time nil (* 10 60) 'recentf-save-list)

(recentf-mode 1)

;; Put the visited directories to recentf
(eval-after-load 'recentf
  '(progn
     (defun recentf-track-opened-file ()
       "Insert the name of the dired or file just opened or written into the recent list."
       (let ((buff-name (or buffer-file-name
                            (and (derived-mode-p 'dired-mode)
                                 default-directory))))
         (and buff-name (recentf-add-file buff-name)))
       ;; Must return nil because it is run from `write-file-functions'.
       nil)

     (defun recentf-track-closed-file ()
       "Update the recent list when a file or dired buffer is killed.
That is, remove a non kept file from the recent list."
       (let ((buff-name (or buffer-file-name
                            (and (derived-mode-p 'dired-mode)
                                 default-directory))))
         (and buff-name
              (recentf-remove-if-non-kept buff-name))))

     (add-hook 'dired-after-readin-hook 'recentf-track-opened-file)
     (add-hook 'kill-buffer-hook 'recentd-track-closed-file)))

(when (and (featurep 'ido) (not *helm-on*))
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))

  (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file))

(provide 'init-recentf)
;;; init-recentf.el ends here
