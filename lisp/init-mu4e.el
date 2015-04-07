;;; init-mu4e.el --- Customizing mu4e mail client
;;; Commentary:
;;; Code:

(defvar mu4e-dir "/usr/local/share/emacs/site-lisp/mu4e")
(when (file-directory-p mu4e-dir)
  (add-to-list 'load-path mu4e-dir)
  (require 'mu4e)
  (eval-after-load "mu4e"
    '(progn
       (setq mu4e-maildir "~/Maildir"
             mu4e-attachment-dir  "~/Downloads"
             mu4e-sent-folder "/cbpark_gmail.com/[Google Mail].Sent Mail"
             mu4e-drafts-folder "/cbpark_gmail.com/[Google Mail].Drafts"
             mu4e-trash-folder "/cbpark_gmail.com/[Google Mail].Trash")

       ;; don't save message to Sent Messages, Gmail/IMAP takes care of this.
       (setq mu4e-sent-messages-behavior 'delete)

       (setq mu4e-maildir-shortcuts
             '(("/cbpark_gmail.com/INBOX"                    . ?i)
               ("/cbpark_gmail.com/[Google Mail].Sent Mail"  . ?s)
               ("/cbpark_gmail.com/[Google Mail].Trash"      . ?t)
               ("/cbpark_gmail.com/arxiv.org"                . ?a)
               ("/cbpark_gmail.com/Haskell-cafe"             . ?h)
               ("/cbpark_gmail.com/github.com"               . ?g)))

       ;; allow for updating mail using 'U' in the main view:
       (setq mu4e-get-mail-command "offlineimap")

       ;; update every 15 minuites
       (setq mu4e-update-interval 900)

       ;; off threading
       (setq mu4e-headers-show-threads nil)

       ;; eww
       (defun my-render-html-message ()
         "Use eww for rendering html."
         (let ((dom (libxml-parse-html-region (point-min) (point-max))))
           (erase-buffer)
           (shr-insert-document dom)
           (goto-char (point-min))))

       (setq mu4e-html2text-command 'my-render-html-message)

       ;; enable inline images
       (setq mu4e-view-show-images t)
       ;; use imagemagick, if available
       (when (fboundp 'imagemagick-register-types)
         (imagemagick-register-types))

       ;; use 'fancy' non-ascii characters in various places in mu4e
       (setq mu4e-use-fancy-chars t)

       ;; attaching files with dired
       (require 'gnus-dired)
       ;; make the `gnus-dired-mail-buffers' function also work on
       ;; message-mode derived modes, such as mu4e-compose-mode
       (defun gnus-dired-mail-buffers ()
         "Return a list of active message buffers."
         (let (buffers)
           (save-current-buffer
             (dolist (buffer (buffer-list t))
               (set-buffer buffer)
               (when (and (derived-mode-p 'message-mode)
                          (null message-sent-message-via))
                 (push (buffer-name buffer) buffers))))
           (nreverse buffers)))

       (setq gnus-dired-mail-mode 'mu4e-user-agent)
       (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

       ;; personal info
       (setq user-mail-address "cbpark@gmail.com"
             user-full-name "Chan Beom Park"
             mu4e-compose-signature-auto-include nil)

       ;; MSMTP setting for multi-smtp sending
       (setq message-send-mail-function 'message-send-mail-with-sendmail)
       (setq sendmail-program "msmtp")

       (defun cg-feed-msmtp ()
         (if (message-mail-p)
             (save-excursion
               (let* ((from
                       (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")))
                      (account
                       (cond
                        ((string-match "cbpark@gmail.com"
                                       from)"cbpark@gmail.com")
                        ((string-match "lunacy@kaist.ac.kr"
                                       from) "cbpark@gmail.com")
                        ((string-match "lunacy@muon.kaist.ac.kr"
                                       from) "cbpark@gmail.com")
                        ((string-match "cbpark@muon.kaist.ac.kr"
                                       from) "cbpark@gmail.com")
                        ((string-match "chanbeom.park@csic.es"
                                       from)"cbpark@gmail.com")
                        ((string-match "chanbeom.park@cern.ch"
                                       from)"cbpark@gmail.com"))))
                 (setq message-sendmail-extra-arguments (list "-a" account))))))

       (setq message-sendmail-envelope-from 'header)
       (add-hook 'message-send-mail-hook 'cg-feed-msmtp)

       ;; don't keep message buffers around
       (setq message-kill-buffer-on-exit t))))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
