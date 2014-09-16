;;; init-mu4e.el --- Customizing mu4e mail client
;;; Commentary:
;;; Code:

(let ((mu4edir "/usr/local/share/emacs/site-lisp/mu4e"))
  (when (file-directory-p mu4edir)
    (add-to-list 'load-path mu4edir)
    (require 'mu4e)
    (eval-after-load 'mu4e
      '(progn
         (setq mu4e-debug t)
         (setq mu4e-mu-binary "/usr/local/bin/mu")
         (setq mu4e-maildir "~/Maildir")
         (setq mu4e-attachment-dir  "~/Downloads")
         (setq mu4e-sent-folder "/cbpark_gmail.com/[Google Mail].Sent Mail")
         (setq mu4e-drafts-folder "/cbpark_gmail.com/[Google Mail].Drafts")
         (setq mu4e-trash-folder "/cbpark_gmail.com/[Google Mail].Trash")

         ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
         (setq mu4e-sent-messages-behavior 'delete)

         (setq mu4e-maildir-shortcuts
               '(("/cbpark_gmail.com/INBOX"                    . ?i)
                 ("/cbpark_gmail.com/[Google Mail].Sent Mail"  . ?s)
                 ("/cbpark_gmail.com/[Google Mail].Trash"      . ?t)))

         ;; allow for updating mail using 'U' in the main view:
         (setq mu4e-get-mail-command "offlineimap")

         ;; update every 15 minuites
         (setq mu4e-update-interval 900)

         ;; off threading
         (setq mu4e-headers-show-threads nil)

         ;; w3m
         ;; (setq mu4e-html2text-command "w3m -dump -cols 80 -T text/html")

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
         (setq user-mail-address "chanbeom.park@cern.ch"
               user-full-name "Chan Beom Park"
               mu4e-compose-signature
               (concat
                "==============================\n"
                "Chan Beom PARK\n"
                "58/1-014, PH-TH, CERN\n"
                "CH-1211 Geneva 23, Switzerland\n"
                "Email: chanbeom.park@cern.ch\n"
                "Tel: +41 22 76 78826\n"
                "=============================="))

         ;; MSMTP setting for multi-smtp sending
         (setq message-send-mail-function 'message-send-mail-with-sendmail)
         (setq sendmail-program "/opt/local/bin/msmtp")
         (setq message-sendmail-extra-arguments
               '("--passwordeval" "'/opt/local/bin/gpg -d ~/.auth/mail.gpg'"))

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
                                         from) "chanbeom.park@cern.ch")
                          ((string-match "lunacy@muon.kaist.ac.kr"
                                         from) "chanbeom.park@cern.ch")
                          ((string-match "cbpark@muon.kaist.ac.kr"
                                         from) "chanbeom.park@cern.ch")
                          ((string-match "chanbeom.park@csic.es"
                                         from)"chanbeom.park@cern.ch")
                          ((string-match "chanbeom.park@cern.ch"
                                         from)"chanbeom.park@cern.ch"))))
                   (setq message-sendmail-extra-arguments (list "-a" account))))))

         (setq message-sendmail-envelope-from 'header)
         (add-hook 'message-send-mail-hook 'cg-feed-msmtp)

         ;; don't keep message buffers around
         (setq message-kill-buffer-on-exit t)))))


(provide 'init-mu4e)
;;; init-mu4e.el ends here
