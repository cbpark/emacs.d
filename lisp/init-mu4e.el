;;; init-mu4e.el --- Customizing mu4e mail client
;;; Commentary:
;;; Code:

(defconst *mu4e-dir* (concat *site-lisp-dir* "mu4e"))

(when (and *mu4e-dir* (file-directory-p *mu4e-dir*))
  (add-to-list 'load-path *mu4e-dir*)
  (require 'mu4e)

  (setq mu4e-attachment-dir  "~/Downloads"
        mu4e-sent-folder "/cbpark_gmail.com/[Google Mail].Sent Mail"
        mu4e-trash-folder "/cbpark_gmail.com/[Google Mail].Trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this.
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (string= (message-sendmail-envelope-from) "cbpark@gmail.com")
              'delete
            'sent)))

  (setq mu4e-maildir-shortcuts
        '(("/cbpark_gmail.com/INBOX"                   . ?i)
          ("/cbpark_gmail.com/[Google Mail].Sent Mail" . ?s)
          ("/cbpark_gmail.com/[Google Mail].Trash"     . ?t)
          ("/cbpark_gmail.com/arXiv-hep"               . ?a)
          ("/cbpark_gmail.com/GitHub"                  . ?g)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap -o")

  ;; update every 20 minuites
  ;; (setq mu4e-update-interval 1200)

  ;; off threading
  (setq mu4e-headers-show-threads nil)

  ;; Html2text
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (setq shr-color-visible-luminance-min 80)

  ;; enable inline images
  (setq mu4e-view-show-images t
        mu4e-view-image-max-width 800)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))

  ;; use fancy non-ascii characters in various places in mu4e
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
  (setq mu4e-compose-signature-auto-include nil
        user-full-name "Chan Beom Park"
        user-mail-address "cbpark@gmail.com")

  ;; use the address in the message when replying.
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message))
      (if msg
          (setq user-mail-address
                (cond ((mu4e-message-contact-field-matches
                        msg :to "cbpark@kias.re.kr") "cbpark@kias.re.kr")
                      ((mu4e-message-contact-field-matches
                        msg :to "cbpark@ibs.re.kr") "cbpark@ibs.re.kr")
                      (t "cbpark@gmail.com"))))))
  (add-hook 'mu4e-compose-pre-hook 'my-set-from-address)

  ;; MSMTP setting for multi-smtp sending
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")

  (defun choose-msmtp-account ()
    "Choose account label to feed msmtp -a option based on From header."
    (if (message-mail-p)
        (save-excursion
          (let* ((from (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")))
                 (account (cond ((string-match "cbpark@gmail.com" from)
                                 "cbpark@gmail.com")
                                ((string-match "cbpark@kias.re.kr" from)
                                 "cbpark@kias.re.kr")
                                ((string-match "cbpark@ibs.re.kr" from)
                                 "cbpark@ibs.re.kr"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

  ;; don't keep message buffers around.
  (setq message-kill-buffer-on-exit t)

  ;; message buffers left lying around are cleaned up if exited.
  (setq mu4e-hide-index-messages t)

  ;; customize the reply-quote-string
  (setq message-citation-line-format   "On %a, %d %b %Y at %R %Z, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line)

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; apply format=flowed
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

  ;; speeding up indexing
  ;; (setq mu4e-index-cleanup    nil ;; don't do a full cleanup check
  ;;       mu4e-index-lazy-check t)  ;; don't consider up-to-date dirs

  (setq mu4e-view-show-addresses t))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
