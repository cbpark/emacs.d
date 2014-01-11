;;; init-gunus.el --- GNUS
;;; Commentary:
;;; Code:

;; Accessing Gmail via IMAP
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nnimap "cbpark@gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                ;; (nnimap-authinfo-file "~/.authinfo.gpg")
                (nnir-search-engine imap))
        (nnimap "malice.doomed@gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap))))

;; Gwene
;; (add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
;; (setq gnus-select-method '(nntp "news.gwene.org"))

;; Store sent mail in the server
(setq gnus-posting-styles
      '((".*"
         ("Bcc" "cbpark@gmail.com")
         (address "cbpark@gmail.com"))))

;; nnrss will read the feeds from local files in nnrss-directory
(setq nnrss-use-local t)

;; Gnus window layout
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;;               (vertical 0.4
;;                         (group 1.0))
;;               (vertical 1.0
;;                         (summary 0.3 point)
;;                         (article 1.0)))))
;; (gnus-add-configuration
;;  '(summary
;;    (horizontal 1.0
;;                (vertical 0.4
;;                          (group 1.0))
;;                (vertical 1.0
;;                          (summary 1.0 point)))))
(gnus-add-configuration
 '(article (vertical 1.0 (summary .35 point) (article 1.0))))

;; Mime types with Guns
;; (setq mm-discouraged-alternatives
;;       '("text/html" "text/richtext")
;;       mm-automatic-display
;;       (remove "text/html" mm-automatic-display))

;; Set the default value of mm-discouraged-alternatives
(eval-after-load "gnus-sum"
  '(add-to-list 'gnus-newsgroup-variables
                '(mm-discouraged-alternatives
                  . '("text/html" "image/.*"))))

;; Display 'text/html' parts in nnrss groups
(add-to-list 'gnus-parameters
             '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;; inline image
(setq mm-attachment-override-types '("image/.*"))

;; -------------------------------------------------------------------
;; Reading HTML mails in GNUS
;; -------------------------------------------------------------------
;; (setq mm-text-html-renderer 'w3m)
;; (setq mm-inline-text-html-with-images t)
;; (setq mm-inline-text-html-with-w3m-keymap nil)

;; -------------------------------------------------------------------
;; Topic mode
;; -------------------------------------------------------------------
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; -------------------------------------------------------------------
;; Automatic linebreaking
;; -------------------------------------------------------------------
(defun my-message-mode-setup ()
  (setq fill-column 80)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

;; -------------------------------------------------------------------
;; Atom 1.0 to RSS 2.0
;; -------------------------------------------------------------------
(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
			 "xsltproc"
			 t t nil
			 (expand-file-name "~/.emacs.d/etc/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)

;; -------------------------------------------------------------------
;; Gnus daemon
;; -------------------------------------------------------------------
;; check Gnus for new mail every 10 minuites
(gnus-demon-add-handler 'gnus-demon-scan-news 15 t)
(gnus-demon-init)
;; to avoid crash
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (120 (message "Gnus timed out."))
    ad-do-it))

;; -------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------
;; ignore uninterested newsgroups
(setq gnus-ignored-newsgroups "")
;; all outgoing message will be put in this group
(setq gnus-outgoint-message-group "[Google Mail]/Sent Mail")
;; smileys
(setq gnus-treat-display-smileys nil)
;; require confirmation before exiting Gnus
(setq gnus-interactive-exit t)

;; -------------------------------------------------------------------
;; Gnus/MSMTP setting for multi-smtp sending
;; -------------------------------------------------------------------
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/opt/local/bin/msmtp")
(setq message-sendmail-extra-arguments '("--passwordeval" "'gpg -d ~/.auth/mail.cern.ch.gpg'"))

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
                                from) "chanbeom.park@csic.es")
                 ((string-match "lunacy@muon.kaist.ac.kr"
                                from) "chanbeom.park@csic.es")
                 ((string-match "cbpark@muon.kaist.ac.kr"
                                from) "chanbeom.park@csic.es")
                 ((string-match "chanbeom.park@csic.es"
                                from)"chanbeom.park@csic.es")
                 ((string-match "chanbeom.park@cern.ch"
                                from)"chanbeom.park@cern.ch"))))
          (setq message-sendmail-extra-arguments (list "-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)

;; Threaded summary view
;; (setq-default
;;  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
;;  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
;;  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
;;  gnus-thread-sort-functions '(gnus-thread-sort-by-date))
;; (setq gnus-sum-thread-tree-indent "  ")
;; (setq gnus-sum-thread-tree-root "") ;; "● ")
;; (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
;; (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
;; (setq gnus-sum-thread-tree-vertical        "│")
;; (setq gnus-sum-thread-tree-leaf-with-other "├▶ ")
;; (setq gnus-sum-thread-tree-single-leaf     "└▶ ")

;; Kill the message buffer after sending
(setq message-kill-buffer-on-exit t)

;; Gravatar
(require 'gnus-gravatar)
(setq gnus-treat-from-gravatar 'head)
(setq gnus-treat-mail-gravatar 'head)
(setq gnus-gravatar-size 43)
(setq gnus-gravatar-properties '(:ascent 50 :relief 1))

(provide 'init-gnus)
;;; init-gnus.el ends here
