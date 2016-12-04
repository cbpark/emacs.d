;;; init-elpa.el --- Use Emacs ELPA
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; exec-path-from-shell
(require-package 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (eval-after-load 'exec-path-from-shell
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
      (exec-path-from-shell-copy-env var))))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require-package 'cl-lib)
(require 'cl-lib)

(provide 'init-elpa)
;;; init-elpa.el ends here
