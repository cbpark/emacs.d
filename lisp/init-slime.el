;;; init-slime.el --- SLIME
;;; Commentary:
;;; Code:

(require-package 'slime)
(require 'slime-autoloads)

(let ((slime-contrib-dir (concat (directory-of-library "slime") "/contrib")))
  (if (file-directory-p slime-contrib-dir)
      ;; Ensure contrib dir is ahead of any slime-{fuzzy,repl} package
      (add-to-list 'load-path slime-contrib-dir)
    (require-package 'slime-fuzzy)
    (require-package 'slime-repl)))

(setq slime-net-coding-system 'utf-8-unix
      slime-protocol-version 'ignore)
(setq slime-lisp-implementations
      '((sbcl ("/opt/local/bin/sbcl" "--noinform")
              :coding-system utf-8-unix)
        (ccl ("/opt/local/bin/ccl64" "-K utf-8"))))
(eval-after-load "slime"
                 '(progn
                   (slime-setup
                    '(slime-repl slime-asdf slime-fancy slime-banner))
                   (add-hook-fn slime-mode-hook
                                ;; (linum-mode 1)
                                (enable-paredit-mode))
                   (add-hook-fn slime-repl-mode-hook
                                (linum-mode -1)
                               (enable-paredit-mode)
                               (push ?\(
                                     (getf autopair-dont-pair :never)))
                   (add-hook-fn slime-repl-mode-hook (setq global-hl-line-mode nil))
                   (define-key slime-repl-mode-map (kbd "C-j")
                     'paredit-newline)))

;; Common Lisp HyperSpec
(setq common-lisp-hyperspec-root
      "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")

;; stop SLIME's REPL from grabbing DEL
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; auto-complete for slime
(require-package 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(provide 'init-slime)
;;; init-slime.el ends here
