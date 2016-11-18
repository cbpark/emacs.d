;;; init-slime.el --- SLIME
;;; Commentary:
;;; Code:

(require-package 'slime)
(require 'slime-autoloads)

(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Directory of the libary for given LIBRARY-NAME."
  (file-name-as-directory (file-name-directory
                           (find-library-name library-name))))

(let ((slime-contrib-dir (concat (directory-of-library "slime") "/contrib")))
  (if (file-directory-p slime-contrib-dir)
      (add-to-list 'load-path slime-contrib-dir)
    (require-package 'slime-fuzzy)
    (require-package 'slime-repl)))

(eval-after-load 'slime
  '(progn
     (setq slime-net-coding-system 'utf-8-unix
           slime-protocol-version 'ignore)
     (setq slime-lisp-implementations '((sbcl ("sbcl" "--noinform")
                                              :coding-system utf-8-unix)
                                        (ccl ("ccl64" "-K utf-8"))))

     (slime-setup
      '(slime-repl slime-asdf slime-fancy slime-banner))

     ;; Common Lisp HyperSpec
     (when *is-darwin*
       (setq common-lisp-hyperspec-root
             "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"))

     (define-key slime-repl-mode-map (kbd "C-j") 'paredit-newline)))

(dolist (hook '(slime-mode-hook slime-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (linum-mode -1)
                                  (setq global-hl-line-mode nil)))

(provide 'init-slime)
;;; init-slime.el ends here
