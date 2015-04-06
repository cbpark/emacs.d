;;; init-lisp-mode.el --- Lisp modes
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'lisp-mode "lisp" "Major mode for Lisp." t)
(setq auto-mode-alist
      (append
       '(("\\.lisp\\'" . lisp-mode)
         ("\\.asd\\'"  . lisp-mode)) auto-mode-alist))

;; SLIME
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

(eval-after-load "slime"
  '(progn
     (setq slime-net-coding-system 'utf-8-unix
           slime-protocol-version 'ignore)
     (setq slime-lisp-implementations
           '((sbcl ("sbcl" "--noinform")
                   :coding-system utf-8-unix)
             (ccl ("ccl64" "-K utf-8"))))

     (slime-setup
      '(slime-repl slime-asdf slime-fancy slime-banner))

     ;; Common Lisp HyperSpec
     (when (string-equal system-type "darwin")
       (setq common-lisp-hyperspec-root
             "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"))

     (define-key slime-repl-mode-map (kbd "C-j") 'paredit-newline)))

(dolist (hook '(slime-mode-hook slime-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(add-hook 'slime-repl-mode-hook #'(lambda ()
                                    (linum-mode -1)
                                    (setq global-hl-line-mode nil)))

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'(lambda ()
                     (linum-mode -1)
                     (enable-paredit-mode))))

(add-hook 'lisp-mode-hook #'(lambda () (flyspell-prog-mode)))
(add-hook 'lisp-interaction-mode-hook #'(lambda () (linum-mode -1)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (eldoc-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'scheme-mode "scheme" "Major mode for Scheme." t)
(setq auto-mode-alist
      (append
       '(("\\.scm$" . scheme-mode)
         ("\\.rkt$" . scheme-mode)) auto-mode-alist))

(eval-after-load "scheme"
  '(progn
     (dolist (hook '(scheme-mode-hook inferior-scheme-mode-hook))
       (add-hook hook #'(lambda ()
                          (enable-paredit-mode))))
     (add-hook 'scheme-mode-hook #'(lambda () (flyspell-prog-mode)))))

(provide 'init-lisp-mode)
;;; init-lisp-mode.el ends here
