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

;; Paredit
(require-package 'paredit)
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)))

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
           '((sbcl ("/opt/local/bin/sbcl" "--noinform")
                   :coding-system utf-8-unix)
             (ccl ("/opt/local/bin/ccl64" "-K utf-8"))))

     (slime-setup
      '(slime-repl slime-asdf slime-fancy slime-banner))

     ;; Common Lisp HyperSpec
     (when (string-equal system-type "darwin")
       (setq common-lisp-hyperspec-root
             "file:///opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"))

     (define-key slime-repl-mode-map (kbd "C-j") 'paredit-newline)))

(dolist (hook '(slime-mode-hook slime-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;; Auto-complete for SLIME
(require-package 'ac-slime)
(dolist (hook '(slime-mode-hook slime-repl-mode-hook))
  (add-hook hook 'set-up-slime-ac))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(eval-after-load "slime"
  '(progn
     (setq ac-auto-show-menu nil)
     (define-key slime-mode-map (kbd "C-c TAB") 'ac-show-menu)))

(add-hook 'slime-repl-mode-hook #'(lambda ()
                                    (linum-mode -1)
                                    (setq global-hl-line-mode nil)))

(require-package 'hl-sexp)

(eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'(lambda ()
                     (linum-mode 1)
                     (enable-paredit-mode)
                     (flycheck-mode))))

(add-hook 'lisp-interaction-mode-hook #'(lambda () (linum-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'scheme-mode "scheme" "Major mode for Scheme." t)
(setq auto-mode-alist
      (append
       '(("\\.rkt$" . scheme-mode)
         ("\\.scm$" . scheme-mode)) auto-mode-alist))

(add-hook 'scheme-mode-hook #'(lambda ()
                                (linum-mode 1)
                                (enable-paredit-mode)
                                (flycheck-mode)))

;; Geiser
(require-package 'geiser)
(eval-after-load "geiser"
  '(progn
     (setq geiser-active-implementations '(racket))
     (setq geiser-repl-query-on-kill-p nil)))

;; ac-geiser
(require-package 'ac-geiser)
(dolist (hook '(geiser-mode-hook geiser-repl-mode-hook))
  (add-hook hook (ac-geiser-setup)))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

(provide 'init-lisp-mode)
;;; init-lisp-mode.el ends here
