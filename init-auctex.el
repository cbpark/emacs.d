;;; init-auctex.el --- Customizing AUCTeX mode
;;; Commentary:
;;; Code:

(require-package 'auctex)
(require 'tex-site)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(setq LaTeX-default-style "article")
(setq LaTeX-default-environment "align")
(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))
(when (string-equal system-type "darwin")
  (setq TeX-view-program-list
        '(("DVI Viewer" "/usr/bin/open -a TeXShop %o")
          ("PDF Viewer" "/usr/bin/open -a Preview %o")
          ("HTML Viewer" "/usr/bin/open -a Safari %o"))))

;; typing "$" automatically
(setq LaTeX-mode-hook
      '(lambda () (defun TeX-insert-dollar ()
               "custom redefined insert-dollar"
               (interactive)
               (insert "$$")
               (backward-char 1))))

;; RefTeX
(setq reftex-plug-into-AUCTeX t)
(setq reftex-extra-bindings t)

(add-hook 'latex-mode-hook (lambda ()
                             ;; spell-checking
                             (flyspell-mode)
                             ;; flycheck
                             (flyspell-mode)
                             ;; linum-mode
                             (linum-mode 1)))

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; yasnippet
(add-hook-fn latex-mode-hook (yas-minor-mode))

;; ac-ispell
(add-hook 'latex-mode-hook 'ac-ispell-ac-setup)

;; ac-math
(require-package 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources)))
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(ac-flyspell-workaround)

(provide 'init-auctex)
;;; init-auctex.el ends here
