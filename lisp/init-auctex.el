;;; init-auctex.el --- Customizing AUCTeX mode
;;; Commentary:
;;; Code:

(require-package 'auctex)
(require 'tex-site)

(setq auto-mode-alist
	  (append '(("\\.tex\\'" . latex-mode)) auto-mode-alist))

(eval-after-load 'tex
  '(progn
     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq TeX-save-query nil)
     (setq-default TeX-master nil)
     (setq TeX-PDF-mode t)
     (setq TeX-view-program-selection
           '((output-dvi "DVI Viewer")
             (output-pdf "PDF Viewer")
             (output-html "HTML Viewer")))

     (when (string-equal system-type "darwin")
       (setq TeX-view-program-list
             '(("DVI Viewer" "/usr/bin/open -a TeXShop %o")
               ("PDF Viewer" "/usr/bin/open -a Preview %o")
               ("HTML Viewer" "/usr/bin/open -a Safari %o"))))))

(eval-after-load 'latex
  '(progn
     (setq LaTeX-default-style "article")
     (setq LaTeX-default-environment "align")

     (add-hook 'LaTeX-mode-hook #'(lambda ()
                                    (setq electric-pair-pairs '((?\$ . ?\$)))
                                    (electric-pair-mode 1)
                                    (flyspell-mode)
                                    (linum-mode 1)
                                    (turn-on-reftex)
                                    (yas-minor-mode)
                                    (ac-ispell-ac-setup)))))

(eval-after-load 'reftex
  '(progn
     (setq reftex-plug-into-AUCTeX t)
     (setq reftex-extra-bindings t)))

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

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
