;;; init-auctex.el --- Customizing AUCTeX mode
;;; Commentary:
;;; Code:

(require-package 'auctex)

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

     (cond ((string-equal system-type "darwin")
            (setq TeX-view-program-list
                  '(("DVI Viewer"  "/usr/bin/open -a TeXShop %o")
                    ("PDF Viewer"  "/usr/bin/open -a Preview %o")
                    ("HTML Viewer" "/usr/bin/open -a Safari %o"))))
           ((string-equal system-type "gnu/linux")
            (setq TeX-view-program-list
                  '(("DVI Viewer"  "xdg-open %o")
                    ("PDF Viewer"  "xdg-open %o")
                    ("HTML Viewer" "xdg-open %o")))))))

(eval-after-load 'latex
  '(progn
     (setq latex-run-command "pdflatex")
     (setq LaTeX-default-style "article")
     (setq LaTeX-default-environment "align")

     (add-hook 'LaTeX-mode-hook #'(lambda ()
                                    (flyspell-mode)
                                    (turn-on-reftex)
                                    (rainbow-delimiters-mode-enable)))))

(eval-after-load 'reftex
  '(progn
     (setq reftex-plug-into-AUCTeX t)
     (setq reftex-extra-bindings t)))

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(provide 'init-auctex)
;;; init-auctex.el ends here
