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

     (cond (*is-darwin* (setq TeX-view-program-list
                              '(("DVI Viewer"  "/usr/bin/open -a TeXShop %o")
                                ("PDF Viewer"  "/usr/bin/open -a Preview %o")
                                ("HTML Viewer" "/usr/bin/open -a Safari %o"))))
           ((string-equal system-type "gnu/linux")
            (setq TeX-view-program-list
                  '(("DVI Viewer"  "xdg-open %o")
                    ("PDF Viewer"  "xdg-open %o")
                    ("HTML Viewer" "xdg-open %o")))))))

(require-package 'company-math)
(defun company-math-setup ()
  "Add company-math backends."
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

(eval-after-load 'latex
  '(progn
     (setq latex-run-command "pdflatex")
     (setq LaTeX-default-style "article")
     (setq LaTeX-default-environment "align")
     (add-hook 'LaTeX-mode-hook (lambda ()
                                  (company-math-setup)
                                  (flyspell-mode)
                                  (turn-on-reftex)))))

(eval-after-load 'reftex
  '(progn
     (setq reftex-plug-into-AUCTeX t)
     (setq reftex-extra-bindings t)))

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; latex-preview-pane
(when *is-gui*
  (require-package 'latex-preview-pane))

(provide 'init-auctex)
;;; init-auctex.el ends here
