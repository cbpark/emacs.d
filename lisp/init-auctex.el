;;; init-auctex.el --- Customizing AUCTeX mode
;;; Commentary:
;;; Code:

(require-package 'auctex)
(setq auto-mode-alist (append '(("\\.tex\\'" . latex-mode)) auto-mode-alist))

(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (if *is-darwin*
      (setq TeX-view-program-list
            '(("DVI Viewer"  "/usr/bin/open -a TeXShop %o")
              ("PDF Viewer"  "/usr/bin/open -a Preview %o")
              ("HTML Viewer" "/usr/bin/open -a Safari %o")))
    (setq TeX-view-program-list
          '(("DVI Viewer"  "xdg-open %o")
            ("PDF Viewer"  "xdg-open %o")
            ("HTML Viewer" "xdg-open %o")))))

(require-package 'company-math)
(defun company-math-setup ()
  "Add company-math backends."
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

(defun LaTeX-delete-environment ()
  "Delete the environment wrapper."
  (interactive)
  (when (LaTeX-current-environment)
    (save-excursion
      (let* ((begin-start (save-excursion
                            (LaTeX-find-matching-begin)
                            (point)))
             (begin-end (save-excursion
                          (goto-char begin-start)
                          (search-forward-regexp "begin{.*?}")))
             (end-end (save-excursion
                        (LaTeX-find-matching-end)
                        (point)))
             (end-start (save-excursion
                          (goto-char end-end)
                          (1- (search-backward-regexp "\\end")))))
        (delete-region end-start end-end)
        (delete-region begin-start begin-end)))))

(with-eval-after-load 'latex
  (setq latex-run-command "pdflatex")
  (setq LaTeX-default-style "article")
  (setq LaTeX-default-environment "align")

  (define-key LaTeX-mode-map (kbd "C-c C-d") 'LaTeX-delete-environment)

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (company-math-setup)
                               (when *has-aspell* (flyspell-mode))
                               (turn-on-reftex))))

(with-eval-after-load 'reftex
  (setq reftex-plug-into-AUCTeX t
        reftex-extra-bindings t))

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; latex-preview-pane
(when *is-gui* (require-package 'latex-preview-pane))

(provide 'init-auctex)
;;; init-auctex.el ends here
