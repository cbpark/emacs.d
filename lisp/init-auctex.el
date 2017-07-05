;;; init-auctex.el --- Customizing AUCTeX mode
;;; Commentary:
;;; Code:

(if (file-directory-p (concat *site-lisp-dir* "auctex"))
    (progn
      (load "auctex.el" nil t t)
      (load "preview-latex.el" nil t t))
  (require-package 'auctex))

(setq TeX-auto-save  t
      TeX-parse-self t
      TeX-save-query nil)
(setq-default TeX-master nil)
(setq LaTeX-default-style       "article"
      LaTeX-default-environment "align")

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(setq LaTeX-command "latex -synctex=1 --shell-escape -interaction=nonstopmode -halt-on-error -file-line-error")

;; synctex
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (when *has-aspell* (flyspell-mode))
                             (linum-mode -1)))

(defun LaTeX-delete-environment ()
  "Delete the environment wrapper."
  (interactive)
  (when (LaTeX-current-environment)
    (save-excursion
      (let* ((begin-start (save-excursion
                            (LaTeX-find-matching-begin)
                            (point)))
             (begin-end   (save-excursion
                            (goto-char begin-start)
                            (search-forward-regexp "begin{.*?}")))
             (end-end     (save-excursion
                            (LaTeX-find-matching-end)
                            (point)))
             (end-start   (save-excursion
                            (goto-char end-end)
                            (1- (search-backward-regexp "\\end")))))
        (delete-region end-start end-end)
        (delete-region begin-start begin-end)))))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-d") 'LaTeX-delete-environment))

;; company-math
(require-package 'company-math)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq-local company-backends
                                         (append '((company-math-symbols-latex
                                                    company-latex-commands))
                                                 company-backends))))

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t
      reftex-extra-bindings t)

;; doc-view with auto-revert to review output
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; latex-preview-pane
(when *is-gui* (require-package 'latex-preview-pane))

(provide 'init-auctex)
;;; init-auctex.el ends here
