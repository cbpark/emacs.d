;;; init-html-js.el --- Customizing html and js modes
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(dolist (hook '(html-mode-hook js-mode-hook))
  (add-hook hook (lambda ()
		   (flyspell-prog-mode)
		   (company-mode 1))))

(provide 'init-html-js)
;;; init-html-js.el ends here
