;;; init-html-js.el --- Customizing html and js modes
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(dolist (hook '(html-mode-hook js-mode-hook))
  (add-hook hook #'(lambda ()
                     (flyspell-prog-mode)
                     (company-mode 1))))

(eval-after-load "js"
  '(add-hook 'js-mode-hook #'(lambda ()
                               (rainbow-delimiters-mode-enable)
                               (my-paredit-nonlisp))))

(provide 'init-html-js)
;;; init-html-js.el ends here
