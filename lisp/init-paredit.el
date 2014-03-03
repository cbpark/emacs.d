;;; init-paredit.el --- Paredit
;;; Commentary:
;;; Code:

(require-package 'paredit)
(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on."
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)))
(ad-activate 'paredit-mode)

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)))

(provide 'init-paredit)
;;; init-paredit.el ends here
