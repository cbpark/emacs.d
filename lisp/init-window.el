;;; init-window.el --- Customization for Emacs windows
;;; Commentary:
;;; Code:

;; Turn off mouse interface in startup
(when (fboundp 'menu-bar-mode)
  (if (and *is-darwin* *is-gui*)
      (menu-bar-mode 1)
    (menu-bar-mode -1)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

;; No border
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; Disable scroll-bar and fringe: necessary for emacsclient
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (progn
              (modify-frame-parameters frame
                                       '((vertical-scroll-bars . nil)
                                         (horizontal-scroll-bars . nil)))
              (set-fringe-mode 0))))

;; Make the frame appear truly maximized
(setq frame-resize-pixelwise t)

;; Turn on winner-mode
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-x 0") 'winner-undo)
  (global-set-key (kbd "C-x 9") 'winner-redo))

(defun my-toggle-window-split ()
  "Toggle window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 6") 'my-toggle-window-split)

(defun my-swap-two-windows ()
  "Swap two windows."
  (interactive)
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))

(global-set-key (kbd "C-x 7") 'my-swap-two-windows)

;; Compilation buffer
;; (defun compile-autoclose-if-successful (buffer string)
;;   "Close the compilation BUFFER after a successful compilation, determined by reading the STRING."
;;   (when (and (buffer-live-p buffer)
;;              (string-match "compilation" (buffer-name buffer))
;;              (string-match "finished" string)
;;              (not (with-current-buffer buffer
;;                     (goto-char (point-min))
;;                     (search-forward "warning" nil t))))
;;     (bury-buffer "*compilation*")
;;     (winner-undo)
;;     (message "Build successful.")))
;; (setq compilation-finish-functions 'compile-autoclose-if-successful)

(require 'ansi-color)
(defun my-ansi-colorize-compilation ()
  "Handle ANSI escape sequences."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my-ansi-colorize-compilation)

(provide 'init-window)
;;; init-window.el ends here
