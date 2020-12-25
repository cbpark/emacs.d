;;; init-encoding.el -- UTF-8 encoding
;;; Commentary:
;;; Code:

(require 'ucs-normalize)
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(provide 'init-encoding)
;;; init-encoding.el ends here
