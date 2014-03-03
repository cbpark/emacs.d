;; init-encoding.el -- UTF-8 encoding
;; Commentary:
;; Codes:

(require 'ucs-normalize)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(set-default-coding-systems 'utf-8-unix)

(provide 'init-encoding)
;; init-encoding.el ends here
