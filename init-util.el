;;; init-util.el --- Utils
;;; Commentary:
;;; Code:

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro add-hook-fn (name &rest body)
  `(add-hook ',name #'(lambda ()
                        ,@body)))

;; Find the directory containing a given library
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory
                           (find-library-name library-name))))

(provide 'init-util)
;;; init-util.el ends here
