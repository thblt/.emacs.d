(let* ((el (expand-file-name "init.el" user-emacs-directory))
       (elc (format "%sc" el)))

  (unless (and
           (file-exists-p elc)
           (time-less-p
            (file-attribute-modification-time (file-attributes el))
            (file-attribute-modification-time (file-attributes elc))))
    (message "Recompiling init.el…")
    (byte-compile-file el)))

;; Never load bytecode if .el is more recent
(setq load-prefer-newer t)
