(setq load-prefer-newer t) ;; Let's do that early.

;;; Package manager
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; Add breaking commands here
