(setq load-prefer-newer t) ;; Let's do that early.

;;; Borg

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;;; Auto compile

(auto-compile-on-load-mode)

;;; Load configuration

(require 'org)
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
