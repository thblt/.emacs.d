(setq load-prefer-newer t) ;; Let's do that early.

;;; Package manager
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; Convenience (BÉPO)
(define-key key-translation-map (kbd "M-é") (kbd "M-w"))
(define-key key-translation-map (kbd "M-w") (kbd "M-é"))
(define-key key-translation-map (kbd "C-é") (kbd "C-w"))
(define-key key-translation-map (kbd "C-w") (kbd "C-é"))
(define-key key-translation-map (kbd "C-M-é") (kbd "C-M-w"))
(define-key key-translation-map (kbd "C-M-w") (kbd "C-M-é"))
;; Read êÊ as <> (key right of left shift)
(define-key key-translation-map(kbd "M-ê") (kbd "M-<"))
(define-key key-translation-map(kbd "C-ê") (kbd "C-<"))
(define-key key-translation-map(kbd "C-M-ê") (kbd "C-M-<"))
(define-key key-translation-map(kbd "M-Ê") (kbd "M->"))
(define-key key-translation-map(kbd "C-Ê") (kbd "C->"))
(define-key key-translation-map(kbd "C-M-Ê") (kbd "C-M->"))

;; Add breaking commands here
(define-key global-map (kbd "M-é") 'embark-act)

(setq frame-title-format '("%b — GNU Emacs [" (:eval (frame-parameter (selected-frame) 'window-id)) "]"))
