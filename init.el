(setq load-prefer-newer t) ;; Let's do that early.

;;; Borg

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)
(require 'borg-nix-shell)
(setq borg-build-shell-command 'borg-nix-shell-build-command)
(defmacro want-drone (&rest _))
(defalias 'want-drones 'want-drone)

(want-drone ;; borg '(epkg (closql))
            borg-nix-shell
            borg-queen)

;;; Auto compile

(want-drone auto-compile)
(auto-compile-on-load-mode)

;;; Load configuration

(unless noninteractive
  (require 'org)
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory)))
