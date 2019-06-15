;; Pour le moment Borg passe la valeur de borg-build-shell-command au
;; processus fils so this file can be empty.

;; (add-to-list 'load-path "/home/thblt/.emacs.d/lib/borg-nix-shell")
;; (require 'borg-nix-shell)A
(message "Loading Borg user config")
(setq borg-build-shell-command 'borg-nix-shell-build-command
      borg-nix-shell-build-use-pure-shell t)
