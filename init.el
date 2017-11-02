(setq load-prefer-newer t) ;; Let's do that early.

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)
(require 'borg-nix-shell)
(setq borg-build-shell-command 'borg-nix-shell-build-command)

(require 'no-littering)
(require 'auto-compile)

(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; A utility macro to declare that a drone is actually required.
(defvar thblt/required-drones '())

(defmacro want-drone (&rest d)
  "Declare required drones and their dependencies.

This macro takes a list of symbols D and treats it as a list of
drone names, optionnally followed by the list of their
dependencies.  For example:

(want-drone borg-queen (borg))

Multiple drones may be declared in a single declaration:

(want-drone drone1 (depA depB)
            drone2
            drone3 (depC depB))

For that purpose, this macro is aliased to `want-drones'.

At some point, the Queen will use this to identify orphans.")

(defalias 'want-drones 'want-drone)

(want-drones auto-compile
             borg
             no-littering)

;; Before we start, if this is a new installation, build all the
;; drones.

(let ((flag-file (no-littering-expand-var-file-name "borg/.initial-build-done")))
  (unless (file-exists-p flag-file)
    (message "Building Borg drones...")
    (mapc 'borg-build (borg-drones))

    (make-directory (file-name-directory flag-file))
    (write-region "" nil flag-file)))

;; We need to configure and (require) borg.el _before_ requiring org
;; and tangling dotemacs.org, or builtin org-mode will be loaded
;; instead of Elpa version and updated versions will never be used.

(unless noninteractive
  (require 'org)
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory)))
