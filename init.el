(setq load-prefer-newer t) ;; Let's do that early.

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(require 'no-littering)
(require 'auto-compile)

(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; A utility macro to declare that a drone is actually required.
(defvar thblt/required-drones '())

(defmacro want-drone (&rest d)
  "Declare drones D as required by the current configuration."
  (mapc (lambda (x) (add-to-list 'thblt/required-drones (symbol-name x))) d)
  'thblt/required-drones)

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

(require 'org)
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
