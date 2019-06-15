;;; pure-p.el --- attempt to verify that just loading a package is free of side effects

;;; Commentary:

;; pure-p attempts to determine if loading package x or its autoload
;; file will have side effects.  A side effect is defined as anything
;; that isn't defining a symbol scoped within the x- pseudo-namespace.
;;
;;;; Limitations
;;
;; pure-p is NOT a security mechanism, and WON'T detect even poorly
;; hidden attacks.  Its goal is to identify design issues in packages
;; before they wreck havoc on a running Emacs, NOT to perform any kind
;; of security analysis.
;;
;; Also worthy of note is the fact that loading a package may trigger
;; expansion of macros defined by the package and accepted by pure-p,
;; and such expansion may produce side effects, since macros have the
;; same access to the runtime environment as regular functions.  The
;; following code:
;;
;; #+begin_src emacs-lisp
;; ;; bad-package.el
;;
;; (defmacro bad-package-be-bad () (message "I'M IMPURE!") '(defvar bad-package-variable))
;; (bad-package-be-bad)
;; #+end_src
;;
;; won't be detected by pure-p as suspicious.

;;; Code:

(defconst pure-p-handlers
  "An alist "

  '(
    declare-function
    defalias ;; TODO Check prefix
    defconst
    define-derived-mode
    defmacro
    defun
    defvar
    eval-when-compile
    provide
    require
    ))

(defun pure-p-extract-package-name (file)
  (when (string-match (rx bol (group (minimal-match (+ any)))
                          (any
                           "-autoloads.el"
                           ".el") eol)
                      file)
    (substring file (match-beginning 1) (match-end 1))))



;; TODOs
;; - TODO with-eval-after-load
;; - DONE macros
;; - TODO def*: check prefix

(defun pure-p-check-file (file)
  "Flightcheck FILE."
  (unless (string-suffix-p ".el" file)
    (error "File name must end with \".el\""))
  (with-temp-buffer
    (insert-file file)
    (let (point
          lisp
          car)
      (condition-case nil
          (while t
            (setq point (point)
                  lisp (macroexpand (read (current-buffer)))
                  car (car lisp))
            (unless (member car thblt/flightcheck-whitelist)
              (message "SuspiAcious car in %s in position %s: %s" file point car))
            )
        (error (unless (= (point) (point-max))
                 (message "All read")
                 (error "Error reading %s at position %s." file point)))
        ))))

(thblt/flightcheck-file (expand-file-name"~/.emacs.d/lib/outshine/outshine.el"))
