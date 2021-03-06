;;; ~thblt/.emacs.d/init.el -*- lexical-binding: t; -*-

(setq gc-cons-percentage 100
      gc-cons-threshold most-positive-fixnum
      garbage-collection-messages t)

(message "███████╗███╗   ███╗ █████╗  ██████╗███████╗")
(message "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝")
(message "█████╗  ██╔████╔██║███████║██║     ███████╗")
(message "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║")
(message "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║")
(message "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝ %s\n"
         (if lexical-binding "NT 4" "3.11 FOR WORKGROUPS"))

;;; Introduction

;; This chapter deals with the general use of Emacs, and is limited to
;; general settings and sane defaults.  It's a bit messy, since it's
;; mostly made up of all the bits that don't fit anywhere else.

;; Never load bytecode if .el is more recent
(setq load-prefer-newer t)

;;;; Package managers

;;;;; package.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize 'no-activate)

;;;;; Borg

;; Borg comes second, because it comes first.  The second initialized
;; manager will be the first in load-path.

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;;;;; Paths

(require 'no-littering)

;;;; Settings

(setq user-full-name "Thibault Polge"
      user-mail-address "thibault@thb.lt"

      ;; For some reason, the default value of =max-specpdl-size=
      ;; prevents [[Mu4e][Mu4e]] from correctly rendering some HTML
      ;; e-mails.  We increase it from 1300 to 5000.
      max-specpdl-size 5000
      ;; Numbered backups, because catastrophes happen.  The numbers
      ;; may be a bit crazy, but better safe than sorry.
      version-control t
      kept-new-versions 500
      kept-old-versions 500
      delete-old-versions t
      ;; But lockfiles are so 1990
      create-lockfiles nil
      ;; Don't lose the contents of system clipboard when killing from Emacs:
      save-interprogram-paste-before-kill t
      custom-file "/dev/null" ;; (no-littering-expand-var-file-name "customize.el")

      inhibit-compacting-font-caches (eq system-type 'windows-nt) ; This prevents slowdown when using strange characters.

      ;; Use default browser from the system. Using =setsid xdg-open=
      ;; prevents Emacs from killing xdg-open before it actually opened
      ;; anything. See
      ;; [[https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open][here]].
      browse-url-browser-function (if (eq system-type 'windows-nt) 'browse-url-default-browser 'browse-url-generic)
      ;; browse-url-generic-program "setsid"
      browse-url-generic-program "firefox")
;; browse-url-generic-args '("xdg-open"))

(load custom-file t)

(setq-default major-mode 'text-mode)

(defun thblt/disable-key-translations (&optional frame)
  "Disable key translations used for terminal compatibility, selecting FRAME.

Notice the variables this sets are terminal-local, not frame
local."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (define-key input-decode-map [?\C-m] [C-m])
      (define-key input-decode-map [?\C-i] [C-i]))))
;; Don't lose usage of C-* in X

(add-to-list 'after-make-frame-functions 'thblt/disable-key-translations)
(unless (daemonp)
  (thblt/disable-key-translations))

;; I do that by accident, so let's not
(define-key global-map [remap suspend-frame] 'ignore) ; nil doesn't unset.
(define-key global-map [remap suspend-emacs] 'ignore)

;;; User interface

;;;; Settings and general configuration

(setq-default
 enable-recursive-minibuffers t
 inhibit-startup-screen t
 use-dialog-box nil
 vc-follow-symlinks t

 truncate-lines t

 disabled-command-function nil)

(minibuffer-depth-indicate-mode)

;; Line numbers
(setq-default display-line-numbers-type 'relative
              display-line-numbers-major-tick 10
              display-line-numbers-minor-tick 5
              display-line-numbers-current-absolute t)

;; Line numbers in mode-line
(column-number-mode)

;; Cursor configuration
(setq-default  cursor-type 'box)
(blink-cursor-mode)

;; Never use the "safe" ~yes-or-no~ function:
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't show the menu bar, unless this is MacOS.  Never show toolbar
;; or scrollbars.
(unless (string= 'system-type 'darwin) (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Mouse wheel scrolling makes big jumps by default, let's make it smoother.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse

      scroll-step 1)

;; Rebind =C-x k= to kill the /current/ buffer.
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))

;; And force leave the minibuffer with =C-M-]=
(global-set-key (kbd "C-M-à") 'abort-recursive-edit)

;; Fringes are pointless
(fringe-mode 0)

;; Rebind =C-x C-b= to =ibuffer= instead of =list-buffers=:
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; A small function to identify the face at point.  Nice to have when
;; writing themes, and faster than C-u C-x =
(defun what-face (pos)
  "Show face at POS, defaulting to point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;; Fonts and themes

;; Configure the default font:
(add-to-list 'default-frame-alist '(font . "Iosevka"))
(set-face-attribute 'default nil
                    :height (pcase (system-name)
                              ("dru" 105)
                              ("maladict" 100)))

(add-to-list 'custom-theme-load-path borg-drones-directory)
(add-to-list 'load-path borg-drones-directory)
(setq x-underline-at-descent-line t)

(defun thblt/disable-all-themes ()
  (interactive)
  (mapc 'disable-theme custom-enabled-themes))

;; Note to self: theme is configured in solaris.el
(defun thblt/dark-theme () "Activate dark theme." (interactive) (thblt/disable-all-themes) (load-theme 'solaris-dark t))
(defun thblt/light-theme () "Activate light theme." (interactive) (thblt/disable-all-themes) (load-theme 'solaris-light t))
(defun thblt/dark-hc-theme () "Activate dark (high contrast) theme." (interactive) (thblt/disable-all-themes) (load-theme 'solaris-dark-high-contrast t))
(defun thblt/light-hc-theme () "Activate light (high contrast) theme." (interactive) (thblt/disable-all-themes) (load-theme 'solaris-light-high-contrast t))
(defun thblt/zenburn-theme () "Activate Zenburn-ish theme." (interactive) (thblt/disable-all-themes) (load-theme 'solaris-zenburn t))

;; Theme is loaded at the very end of this file.

(defun disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  (mapc 'disable-theme custom-enabled-themes))

;;;; Projectile

(eval-when-compile (require 'projectile))

(setq projectile-completion-system 'auto
      ;; globally ignore undo-files and similar byproducts.
      projectile-globally-ignored-file-suffixes '(".un~"
						                                      ".~undo-tree~")
      ;; Manage submodules as distinct projects.
      projectile-git-submodule-command nil

      ;; This slows down bootstrap, and doesn't make much sense since
      ;; Projectile persists its project list.
      projectile-auto-discover nil

      projectile-project-search-path
      (mapcar 'expand-file-name '("/etc/"
                                  "~"
                                  "~/.emacs.d/lib/"
                                  "~/Documents/"
                                  "~/Documents/Code")))

(projectile-mode)

;; If we don't have any project, discover them
(unless projectile-known-projects
  (message "Looking for projects in `projectile-project-search-path'.  This will happen only once.")
  (projectile-discover-projects-in-search-path)
  (message "Found %s projects." (length projectile-known-projects)))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(diminish 'projectile-mode)

;;;; UI Utilities

;;;;; Hydra

(eval-when-compile
  (require 'hydra))

(setq hydra-hint-display-type 'message)

;;;;; Ivy

(vertico-mode)
(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless))

(require 'embark-consult)

(define-key vertico-map (kbd "M-<RET>") 'embark-act)

(setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

;;;;; Shackle

(eval-when-compile
  (require 'magit))

(require 'shackle)
(require 'sway)

(setq shackle-rules
      `(("*Help*" :align t :select t)
        ;; ** Magit **
        (magit-status-mode :same t)
        ((:custom
          ,(lambda (buffer)
             (with-current-buffer buffer
               (and
                (eq major-mode 'magit-diff-mode)
                magit-display-buffer-noselect))))
         :select nil :frame t :dedicate t)
        ((:custom
          ,(lambda (buffer)
             (with-current-buffer buffer
               (bound-and-true-p magit-display-buffer-noselect))))
         :select nil :frame nil :dedicate t)
        (magit-log-mode :same t)
        (magit-submodule-list-mode :same t)
        (magit-revision-mode :same t)
        (magit-process-mode :frame nil)
        ("COMMIT_EDITMSG" :popup t :select t)
        ("^magit.*$'" :regexp t :frame nil)
        (" *transient*" :frame nil :popup t :select nil) ; Magit helper popups
        ;; ** Common Emacs UI elements **
        ("*Completions*" :frame nil :popup t :select t) ; Magit helper popups
        ;; ** Sunrise commander **
        (sunrise-mode :custom (lambda (&rest _)))
        ;; ** Proced **
        ("*Proced*" :same t)
        (" *Marked Processes*" :frame nil :popup t :select t)
        ;; ** Byte-compiler
        ("*Compile-Log*" :frame nil :popup t :select t)
        ;; ** Local variables warning **
        ("*Local Variables*" :same t :frame nil :popup t :select t)
        ("*Backtrace*" :frame t :select t :dedicate t)
        ;; ** Misc **
        ("*Org PDF LaTeX Output*" :select nil)
        ("*Org Preview LaTeX Output*" :select nil)
        (" *undo-tree*" :frame nil)
        ("*Register Preview*" :frame nil :noselect t)
        (flycheck-error-list-mode :select t)
        ((compilation-mode "\\`\\*firestarter\\*\\'"
                           "\\`\\*magit-diff: .*?\\'") :regexp t :noselect t)
        ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))

      shackle-default-rule '(:frame t)
      shackle-default-size 0.4
      shackle-inhibit-window-quit-on-same-windows t
      shackle-display-buffer-frame-function 'sway-shackle-display-buffer-frame)

(shackle-mode)
(sway-socket-tracker-mode)
(sway-undertaker-mode)
(sway-x-focus-through-sway-mode)

;;;; Resurrecting *scratch*

(defun thblt/resurrect-scratch (&optional only-install)
  "Add self as a hook to scratch, creating it if it doesn't exist already."
  (when (and (not only-install)
             (eq (current-buffer) (get-buffer "*scratch*")))
    (rename-buffer "*this-scratch-is-no-more*" t))
  (with-current-buffer (get-buffer-create "*scratch*")
    (when (eq (point-min) (point-max))
      (insert initial-scratch-message))
    (lisp-interaction-mode)
    (add-hook 'kill-buffer-query-functions 'thblt/resurrect-scratch 0 t)))

;;;; BÉPO adjustments

;; Unshifted digit argument

(defmacro thblt/digit-argument-with-value (char)
  "Simulate `digit-argument' as if it was called by pressing CHAR.

This can be used to update the digit argument from arbitrary keys."
  `(lambda () (interactive)
     (prefix-command-preserve-state)
     (let ((last-command-event ,char))
       (call-interactively 'digit-argument))))

(define-key universal-argument-map (kbd "\"") (thblt/digit-argument-with-value ?1))
(define-key universal-argument-map (kbd "«") (thblt/digit-argument-with-value ?2))
(define-key universal-argument-map (kbd "»") (thblt/digit-argument-with-value ?3))
(define-key universal-argument-map (kbd "(") (thblt/digit-argument-with-value ?4))
(define-key universal-argument-map (kbd ")") (thblt/digit-argument-with-value ?5))
(define-key universal-argument-map (kbd "@") (thblt/digit-argument-with-value ?6))
(define-key universal-argument-map (kbd "+") (thblt/digit-argument-with-value ?7))
(define-key universal-argument-map (kbd "-") (thblt/digit-argument-with-value ?8))
(define-key universal-argument-map (kbd "/") (thblt/digit-argument-with-value ?9))
(define-key universal-argument-map (kbd "*") (thblt/digit-argument-with-value ?0))

;; Beginning and end of buffer

(define-key global-map(kbd "M-ê") 'beginning-of-buffer)
(define-key global-map (kbd "M-Ê") 'end-of-buffer)

(defmacro thblt/self-insert-this (char)
  "Run `self-insert' as if it was called by pressing CHAR."
  `(lambda () (interactive)
     (let ((last-command-event ,char))
       (call-interactively 'self-insert-command))))

(defvar thblt/normalize-spaces-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd " ") (thblt/self-insert-this ? ))
    (define-key map (kbd " ") (thblt/self-insert-this ?_))
    map))

;; Fuck BÉPO's shift-space in prog-mode
(define-minor-mode thblt/normalize-spaces-mode
  "Map various non-breaking spaces to a regular space."
  :lighter " spaces"
  :keymap thblt/normalize-spaces-mode-map)

(add-hook 'prog-mode-hook 'thblt/normalize-spaces-mode)
(diminish 'thblt/normalize-spaces-mode)

;;; Editing text

;; This chapter deals with /general/ text editing.  The next two configure
;; prose and code editing, respectively.

(setq shift-select-mode nil)

(editorconfig-mode)
(diminish 'editorconfig-mode "")

(define-key global-map (kbd "M-<SPC>") 'cycle-spacing)

(define-key global-map (kbd "M-i")
  (defun thblt/imenu-or-outline (arg)
    "With no arg, execute `consult-imenu'. With an argument,
`consult-outline'. If the function to execute isn't defined,
execute `imenu' instead." ; Yes I know docstrings need a symbol.
    (interactive "P")
    (cond ((and (not arg) (fboundp 'consult-imenu))
           (consult-imenu))
          ((fboundp 'consult-outline)
           (consult-outline))
          (t (call-interactively 'imenu)))))

;;;; The editor appearance hydra

(eval-when-compile
  (require 'hydra)
  (require 'visual-fill-column))

(eval-and-compile
  (defmacro thblt/hydra-indicator (desc active)
    "Return DESC with a state indicator determined by ACTIVE.))

If at runtime ACTIVE is an unbound symbol it is interpreted as
nil; otherwise it's evaluated normally."
    `(format "[%s] %s" (if ,(if (symbolp active)
                                `(bound-and-true-p ,active)
                              active)
                           (propertize "✔" 'face 'bold)
                         (propertize "-" 'face 'shadow))
             ,desc)))

(defhydra hydra-editor-appearance ()
  ("b" text-scale-decrease "Size -" :column "Font and theme")
  ("é" thblt/text-scale-reset (thblt/hydra-indicator "Default size"
                                                     (not (bound-and-true-p text-scale-mode))))
  ("p" text-scale-increase "Size +")
  ("V" variable-pitch-mode (thblt/hydra-indicator "Var. pitch" buffer-face-mode))
  ("tl" thblt/light-theme "Light theme")
  ("tL" thblt/light-hc-theme "Light (high contrast) theme")
  ("td" thblt/dark-theme "Dark theme")
  ("tD" thblt/dark-hc-theme "Dark (high contrast) theme")
  ("tz" thblt/zenburn-theme "Zenburn theme")

  ("f" thblt/visual-fill-column-toggle-mode (thblt/hydra-indicator "Visual fill" visual-fill-column-mode) :column "Appearance")
  ;; @FIXME This breaks is `visual-fill-column' hasn't been loaded yet.
  ("c" thblt/visual-fill-column-toggle-centering (thblt/hydra-indicator "Centering" visual-fill-column-center-text))
  ("g" thblt/visual-fill-column-width-decrease "Width -")
  ("h" thblt/visual-fill-column-width-increase "Width +")
  ("l" visual-line-mode (thblt/hydra-indicator "Line wrap" visual-line-mode))
  ("-" toggle-word-wrap (thblt/hydra-indicator "Word wrap" word-wrap))
  ("L" display-line-numbers-mode (thblt/hydra-indicator "Line numbers" display-line-numbers-mode))

  ("r" rainbow-mode (thblt/hydra-indicator "Rainbow" rainbow-mode) :column "Helpers")

  ("W" superword-mode (thblt/hydra-indicator "super-word" superword-mode))
  ("w" subword-mode (thblt/hydra-indicator "SubWord" subword-mode))

  ("a" auto-fill-mode (thblt/hydra-indicator "Auto fill" auto-fill-function) :column "Magic/more magic")
  ("A" refill-mode (thblt/hydra-indicator "Auto refill" refill-mode))
  ("I" aggressive-indent-mode (thblt/hydra-indicator "Aggressive indent" aggressive-indent-mode))
  ("de" toggle-debug-on-error (thblt/hydra-indicator "Debug on error" debug-on-error))
  ("dq" toggle-debug-on-quit (thblt/hydra-indicator "Debug on quit" debug-on-quit))

  ("!" flycheck-mode (thblt/hydra-indicator "Code" flycheck-mode) :column "Utility")
  ("?" flyspell-mode  (thblt/hydra-indicator "Spell" flyspell-mode))
  ("F" thblt/ispell-use-french (thblt/hydra-indicator "Français" (string= (bound-and-true-p ispell-local-dictionary) "french")))
  ("E" thblt/ispell-use-english (thblt/hydra-indicator "English" (string= (bound-and-true-p ispell-local-dictionary) "english")))

  ("R" auto-revert-mode (thblt/hydra-indicator "Auto-revert" auto-revert-mode) :column "Misc"))

(define-key global-map (kbd "C-c l") 'hydra-editor-appearance/body)

(defun thblt/visual-fill-column-reset (&optional activate)
  "Turn visual-fill-column off and on again.  Also ACTIVATE it if non-nil.

For use by `hydra-editor-appearance/body'."
  (interactive)
  (when (or
         activate
         (bound-and-true-p visual-fill-column-mode))
    (visual-fill-column-mode -1)
    (visual-fill-column-mode t)))

(defun thblt/ispell-use-french ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (ispell-change-dictionary "french")
  (when flyspell-mode (flyspell-buffer)))

(defun thblt/ispell-use-english ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (ispell-change-dictionary "english")
  (when flyspell-mode (flyspell-buffer)))

(defun thblt/text-scale-reset ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (text-scale-set 0))

(defun thblt/visual-fill-column-toggle-mode ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (setq visual-fill-column-center-text nil)
  (call-interactively 'visual-fill-column-mode))

(defun thblt/visual-fill-column-toggle-centering ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (if
      (setq visual-fill-column-center-text (not (bound-and-true-p visual-fill-column-center-text)))
      (thblt/visual-fill-column-reset visual-fill-column-center-text)
    (visual-fill-column-mode 0)))

(defun thblt/visual-fill-column-width-adjust (delta)
  "Adjust visual fill column by DELTA."
  (interactive)
  (setq visual-fill-column-width
        (+ delta (or visual-fill-column-width fill-column)))
  (thblt/visual-fill-column-reset))

(defun thblt/visual-fill-column-width-increase ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (thblt/visual-fill-column-width-adjust 5))

(defun thblt/visual-fill-column-width-decrease ()
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (thblt/visual-fill-column-width-adjust -5))

;;;; Spell checking

(eval-when-compile
  (require 'flyspell)
  (require 'ispell))

(setq ispell-program-name "aspell"
      ispell-silently-savep t)

;; Enable Flyspell:
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))
(diminish 'flyspell-mode "Fly")

;; Correct words using Ivy instead of default method:

(with-eval-after-load 'flyspell
  (require 'flyspell-correct)
  (define-key flyspell-mode-map (kbd "M-$") 'flyspell-auto-correct-previous-word)
  (define-key flyspell-mode-map (kbd "C-,") 'flyspell-correct-previous))

;;;; Moving around

(define-key global-map (kbd "M-\"") 'backward-paragraph)
(define-key global-map (kbd "M-«") 'forward-paragraph)

(define-key global-map (kbd "C-M-\"") 'beginning-of-defun)
(define-key global-map (kbd "C-M-«") 'end-of-defun)

;;;;; avy

(define-key global-map (kbd "C-:") 'avy-goto-char-timer)
(define-key global-map (kbd "M-g f") 'avy-goto-line)

;;;;; beginend

(eval-when-compile
  (require 'beginend))

(beginend-global-mode)

(mapc (lambda (m) (diminish (cdr m)))
      beginend-modes)
(diminish 'beginend-global-mode)

;;;;; mwim

(eval-when-compile
  (require 'haskell-interactive-mode))

(define-key global-map [remap move-beginning-of-line] 'mwim-beginning)
(define-key global-map [remap move-end-of-line] 'mwim-end)
(define-key visual-line-mode-map [remap move-beginning-of-line] 'mwim-beginning)
(define-key visual-line-mode-map [remap move-end-of-line] 'mwim-end)
;; but…
(with-eval-after-load 'haskell-interactive-mode
  (define-key haskell-interactive-mode-map (kbd "C-a") 'haskell-interactive-mode-bol)
  (diminish 'interactive-haskell-mode " λ"))

;;;;; pulse (don't get lost)

(advice-add 'recenter-top-bottom :after (lambda (_) (pulse-momentary-highlight-one-line (point))))
(advice-add 'scroll-down-command :after (lambda (_) (pulse-momentary-highlight-one-line (point))))
(advice-add 'scroll-up-command :after (lambda (_) (pulse-momentary-highlight-one-line (point))))
(advice-add 'yank :after (lambda (_) (pulse-momentary-highlight-region (point) (mark))))
(advice-add 'yank-pop :after (lambda (_) (pulse-momentary-highlight-region (point) (mark))))

;;;; Replace

(define-key global-map (kbd "C-M-%") 'vr/query-replace)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;;;; Minor modes

;;;;; Auto-revert-mode

(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode " 🔃"))

;;;;; Recentf

(recentf-mode)

;;;;; Smartparens

(require 'smartparens-config) ;; Load default config

(smartparens-global-mode)
(show-smartparens-global-mode)

(define-key smartparens-mode-map (kbd "C-M-f")  'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b")  'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d")  'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a")  'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d")  'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a")  'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e")  'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u")  'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t")  'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n")  'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p")  'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k")  'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w")  'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>")  'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>")  'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>")  'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>")  'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>")  'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D")  'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>")  'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>")  'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>")  'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]")  'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>")  'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]")  'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F")  'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B")  'sp-backward-symbol)

(diminish 'smartparens-mode)

;;;;; Undo-Tree

(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;;;; Yasnippet

(setq yas-snippet-dirs
      (list (no-littering-expand-etc-file-name "snippets")
            (borg-worktree "yasnippet-snippets/snippets")))

;; @FIXME Takes almost a second!
(yas-global-mode)
(diminish 'yas-minor-mode)

(defun thblt/split-path (path)
  (let ((base t)
        (list))
    (while (< 1 (length path))
      (setq base (file-name-nondirectory path)
            path (substring (file-name-directory path) 0 -1))
      (push base list))
    list))

;;;;;; Snippet helpers

(defun thblt/guess-module-name-from-path (path &optional keep-extension)
  "Guess a Haskell-ish module name.

This works by concatenating PATH components that don't start with
a lowercase letter and dropping the extension, unless KEEP-EXTENSION."
  (let ((module)
        (case-fold-search nil))
    (mapc (lambda (item)
            (if (string-match-p (rx bos lower) item)
                (setq module nil)
              (push item module)))
          (thblt/split-path path))
    (unless keep-extension
      (setf (car module) (file-name-base (car module))))
    (mapconcat 'identity (reverse module) ".")))

(defun thblt/guess-module-name (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when buffer-file-name
      (thblt/guess-module-name-from-path buffer-file-name))))

;;;; Misc

(defun kill-duplicate-blank-lines ()
  "Replace successive empty lines by a single one."
  (interactive)
  (let ((regexp (rx bol (* whitespace) eol)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (forward-line 1)
        (while (looking-at-p regexp)
          (kill-whole-line 1)))))) ; t because kill-line returns nil

(defmacro with-maybe-region (name region-fun default-fun)
  "Creates a command NAME wrapping REGION-FUN and DEFAULT-FUN.

If the region is active, call REGION-FUN interactively, otherwise
DEFAULT-FUN."
  `(defun ,name ()
     ,(format "Run %s if region is active, %s otherwise." region-fun default-fun)
     (interactive)
     (call-interactively
      (if (use-region-p) ',region-fun ',default-fun))))

(define-key global-map [remap upcase-word]
  (with-maybe-region upcase-something upcase-region upcase-word))
(define-key global-map [remap downcase-word]
  (with-maybe-region downcase-something downcase-region downcase-word))
(define-key global-map [remap capitalize-word]
  (with-maybe-region capitalize-something capitalize-region capitalize-word))

;;;;; Bindings

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-y") 'copy-line)
(global-set-key (kbd "C-h") 'delete-backward-char)

(define-key global-map (kbd "M-Q") 'unfill-paragraph)

;;;;; Autosave when losing focus

(super-save-mode +1)
(diminish 'super-save-mode)

;;;;; What to do when saving

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Make scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Writing prose

;;;; Common settings and minor modes
;;;; abbrev

(add-hook 'text-mode-hook (lambda () (abbrev-mode t)))
(diminish 'abbrev-mode)

;;;; Wordwrap/visual line/visual-fill-column

(diminish 'visual-line-mode)

;;;; Major modes

;;;; AucTex

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (visual-line-mode t)
            (TeX-fold-mode t)
            (aggressive-indent-mode)))

(setq-default TeX-save-query nil ; Autosave
              TeX-parse-self t
              TeX-engine 'xetex
              TeX-source-correlate-mode t)

(sp-with-modes 'latex-mode
  (sp-local-pair "«" "»")
  (sp-local-pair "“" "”"))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "M-RET") 'latex-insert-item))

;;;; Org-Mode

(eval-when-compile
  (require 'org)
  (require 'ox-latex))

(setq org-catch-invisible-edits t
      org-hide-leading-stars t
      org-hide-emphasis-markers nil
      org-html-htmlize-output-type 'css
      org-imenu-depth 8
      org-src-fontify-natively t
      org-use-speed-commands nil
      ;;org-ellipsis " ▼"
      org-special-ctrl-a/e t
      org-special-ctrl-k t)

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode t)
                           (visual-line-mode t)))

;; Use shift-arrow for window navigation when not on an heading
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))

;;;;; Pairs

(defun sp--org-skip-asterisk (_ mb me)
  "I assume: skip opening * in matched string _ between MB and ME."
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»")
  (sp-local-pair "“" "”"))

;;;;; Export

(with-eval-after-load 'org
  (when (require 'ox-extra nil t)
    (ox-extras-activate '(ignore-headlines))))

(setq org-latex-pdf-process (list "latexmk -CA %f" "latexmk -f -pdfxe -xelatex %f"))
(setq org-latex-pdf-process (list "latexmk -f -pdfxe -xelatex %f"))

;;;;; Org-agenda:

(setq org-agenda-files (list "~/Documents/LOG.org")
      org-default-notes-file "~/Documents/LOG.org")

;;;;; Org-babel

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t))))

;;;;; Stuff

(defun thblt/org-insert-magic-link (url)
  "Auto create org links to Wikipedia URL."
  (interactive "sLink to? ")
  (require 'url-util)
  (let ((title))
    (cond
     ((string-prefix-p "https://fr.wikipedia.org/wiki/" url)
      (setq title (decode-coding-string (url-unhex-string (substring url 30)) 'utf-8)))
     (t (error "I have no idea what to do with this")))
    (org-insert-link nil url title)))

(setq org-num-skip-unnumbered t)
(defun thblt/org-num-format-function/cours (numbers)
  (let ((seance (car numbers))
        (partie (cadr numbers))
        (sous-partie (caddr numbers))
        (section (cadddr numbers))
        (moment (nth 4 numbers)))
    (cond
     (moment (format "(%s) "
                     (nth (- moment 1) (mapcar 'char-to-string (number-sequence ?α ?ω)))))
     (section (format "(%s) " section))
     (sous-partie (format "%s/ "
                          (nth (- sous-partie 1) (mapcar 'char-to-string (number-sequence ?A ?Z)))))
     (partie (format "%s/ " (org-export-number-to-roman partie)))
     (seance (format "[SÉANCE %s] " seance))
     (t ""))))

;;; Writing code

;;;; Settings

;; Some basic settings...

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq-default comment-empty-lines nil
              tab-width 2
              c-basic-offset 2
              cperl-indent-level 2
              indent-tabs-mode nil)

(global-set-key (kbd "<f8>") 'ffap)

;;;; Minor modes
;;;;; Aggressive indent

(require 'aggressive-indent)

(setq aggressive-indent-dont-electric-modes
      '( haskell-mode
         python-mode
         ruby-mode
         org-mode ))

(aggressive-indent-global-mode)

(diminish 'aggressive-indent-mode " ⭾")

;;;;; Company

(eval-when-compile
  (require 'company))

(add-hook 'prog-mode-hook 'company-mode)

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-TAB") 'company-complete-common)
  (diminish 'company-mode " Co"))

;;;;; Evil Nerd Commenter

(define-key global-map [remap comment-dwim] 'evilnc-comment-or-uncomment-lines)

;;;;; Hi-lock

(eval-when-compile
  (require 'hi-lock))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (hi-lock-set-pattern "@?FIXME" 'hi-red-b)
            (hi-lock-set-pattern "@?XXX" 'hi-red-b)
            (hi-lock-set-pattern "@?TODO" 'hi-aquamarine))) ; @TODO Find a better face

(global-hi-lock-mode)
(with-eval-after-load 'hi-lock
  (diminish 'hi-lock-mode))

;;;;; LSP

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-f")
    (with-maybe-region
     thblt/lsp-format lsp-format-region lsp-format-buffer)))

;;;;; Outline, hideshow, bicycle

;; @FIXME Need to simulate Outshine behavior of generating Outline
;; defs given a comment-delimiter.  It's easy enough, eg Haskell:
;;
;; (setq outline-regexp (rx (* space) "-- " (group (one-or-more "*")) (* any))
;;      outline-level (lambda () (length (match-string 1))))
;;
;; See the default impl of outline-level before overriding it.

(add-hook 'prog-mode-hook 'outline-minor-mode)
;; (remove-hook 'prog-mode-hook 'hs-minor-mode)

(defun thblt/bicycle-tab ()
  "If point is on an outline heading, run `bicycle-cycle'.
Otherwise, disable bicycle-tab and reemit binding."
  (interactive)
  (if (outline-on-heading-p)
      (call-interactively 'bicycle-cycle)
    (let ((outline-minor-mode nil))
      (call-interactively (key-binding (kbd "TAB"))))))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
  (define-key outline-minor-mode-map (kbd "C-M-<tab>") 'bicycle-cycle-global)
  (define-key outline-minor-mode-map (kbd "<tab>") 'thblt/bicycle-tab)
  (advice-add 'outline-flag-region :after 'backline-update)
  (add-hook 'outline-minor-mode-hook
            'outline-minor-faces-add-font-lock-keywords)
  (diminish 'outline-minor-mode))

(with-eval-after-load 'hideshow
  (diminish 'hs-minor-mode))

(add-hook 'outline-mode-hook 'reveal-mode)
(diminish 'reveal-mode)

;;;;;; Outline heading generator (outshine-style)

(defun thblt/outline-configure (delimiter)
  "Configure Outshine-style headings for the current major mode."
  (interactive (list (read-from-minibuffer "Delimiter: " comment-start)))
  (setq outline-regexp
        ;;; FIXME Add support for lisp-style
        (rx (literal delimiter) (zero-or-more space) "*" (group (zero-or-more "*")))
        outline-heading-alist
        (mapcar
         (lambda (n) (cons (format "%s *%s" delimiter (make-string n ?*)) n))
         (number-sequence 0 15))
        outline-level (lambda () (length (match-string 1)))))

;;;;; Rainbow mode

(diminish 'rainbow-mode)

;;;; Programming languages

;;;;; C and friends

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'c-mode-hook
          (lambda ()
            (thblt/outline-configure "// ")))

(add-hook 'c++-mode-hook
          (lambda ()
            (thblt/outline-configure "// ")))

;;;;; Elisp

(require 'auto-compile)

(setq auto-compile-display-buffer nil)

(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

;;;;; Haskell

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-interactive-popup-errors nil
      haskell-svg-render-images t
      ;; This vvv is required for that ^^^.
      ;;; See the texi version of the manual in the source tree.
      haskell-interactive-mode-eval-mode 'fundamental-mode)

(add-hook 'haskell-mode-hook (lambda ()
                               (thblt/outline-configure "-- ")))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-f") 'haskell-mode-stylish-buffer))

;;;;; Python

(add-hook 'python-mode-hook
          (lambda ()
            (thblt/outline-configure "# ")))

;;; Tools

;; This section deals with tools that aren't editors.

;;;; Borg

;;;;; Helpers

(defun thblt/borg-fix-branch-declarations ()
  "Verify that all Borg drones have a branch declared in .gitmodules."
  (interactive)
  (let ((errors))
    (mapc (lambda (drone)
            (unless (borg-get drone "branch")
              (add-to-list 'errors drone)
              (call-process "git" nil nil nil
                            "config" "-f" borg-gitmodules-file "--add" (format "submodule.%s.branch" drone) "master")))
          (borg-drones))
    (when errors
      (borg--sort-submodule-sections borg-gitmodules-file)
      (message "These modules were updated: %s" errors))))

(defun thblt/borg-drones-track-upstream ()
  "Make all Borg drones track the branch they're configured to."
  (interactive)
  (require 'magit-git)
  (dolist (drone (borg-drones))
    (let* ((default-directory (borg-worktree drone))
           (current-branch (magit-git-string "branch" "--show-current"))
           (remote-branch (borg-get drone "branch"))
           (local-branch (or (borg-get drone "local-branch") remote-branch))
           (enable-dir-local-variables nil))
      (if (null remote-branch)
          (warn "No remote branch configured for drone %s, ignoring." drone)
        (unless (string= current-branch local-branch)
          (message "Switching to `%s' on %s (was `%s') " local-branch drone current-branch)
          (magit-git "branch" "-f" local-branch "HEAD")
          (magit-git "checkout" local-branch))
        (magit-git "branch" local-branch "--set-upstream-to" (format "%s/%s" "origin" remote-branch))))))

(defun thblt/borg-check-urls ()
  "Verify that all Borg drones remote URLs begin with http."
  (interactive)
  (mapc (lambda (drone)
          (let ((url (borg-get drone "url")))
            (unless (string-prefix-p "http" url)
              (message "Bad remote URL on %s: %s" drone url))))
        (borg-drones)))

;;;;; Projectile integration

(defun thblt/borg-projectile-update (clone &rest _)
  "Make Projectile discover Borg drone CLONE."
  (projectile-add-known-project (borg-worktree clone)))

(advice-add 'borg-clone :after 'thblt/borg-projectile-update)
(advice-add 'borg-assimilate :after 'thblt/borg-projectile-update)
(advice-add 'borg-remove :after (lambda (&rest _) (projectile-cleanup-known-projects)))

;;;; Divine

(require 'divine)
(divine-global-mode)

(defun divine-reload ()
  "Force reload Divine."
  (interactive)
  (dolist (module (list "divine-core.el" "divine-commands.el" "divine.el"))
    (load (expand-file-name module (borg-worktree "divine")))))

(defhydra hydra-smartparens (:hint nil :color pink)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ("0" digit-argument)
  ("1" digit-argument)
  ("2" digit-argument)
  ("3" digit-argument)
  ("4" digit-argument)
  ("5" digit-argument)
  ("6" digit-argument)
  ("7" digit-argument)
  ("8" digit-argument)
  ("9" digit-argument)

  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

(add-hook 'smartparens-mode-hook
          (lambda ()
            (unless (minibufferp)
              (local-set-key (kbd "M-o") 'hydra-smartparens/body))))

;;;; ERC

(eval-when-compile
  (require 'erc)
  (require 'erc-track))

(setq erc-server-auto-reconnect t
      erc-kill-buffer-on-part t

      erc-lurker-hide-list '("JOIN" "PART" "QUIT")
      erc-lurker-threshold-time 900 ; 15mn

      erc-track-exclude-server-buffer t

      erc-hl-nicks-skip-nicks '("thblt")

      erc-header-line-format nil

      erc-kill-server-buffer-on-quit t
      erc-kill-queries-on-quit t)

(advice-add 'load-theme :after (lambda (&rest _) (if (fboundp 'erc-hl-nicks-refresh-colors) (erc-hl-nicks-refresh-colors))))

(add-hook 'erc-mode-hook (lambda ()
                           (erc-fill-mode -1)
                           (visual-line-mode 1)
                           (setq-local wrap-prefix "  ")))

(defun znc ()
  "Connect to ZNC."
  (interactive)
  (if-let ((pwfunc (plist-get
                    (car
                     (auth-source-search
                      :max 1
                      :host "znc.thb.lt"))
                    :secret)))
      (erc-tls
       :server "k9.thb.lt"
       :port 2002
       :nick "thblt"
       :password (format "%s:%s" "thblt" (funcall pwfunc)))
    (auth-source-forget-all-cached)
    (message "No password, maybe retry (I've cleared auth-source-cache)")))


;;;; Magit and Git

(defhydra hydra-magit-launcher (:color blue)
  ("g" thblt/magit-status "Status")
  ("C-g" thblt/magit-status)
  ("l" magit-list-repositories "List repos")
  ("c" magit-clone "Clone")
  ("i" magit-init "Init")
  ("f" forge-list-repositories))

(define-key global-map (kbd "C-x g") 'thblt/magit-status)
(define-key global-map [remap magit-status] 'thblt/magit-status)
(define-key global-map (kbd "C-x C-g") 'hydra-magit-launcher/body)

(with-eval-after-load 'magit
  (require 'forge))

;;;;; Steal repository list from Projectile

;; All Projectile projects will be git directories, so it's kinda
;; pointless to maintain two separate lists.  We instrument Magit to
;; steal projectile-known-projects every time it makes sense.

(defun thblt/magit-repos-from-projectile (&rest _)
  "Overwrite `magit-repository-directories' with `projectile-known-projects'."
  (setq magit-repository-directories
        (mapcar (lambda (p) (cons p 0))
                projectile-known-projects)))

(advice-add 'magit-list-repositories :before 'thblt/magit-repos-from-projectile)

(defun thblt/magit-status (&optional arg)
  "Run Magit status using Projectile as the source of repository completion."
  (interactive "P")
  (require 'magit)
  (or (and (not arg) (magit-toplevel) (magit-status-setup-buffer))
      (projectile-switch-project "v")))

;;;;; magit-list-repositories

;; =magit-list-repositories= provides a summary view of multiple
;; repositories.
;;
;; First, let's configure the view.

(setq magit-repolist-columns
      '(("Name"       25  magit-repolist-column-ident nil)
        ("Branch"     16  magit-repolist-column-branch)
        ("Version" 25  magit-repolist-column-version nil)
        ("Upstream"   15  magit-repolist-column-upstream)
        ("↓U"         5   magit-repolist-column-unpulled-from-upstream)
        ("↑U"         5   magit-repolist-column-unpushed-to-upstream)
        ("↓P"         5   magit-repolist-column-unpulled-from-pushremote)
        ("↑P"         5   magit-repolist-column-unpushed-to-pushremote)
        (""           6   magit-repolist-column-flag)
        ("Path"       99  magit-repolist-column-path nil)))

;;;; Misc utilities

(define-key global-map (kbd "C-x C-p") 'proced)

(define-key global-map (kbd "<f12>") 'sunrise)
(setq dired-omit-files "^\\.")

(defalias 'tdoe 'toggle-debug-on-error)

;;;; Notmuch

(eval-when-compile
  (require 'notmuch)
  (require 'smtpmail))

(define-key global-map (kbd "M-<f12>") 'notmuch)

(setq send-mail-function 'smtpmail-send-it
      notmuch-saved-searches
      '((:name "Inbox" :query "tag:inbox" :key "i")
        (:name "Inbox (unread)" :query "tag:inbox and tag:unread" :key "I")
        (:name "Sent" :query "tag:sent" :key "s")
        (:name "Drafts" :query "tag:draft" :key "d")
        (:name "Lost" :query "tag:lost" :key "l"))
      notmuch-fcc-dirs
      '(("thibault@thb.lt" . "thb.lt/Sent +thb.lt +sent -inbox ")
        ("thibault.polge@ac-amiens.fr" . "ac-amiens/Sent +ac-amiens +sent -inbox ")))

(defun thblt/smtpconfig-ac-amiens.fr ()
  "SMTP settings for ac-amiens.fr."
  (setq smtpmail-smtp-server  "smtp.ac-amiens.fr"
        smtpmail-smtp-user    "tpolge"
        smtpmail-stream-type  'tls
        smtpmail-smtp-service 465))

(defun thblt/smtpconfig-thb.lt ()
  "SMTP settings for thb.lt."
  (setq smtpmail-smtp-server  "ssl0.ovh.net"
        smtpmail-smtp-user    "thibault@thb.lt"
        smtpmail-stream-type  'tls
        smtpmail-smtp-service 465))

(defun thblt/message-send-configure-smtp ()
  "Pick SMTP server by From field."
  (pcase (message-field-value "From")
    ((rx "thb.lt>" eol) (thblt/smtpconfig-thb.lt))
    ((rx "ac-amiens.fr>" eol) (thblt/smtpconfig-ac-amiens.fr))
    (_ (error "I don't know how to configure SMTP here"))))

(add-hook 'message-send-hook 'thblt/message-send-configure-smtp)

;;;; Password management (password-store)

(auth-source-pass-enable)

;;;; PDF Tools

(eval-when-compile
  (require 'tex)
  (require 'pdf-misc)
  (require 'pdf-tools))

(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
(autoload 'pdf-view-mode "pdf-tools")

(setq pdf-misc-print-program (executable-find "lpr")
      pdf-misc-print-program-args '("-o media=A4" "-o fitplot"))

(with-eval-after-load 'pdf-view
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode)))

  (define-key pdf-view-mode-map (kbd "s a") 'pdf-view-auto-slice-minor-mode)
  (define-key pdf-view-mode-map (kbd "s r") 'pdf-view-reset-slice)
  (define-key pdf-view-mode-map (kbd "V") 'thblt/open-pdf-externally))

(with-eval-after-load 'tex
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
    (add-to-list 'TeX-view-program-list-builtin
                 '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools")))

(defvar thblt/external-pdf-viewer (executable-find "evince")
  "External viewer for PDF files.")

(defun thblt/open-pdf-externally (file)
  "Open FILE in `thblt/external-pdf-viewer'.

Interactively, use buffer-file-name."
  (interactive `(,(or buffer-file-name
                      (error "Need a file!"))))
  (start-process "PDF viewer" nil thblt/external-pdf-viewer file))

t;;;; Regular expression builder

(eval-when-compile
  (require 're-builder))

(setq reb-re-syntax 'string)

;;;; scpaste

;; Technomancy's scpaste is a replacement for pastebin,
;; paste.lisp.org, and similar services.  It generates a HTML page out
;; of a buffer or region and publishes it using scp.

(eval-when-compile
  (require 'scpaste))

(setq scpaste-scp-destination "thblt@k9.thb.lt:/var/www/paste.thb.lt/"
      scpaste-http-destination "https://paste.thb.lt"
      scpaste-user-address "https://thb.lt"
      scpaste-make-name-function 'scpaste-make-name-from-timestamp)

(defun thblt/scpaste-without-noise (f &rest args)
  "Disable visual helpers before calling F with ARGS.

  This is meant to be an advice.

  A lot of packages add overlays which are useful when editing,
  noisy when reading.  We advise scpaste so a few minor modes get
  disabled before it runs, and restored afterwards."
  (let ((tmm transient-mark-mode)
        ;; (hig (bound-and-true-p highlight-indent-guides-mode))
        (flyc (bound-and-true-p flycheck-mode))
        (flys (bound-and-true-p flyspell-mode))
        (ssp (bound-and-true-p show-smartparens-mode)))
    (when tmm
      (transient-mark-mode 0))
    ;; (when hig
    ;;   (highlight-indent-guides-mode -1))
    (when flyc
      (flycheck-mode -1))
    (flyspell-mode -1)
    (when ssp
      (show-smartparens-mode -1))
    (apply f args) ; Run wrapped function
    (when tmm
      (transient-mark-mode 1))
    ;; (when hig
    ;;   (highlight-indent-guides-mode 1))
    (when flyc
      (flycheck-mode 1))
    (when flys
      (flyspell-mode 1))
    (when ssp
      (show-smartparens-mode 1))))

(advice-add 'scpaste :around 'thblt/scpaste-without-noise)
(advice-add 'scpaste-region :around 'thblt/scpaste-without-noise)

(with-eval-after-load 'scpaste
  (defun scpaste-footer (&rest _) ""))

;;; Conclusion

;;;; Server configuration

;; I don't explicitly run the server, but I start a new daemon
;; whenever I need one.  With a swarm of instances, updating config
;; may be a pain.  These two functions respectively reload =init.el=
;; and tell all daemons to do so:

(defun thblt/reload-emacs ()
  "Reload Emacs configuration."
  (interactive)
  (load user-init-file))

(defun thblt/reload-all-emacsen ()
  "Execute `thblt/reload-emacs' on all servers."
  (interactive)
  (dolist (instance (directory-files server-socket-dir nil (rx bol (not "."))))
    (unless (equal instance server-name)
      (async-shell-command (format "emacsclient -s %s --eval \"(thblt/reload-emacs)\"" instance))))
  (thblt/reload-emacs))

(defun thblt/server-start (name)
  "Prompt for NAME, then start the Emacs server under that name."
  (interactive "sDaemon name? ")
  (setq server-name name)
  (server-start))

;;;; Report success

;; We finally set the initial contents of the scratch buffer.  This
;; makes it easy to notice when something went wrong (this may not be
;; obvious in daemon mode)
(setq initial-scratch-message
      (concat
       ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n"
       ";; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n"
       ";; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n"))

(thblt/resurrect-scratch t)

(thblt/light-theme)

;; Restore GC settings.
(setq gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value))
      gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
      garbage-collection-messages nil)

(message "Reached the end of init.el.")
