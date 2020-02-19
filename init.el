;;; Introduction

;; This chapter deals with the general use of Emacs, and is limited to
;; general settings and sane defaults.  It's a bit messy, since it's
;; mostly made up of all the bits that don't fit anywhere else.

;;;; First things first

;; Never load bytecode if .el is more recent(
(setq load-prefer-newer t)

;;;;; Package managers

;;;;;; package.el

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			                   ("melpa-stable" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-archive-priorities '(("gnu"          . 100)
                                   ("melpa"        . 50)
                                   ("melpa-stable" . 10)))

;;;;;; Borg

;; Borg comes second, because it comes first.  The second initialized
;; manager will be the first in load-path.

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;;;;; Fundamentals

;; Recompile what needs recompiling
(require 'auto-compile)
(auto-compile-on-load-mode)

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
      ;; Don't lose the contents of system clipboard when killing from Emacs:
      save-interprogram-paste-before-kill t
      ;; Disable Customize by pointing it to =/dev/null=:
      custom-file "/dev/null"

      inhibit-compacting-font-caches (eq 'system-type 'windows-nt) ; This prevents slowdown when using strange characters.

      ;; Use default browser from the system. Using =setsid xdg-open=
      ;; prevents Emacs from killing xdg-open before it actually opened
      ;; anything. See
      ;; [[https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open][here]].
      browse-url-browser-function 'browse-url-generic
      ;; browse-url-generic-program "setsid"
      browse-url-generic-program "firefox")
;; browse-url-generic-args '("xdg-open"))

(setq-default  major-mode 'text-mode)

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-i] [C-i])

(load custom-file t)

;;;; MELPA and package.el

;;; User interface

;;;; Settings and general configuration

(setq-default
 enable-recursive-minibuffers t
 inhibit-startup-screen t
 use-dialog-box nil
 vc-follow-symlinks t

 truncate-lines t

 disabled-command-function nil)

;; Cursor configuration
(setq-default  cursor-type 'box)
(defun thblt/update-cursor-color ()
  (set-cursor-color (if overwrite-mode "#ff0000" "#ffffff")))
(add-hook 'overwrite-mode-hook 'thblt/update-cursor-color)
(thblt/update-cursor-color)
(blink-cursor-mode)
                                        ; @FIXME Set color per-buffer

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

;; Fringes are pointless (by default)
(fringe-mode 0)

;;;; Fonts and themes

;; Configure the default font:

;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 10"))
(add-to-list 'default-frame-alist '(font . "Iosevka 8"))
(set-face-attribute 'default nil
                    :height 080)

(setq eziam-color-comments t
      eziam-scale-headings t
      eziam-heading-style 'gray-blocks)

;; Create some shortcut commands to load the Eziam themes:
(defun thblt/disable-all-themes ()
  (interactive)
  (mapc 'disable-theme custom-enabled-themes))

(defun eziam-dark () (interactive) (thblt/disable-all-themes) (load-theme 'eziam-dark t) (load-theme 'eziam-dark-line t) (when (fboundp 'pl/reset-cache) (pl/reset-cache)))
(defun eziam-light () (interactive) (thblt/disable-all-themes) (load-theme 'eziam-light t) (load-theme 'eziam-light-line t) (pl/reset-cache))

(eziam-line-install)
(eziam-dark)

;;;; Projectile

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(counsel-projectile-mode)

;; - globally ignore undo-files and similar byproducts.
(setq projectile-globally-ignored-file-suffixes (append '(
                                                          ".un~"
                                                          ".~undo-tree~")
                                                        projectile-globally-ignored-files))

(diminish 'projectile-mode)

;; I prefer to treat submodules as separate projects, so don't include
;; them in the main file listing:

(setq projectile-git-submodule-command nil)

;;;; UI Utilities

;;;;; Ace-window

(define-key global-map (kbd "C-x o") 'ace-window)
(define-key global-map (kbd "M-0") 'thblt/switch-to-minibuffer)

;; We use the value of aw-ignored-buffers, so we need the
;; eval-after-load
(with-eval-after-load 'ace-window
  (setq aw-scope 'frame
        aw-background t
        aw-ignore-on t
        aw-ignored-buffers (append aw-ignored-buffers
                                   (mapcar (lambda (n) (format " *Minibuf-%s*" n))
                                           (number-sequence 0 20)))))

(defun thblt/aw-switch-to-numbered-window (number)
  (aw-switch-to-window (nth (- number 1) (aw-window-list))))

(defun thblt/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;;;;; Buffer management (ibuffer)

;; Rebind =C-x C-b= to =ibuffer= instead of =list-buffers=:

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;; Hydra

(setq hydra-hint-display-type 'message)

;;;;; Ivy

(setq ivy-use-virtual-buffers t)
(ivy-mode)

(define-key global-map (kbd "M-i") 'counsel-imenu)
(define-key global-map (kbd "M-x") 'counsel-M-x)
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)
(define-key global-map (kbd "C-S-s") 'swiper)
(define-key global-map (kbd "C-x 8 RET") 'counsel-unicode-char)

(diminish 'ivy-mode)

;; (setq ivy-posframe-display-functions-alist
;;       '((t . ivy-posframe-display-at-frame-center)))

;; (ivy-posframe-mode 1)
;; (diminish 'ivy-posframe-mode)

;;;;; Shackle

;; Stealing rules from wasamasa's config
(setq shackle-rules
      '(("*Help*" :align t :select t)
        ("\\`\\*magit.*?\\*\\'" :regexp t :frame nil)
        (flycheck-error-list-mode :select t)
        ((compilation-mode "\\`\\*firestarter\\*\\'"
                           "\\`\\*magit-diff: .*?\\'") :regexp t :noselect t)
        ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))
      shackle-default-rule '(:select t :frame nil)
      shackle-default-size 0.4
      shackle-inhibit-window-quit-on-same-windows t)

(shackle-mode)

;;;;; Windmove

(setq windmove-wrap-around t)

(define-key global-map (kbd "S-<up>") 'windmove-up)
(define-key global-map (kbd "S-<left>") 'windmove-left)
(define-key global-map (kbd "S-<right>") 'windmove-right)
(define-key global-map (kbd "S-<down>") 'windmove-down)

;;;;; Winner

(winner-mode)

;;;;; Customization helper

;; A small function to identify the face at point.  Nice to have when
;; writing themes, and faster than C-u C-x =
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;; BÉPO adjustments
;; Unshifted digit argument

(defmacro thblt/digit-argument-with-value (char)
  "Return an anonymous lambda that calls `digit-argument' as if CHAR had been pressed.

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
(define-key universal-argument-map (kbd "0") (thblt/digit-argument-with-value ?0))

;; Beginning and end of buffer

(define-key global-map(kbd "M-ê") 'beginning-of-buffer)
(define-key global-map (kbd "M-Ê") 'end-of-buffer)

;; Fast and efficient killing: q instead of k

(define-key global-map (kbd "C-q") 'kill-line)
(define-key smartparens-mode-map (kbd "C-M-q")  'sp-kill-sexp)

;; Fill and unfill
(define-key global-map (kbd "M-'") 'fill-paragraph)
(define-key global-map (kbd "C-'") 'unfill-paragraph)
                                        ; @FIXME In prog-mode, this could be reformat-defun or something.


;; EXPERIMENTAL Vim-like motion with modifiers

;; (defun k (seq)
;;   (let ((substs '(("left"  . "t")
;;                   ("down"  . "s")
;;                   ("up"    . "r")
;;                   ("right" . "n"))))
;;     (dolist (s substs)
;;       (setq seq (replace-regexp-in-string (format "%%%s%%" (car s)) (cdr s) seq)))
;;     (kbd seq)))

;; (define-key global-map (k "C-b") nil)
;; (define-key global-map (k "C-f") nil)
;; (define-key global-map (k "C-p") nil)
;; (define-key global-map (k "M-d") nil)
;; (define-key global-map (k "M-n") nil)
;; (define-key global-map (k "M-b") nil)
;; (define-key global-map (k "M-f") nil)
;; (define-key global-map (k "M-p") nil)
;; (define-key global-map (k "M-n") nil)
;; (define-key global-map (k "M-v") nil)

;; ;; Motion

;; (define-key global-map (k "C-%left%")             'backward-char)
;; (define-key global-map (k "M-%left%")             'backward-word)
;; (define-key smartparens-mode-map (k "C-M-%left%") 'sp-backward-sexp)
;; (define-key global-map (k "C-%down%")             'next-line)
;; (define-key global-map (k "M-%down%")             nil)
;; (define-key smartparens-mode-map (k "C-M-%down%") nil)
;; (define-key global-map (k "C-%up%")             'previous-line)
;; (define-key global-map (k "M-%up%")             nil)
;; (define-key smartparens-mode-map (k "C-M-%up%") nil)
;; (define-key global-map (k "C-%right%")             'forward-char)
;; (define-key global-map (k "M-%right%")             'forward-word)
;; (define-key smartparens-mode-map (k "C-M-%right%") 'sp-forward-sexp)

;; ;; Isearch

;; (define-key global-map (k "C-v")   'isearch-backward)
;; (define-key global-map (k "C-d")   'isearch-forward)
;; (define-key global-map (k "C-M-d") 'isearch-forward-regexp)
;; (define-key global-map (k "M-d .") 'isearch-forward-symbol-at-point)
;; (define-key global-map (k "M-d o") 'occur)
;; (define-key global-map (k "M-d w") 'isearch-forward-symbol-at-point)
;; (define-key global-map (k "C-M-d") 'isearch-forward-regexp)

;; (define-key isearch-mode-map (k "C-v") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (k "C-d") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (k "C-v") 'isearch-repeat-backward)

;; ;; Killing
;; (define-key global-map (k "C-q") 'delete-char)
;; (define-key global-map (k "M-q") 'kill-word)
;; (define-key text-mode-map (k "C-M-q") 'kill-sentence)
;; (define-key prog-mode-map (k "C-M-q") 'kill-sentence)
;; (define-key global-map (k "C-'") 'kill-line)

;;; Editing text

;; This chapter deals with /general/ text editing.  The next two configure
;; prose and code editing, respectively.

;;;; The editor view hydra

(defmacro thblt/hydra-indicator (desc active)
  "Return DESC with a state indicator determined by ACTIVE.

If at runtime ACTIVE is an unbound symbol it is interpreted as
nil; otherwise it's evaluated normally."
  `(format "[%s] %s" (if ,(if (symbolp active)
                              `(bound-and-true-p ,active)
                            active)
                         (propertize "✔" 'face 'bold)
                       (propertize "-" 'face 'shadow))
           ,desc))

(defhydra hydra-editor-appearance ()
  ("b" text-scale-decrease "Size -" :column "Font and theme")
  ("é" thblt/text-scale-reset (thblt/hydra-indicator "Default size"
                                                     (not (bound-and-true-p text-scale-mode))))
  ("p" text-scale-increase "Size +")
  ("V" variable-pitch-mode (thblt/hydra-indicator "Var. pitch" buffer-face-mode))
  ;; ("t w" (lambda () (interactive) (load-theme 'eziam-white t)) (thblt/hydra-indicator "White theme" (member 'eziam-white custom-enabled-themes)))
  ("t l" eziam-light (thblt/hydra-indicator "Light theme" (member 'eziam-light custom-enabled-themes)))
  ;; ("t u" (lambda () (interactive) (load-theme 'eziam-dusk t)) (thblt/hydra-indicator "Dusk theme" (member 'eziam-dusk custom-enabled-themes)))
  ("t d" eziam-dark (thblt/hydra-indicator "Dark theme" (member 'eziam-dark custom-enabled-themes)))

  ("f" thblt/visual-fill-column-toggle-mode (thblt/hydra-indicator "Visual fill" visual-fill-column-mode) :column "Appearance")
  ("c" thblt/visual-fill-column-toggle-centering (thblt/hydra-indicator "Centering" visual-fill-column-center-text))
  ("g" thblt/visual-fill-column-width-decrease "Width -")
  ("h" thblt/visual-fill-column-width-increase "Width +")
  ("l" visual-line-mode (thblt/hydra-indicator "Line wrap" visual-line-mode))
  ("-" toggle-word-wrap (thblt/hydra-indicator "Word wrap" word-wrap))

  ("v d" rainbow-delimiters-mode (thblt/hydra-indicator "Rainbow delimiters" rainbow-delimiters-mode) :column "Helpers")
  ("v r" rainbow-mode (thblt/hydra-indicator "Rainbow" rainbow-mode))
  ("v i" highlight-indent-guides-mode (thblt/hydra-indicator "Highlight indent" highlight-indent-guides-mode))

  ("W" superword-mode (thblt/hydra-indicator "super-word" superword-mode))
  ("w" subword-mode (thblt/hydra-indicator "SubWord" subword-mode))

  ("a" auto-fill-mode (thblt/hydra-indicator "Auto fill" auto-fill-function) :column "Elecricity")
  ("A" refill-mode (thblt/hydra-indicator "Auto refill" refill-mode))
  ("I" aggressive-indent-mode (thblt/hydra-indicator "Aggressive indent" aggressive-indent-mode))

  ("!" flycheck-mode (thblt/hydra-indicator "Code" flycheck-mode) :column "Utility")
  ("?" flyspell-mode  (thblt/hydra-indicator "Spell" flyspell-mode))
  ("F" thblt/ispell-use-french (thblt/hydra-indicator "Français" (string= (bound-and-true-p ispell-local-dictionary) "french")))
  ("E" thblt/ispell-use-english (thblt/hydra-indicator "English" (string= (bound-and-true-p ispell-local-dictionary) "english")))

  ("R" auto-revert-mode (thblt/hydra-indicator "Auto-revert" auto-revert-mode) :column "Misc"))

(define-key global-map (kbd "C-c l") 'hydra-editor-appearance/body)

(defun thblt/visual-fill-column-reset (&optional activate)
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (when (or
         activate
         visual-fill-column-mode)
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
  (setq visual-fill-column-center-text (not visual-fill-column-center-text))
  (thblt/visual-fill-column-reset t))

(defun thblt/visual-fill-column-width-adjust (delta)
  "For use by `hydra-editor-appearance/body'."
  (interactive)
  (setq visual-fill-column-width (+ delta
                                    (or
                                     visual-fill-column-width
                                     fill-column)))
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

(setq ispell-program-name "aspell"
      ispell-silently-savep t)

;; Enable Flyspell:
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))
(diminish 'flyspell-mode "Fly")

;; Correct words using Ivy instead of default method:

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "M-$") 'flyspell-auto-correct-previous-word)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

;;;; Moving around

(define-key global-map (kbd "M-\"") 'backward-paragraph)
(define-key global-map (kbd "M-«") 'forward-paragraph)

(define-key global-map (kbd "C-M-\"") 'beginning-of-defun)
(define-key global-map (kbd "C-M-«") 'end-of-defun)

;;;;; avy

(define-key global-map (kbd "C-:") 'avy-goto-char-timer)
(define-key global-map (kbd "M-g f") 'avy-goto-line)

;;;;; beginend

(beginend-global-mode)

(mapc (lambda (m) (diminish (cdr m)))
      beginend-modes)
(diminish 'beginend-global-mode)

;;;;; mwim

(define-key global-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key global-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key global-map (kbd "<home>") 'mwim-beginning-of-line-or-code)
(define-key global-map (kbd "<end>") 'mwim-end-of-line-or-code)

;;;;; nav-flash (don't get lost)

(face-spec-set 'nav-flash-face '((t (:inherit pulse-highlight-face :extend t))))
(advice-add 'recenter-top-bottom :after (lambda (x) (nav-flash-show)))

;;;; Replace

(define-key global-map (kbd "C-M-%") 'vr/query-replace)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;;;; Minor modes

;;;;; Auto-revert-mode

(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode " 🔃"))

;;;;; Move text

;; Move lines of text with =M-<up>= and =M-<down>=.
(move-text-default-bindings)
;; Configured with Outshine to DWIM on Outshine headings

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

(yas-global-mode)
(diminish 'yas-minor-mode)

;;;; Misc

(setq shift-select-mode nil)

;; From https://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun kill-duplicate-blank-lines ()
  (interactive)
  (let ((regexp (rx bol (* whitespace) eol)))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward regexp nil t)
        (forward-line 1)
        (while (looking-at-p regexp)
          (kill-whole-line 1)))))) ; t because kill-line returns nil

;;;;; Bindings

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-y") 'copy-line)
(global-set-key (kbd "C-h") 'delete-backward-char)

(define-key global-map (kbd "M-Q") 'unfill-paragraph)



;;;;; Autosave when losing focus

(super-save-mode +1)
(diminish 'super-save-mode)

;;;;; Delete trailing whitespace when saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Writing prose

;; This chapter deals with two things:

;;  1. Major modes dedicated to writing prose, as opposed to code or
;;     configuration.
;;  2. Non-code bits in code/configuration files: comments and integrated
;;     documentation.n

;;;; Common settings and minor modes
;;;;; Abbrev

(add-hook 'text-mode-hook (lambda () (abbrev-mode t)))
(diminish 'abbrev-mode)

;;;;; Wordwrap/visual line/visual-fill-column

(diminish 'visual-line-mode)

;;;; Major modes

;;;;; AucTex

(require 'tex-site nil t) ; I don't build this on Windows.

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

;;;;; Org-Mode

(setq org-catch-invisible-edits t
      org-hide-leading-stars t
      org-hide-emphasis-markers nil
      org-html-htmlize-output-type 'css
      org-imenu-depth 8
		  org-src-fontify-natively t
      org-use-speed-commands t
		  org-ellipsis " ▼"
		  org-special-ctrl-a/e t
		  org-special-ctrl-k t)

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode t)
                           (visual-line-mode t)
                           (which-function-mode t)))

;; Use shift-arrow for window navigation when not on an heading
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))4

;;;;;; Pairs

(defun sp--org-skip-asterisk (ms mb me)
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

;;;;;;; Export

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(setq org-latex-pdf-process (list "latexmk -CA %f" "latexmk -f -pdfxe -xelatex %f"))
(setq org-latex-pdf-process (list "latexmk -f -pdfxe -xelatex %f"))

;; Identify position in buffer:

(defun thblt/org-where-am-i ()
  "Return a string of headers indicating where point is in the current tree."
  (interactive)
  (let (headers)
    (save-excursion
	    (while (condition-case nil
                 (progn
                   (push (nth 4 (org-heading-components)) headers)
                   (outline-up-heading 1))
		           (error nil))))
    (message (mapconcat #'identity headers " > "))))

(define-key org-mode-map (kbd  "<f1> <f1>") 'thblt/org-where-am-i)
(define-key org-mode-map (kbd  "C-'") nil)

;;;;;; Org-agenda:

(setq org-agenda-files (list "~/Documents/LOG.org")
	    org-default-notes-file "~/Documents/LOG.org")

;;;;;; Org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (shell . t)))

;;;;;; Stuff

(defun thblt/org-insert-magic-link (url)
  (interactive "sLink to? ")
  (require 'url-util)
  (let ((title))
    (cond
     ((string-prefix-p "https://fr.wikipedia.org/wiki/" url)
      (setq title (decode-coding-string (url-unhex-string (substring url 30)) 'utf-8)))
     (t (error "I have no idea what to do with this")))
    (org-insert-link nil url title)))

;;; Writing code
;;;; Settings

;; Some basic settings...

(setq-default comment-empty-lines nil
		          tab-width 2
		          c-basic-offset 2
		          cperl-indent-level 2
		          indent-tabs-mode nil)

;; and a few mappings.

(defvar-local thblt/compile-command nil)

(defvar thblt/default-compile-command-alist
  '((haskell-mode . "stack build")
    (c-mode       . "cmake")))

;; TODO

(global-set-key (kbd "<f8>") 'ffap)

;;;; Reformatting

(defvar-local format-buffer-function nil
  "The function used to reformat this buffer")

(defun format-buffer (buffer)
  "Reformat BUFFER.

Interactively, work on active buffer"
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (if format-buffer-function
        (funcall format-buffer-function)
	    (message "Please set `format-buffer-function' first!"))))

;;;; Minor modes
;;;;; Aggressive indent

(with-eval-after-load 'aggressive-indent-mode
  (diminish 'aggressive-indent-mode "⭾"))

(dolist (mode '(elisp-mode lisp-mode))
  (add-hook mode 'aggressive-indent-mode))

;;;;; Color-identifiers

(add-hook 'prog-mode-hook 'color-identifiers-mode)

(with-eval-after-load 'color-identifiers-mode
  (advice-add 'load-theme :after (lambda (&rest _)
                                   (color-identifiers:regenerate-colors)
                                   (color-identifiers:refresh)))
  (add-to-list
   'color-identifiers:modes-alist'
   `(haskell-mode . ("[^.][[:space:]]*"
                     "\\_<\\([[:lower:][:upper:]]\\([_]??[[:lower:][:upper:]\\$0-9]+\\)*\\(_+[#:<=>@!%&*+/?\\\\^|~-]+\\|_\\)?\\)"
                     (nil scala-font-lock:var-face font-lock-variable-name-face))))

  (diminish 'color-identifiers-mode))

;;;;; Company

(add-hook 'prog-mode-hook 'company-mode)

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-TAB") 'company-complete-common)
  (diminish 'company-mode ""))

;; (add-hook 'after-make-frame-functions (lambda (_) (interactive) (company-posframe-mode 1)))

;; (with-eval-after-load 'company-posframe
;;   (diminish 'company-posframe-mode))

;;;;; Evil Nerd Commenter

(dolist (map (list org-mode-map prog-mode-map))
  (define-key map (kbd "M-,") 'evilnc-comment-or-uncomment-lines)
  (define-key map (kbd "C-M-,") 'evilnc-comment-or-uncomment-paragraphs))

(with-eval-after-load 'auctex
  (dolist (map (list latex-mode-map LaTeX-mode-map  tex-mode-map))
    (define-key map (kbd "M-,") 'evilnc-comment-or-uncomment-lines)
    (define-key map (kbd "C-M-,") 'evilnc-comment-or-uncomment-paragraphs)))



;;;;; Flycheck

;; (add-hook 'prog-mode-hook 'flycheck-mode)

;; (with-eval-after-load 'flycheck
;; (diminish 'flycheck-mode "▲"))

;;;;; Outline and Outshine

(provide 'outorg) ; Dirty

(setq outshine-use-speed-commands t)

(add-hook 'prog-mode-hook 'outshine-mode)
(add-hook 'haskell-mode-hook (lambda () (setq-local outshine-preserve-delimiter-whitespace t)))
(diminish 'outline-minor-mode)

(defun thblt/m-up-dwim () (interactive)
       (cond ((and outshine-mode (outline-on-heading-p))
		          (call-interactively 'outline-move-subtree-up))
             (t (call-interactively 'move-text-up))))

(defun thblt/m-down-dwim () (interactive)
       (cond ((and outshine-mode (outline-on-heading-p))
		          (call-interactively 'outline-move-subtree-down))
             (t (call-interactively 'move-text-down))))

(with-eval-after-load 'outshine
  (diminish 'outshine-mode)
  (define-key outshine-mode-map (kbd "M-<up>") 'thblt/m-up-dwim)
  (define-key outshine-mode-map (kbd "M-<down>") 'thblt/m-down-dwim)
  (define-key outshine-mode-map (kbd "M-TAB") nil)
  (define-key outshine-mode-map (kbd "C-M-TAB") 'outshine-cycle-buffer))

;;;;; Rainbow mode

;; Rainbow mode is similar to Atom's Pigments plugin or something.
(add-hook 'prog-mode-hook (rainbow-mode))
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(diminish 'rainbow-mode)

;;;; Programming languages

;;;;; C and friends

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'c-mode-common-hook (lambda nil (setq-local
                                           format-buffer-function 'clang-format)))

;;;;; Elisp

(add-hook 'lisp-mode-hook 'aggressive-indent-mode)

;;;;; Haskell

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;;;; Nix

(with-eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook (lambda nil (setq format-buffer-function 'nix-mode-format))))

;;; Tools

;; This section deals with tools that aren't editors.

;;;; Borg

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
                                        ; (sort errors 'string<)
	    (message "These modules were updated: %s" errors))))

(defun thblt/borg-check-urls ()
  "Verify that all Borg drones remote URLs begin with http."
  (interactive)
  (mapc (lambda (drone)
          (let ((url (borg-get drone "url")))
            (unless (string-prefix-p "http" url)
		          (message "Bad remote URL on %s: %s" drone url))))
        (borg-drones)))

;;;; ERC

(setq  erc-server-auto-reconnect t
	     erc-kill-buffer-on-part t

	     erc-lurker-hide-list '("JOIN" "PART" "QUIT")
	     erc-lurker-threshold-time 900 ; 15mn

       erc-hl-nicks-skip-nicks '("thblt")

	     erc-header-line-format nil)

(add-hook 'erc-mode-hook (lambda ()
                           (visual-line-mode)
                           (erc-hl-nicks-mode)
                           (erc-fill-disable)))

(advice-add 'load-theme :after (lambda (&rest _) (when (functionp 'erc-hl-nicks-reset-face-table)
                                                   (erc-hl-nicks-reset-face-table))))

;; ZNC doesn't know how to use auth-source, and I'm too lazy to
;; implement it.  Instead, this function will initialize znc-servers
;; on first start, reading the password from auth-source.

(defun znc ()
  "A quick and dirty function to read ZNC password before connecting"
  (interactive)
  (require 'znc)
  (unless znc-servers
    (--if-let (plist-get
               (car
                (auth-source-search
                 :max 1
                 :host "znc.thb.lt"))
               :secret)
        (setq znc-servers
              `(("k9.thb.lt" 2002 t
                 ((freenode "thblt" ,(funcall it)
                            )))))
      (message "Cannot read ZNC secret!")))
  (when znc-servers
    (call-interactively 'znc-all)))

;;;; Magit and Git

(define-key global-map (kbd "C-x g") 'magit-status)

;; Use Projectile projects as a source of repositories:

(defun thblt/update-magit-repository-directories (&rest _)
  (setq magit-repository-directories
        (-non-nil
         (mapcar (lambda (x)
                   (unless (string-prefix-p borg-drone-directory (expand-file-name x))
                     (cons x 0)))
                 projectile-known-projects))))

(advice-add 'magit-status :before 'thblt/update-magit-repository-directories)
(advice-add 'magit-list-repositories :before 'thblt/update-magit-repository-directories)

;;;;; magit-list-repositories

;; =magit-list-repositories= provides a summary view of multiple
;; repositories.
;;
;; First, let's configure the view.

(setq magit-repolist-columns
	    '(
        ("Name"       25  magit-repolist-column-ident nil)
        ("Branch"     16  magit-repolist-column-branch)
        ("Version" 25  magit-repolist-column-version nil)
        ("Upstream"   15  magit-repolist-column-upstream)
        ("↓U"         5   magit-repolist-column-unpulled-from-upstream)
        ("↑U"         5   magit-repolist-column-unpushed-to-upstream)
        ("↓P"         5   magit-repolist-column-unpulled-from-pushremote)
        ("↑P"         5   magit-repolist-column-unpushed-to-pushremote)
        (""           6   magit-repolist-column-dirty)
        ("Path"       99  magit-repolist-column-path nil)))

;; An extra feature: update all remotes.  Probably very dirty.

(require 'magit-repos)

(defun thblt/magit-repolist-fetch-all ()
  "@TODO Add documentation"
  (interactive)
  (mapc (lambda (d)
          (shell-command
           (format "git -C %s fetch --all &"
                   (shell-quote-argument
                    (expand-file-name (car d))))))
        magit-repository-directories))

(define-key magit-repolist-mode-map (kbd "G") 'thblt/magit-repolist-fetch-all)

;;;; Misc utilities

(defun umount (point)
  "Unmount device at POINT.

Interactively, prompt for an ejectable device."
  (interactive )

  )

;;;; Mu4e

;; Each of my accounts is synced (by =mbsync=) to a folder at the root
;; of the Maildir (eg, =~/.Mail/Academic/=).  We then need a function
;; to switch contexts based on a regular expression on the current
;; Maildir path.  For some reason, this doesn't come included with
;; mu4e, so here it is, and it probably comes
;; [[https://www.reddit.com/r/emacs/comments/47t9ec/share_your_mu4econtext_configs/d0fsih6/][from
;; here]].

(with-eval-after-load 'mu4e

  (require 'mu4e-contrib)

  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
	    (if (listp rx)
          ;; if rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
		          (mu4e-message-maildir-matches msg (cdr rx)))
        ;; not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  ;; Then the bulk of the config:

  (setq mu4e-completing-read-function 'ivy-completing-read

        ;; General settings
        mu4e-mu-binary (expand-file-name "mu/mu" (borg-worktree "mu4e"))
        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        mu4e-change-filenames-when-moving t  ; Required for mbsync
        mu4e-get-mail-command "mbsync -a"
        mu4e-headers-auto-update t
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-maildir "~/.Mail/"
        mu4e-update-interval 60 ;; seconds
        mu4e-sent-messages-behavior 'sent

        ;; Behavior
        mu4e-compose-dont-reply-to-self t

        ;; UI settings
        mu4e-confirm-quit nil
        mu4e-hide-index-messages t
        mu4e-split-view 'vertical
        mu4e-headers-include-related t  ; Include related messages in threads
        mu4e-view-show-images t

        ;; UI symbols
        mu4e-use-fancy-chars t
        mu4e-headers-attach-mark '("A" . "A")
        mu4e-headers-encrypted-mark '("" . "")
        mu4e-headers-flagged-mark '("+" . "⚑")
        mu4e-headers-list-mark '("" . "")
        mu4e-headers-new-mark '("" . "")
        mu4e-headers-read-mark '("" . "")
        mu4e-headers-replied-mark '("" . "↩")
        mu4e-headers-seen-mark '("" . "")
        mu4e-headers-unseen-mark '("" . "")
        mu4e-headers-unread-mark '("" . "✱")
        mu4e-headers-signed-mark '("" . "")
        mu4e-headers-trashed-mark '("T" . "T")

        mu4e-headers-from-or-to-prefix '("" . "→ ")

        mu4e-headers-default-prefix '(" " . " ─")
        mu4e-headers-duplicate-prefix '("D" . "D")
        mu4e-headers-empty-parent-prefix '("X" . " X")
        mu4e-headers-first-child-prefix '("|" . "╰─")
        mu4e-headers-has-child-prefix '("+" . "╰┬")

        mu4e-headers-fields '(
				                      (:flags          . 5)
				                      (:mailing-list   . 18)
				                      (:human-date     . 12)
				                      (:from-or-to     . 25)
				                      (:thread-subject . nil))

        mu4e-user-mail-address-list '("thblt@thb.lt"
					                            "thibault.polge@ac-amiens.fr"
					                            "thibault.polge@ac-orleans-tours.fr"
					                            "thibault.polge@etu.univ-paris1.fr"
					                            "thibault.polge@malix.univ-paris1.fr"
					                            "thibault.polge@univ-paris1.fr"
					                            "thibault@thb.lt"
					                            "tpolge@gmail.com")
        mu4e-refile-folder (lambda (msg)
                             (let ((maildir (mu4e-message-field msg :maildir)))
				                       (message "This is in %s" maildir)
				                       (if (string-suffix-p "/Inbox" maildir)
                                   (concat (substring maildir 0 -5) "Archive")
                                 maildir)))

        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask

        mu4e-contexts `(
                        ,(make-mu4e-context
                          :name "OVH"
                          :enter-func (lambda () (mu4e-message "OVH"))
                          :match-func (lambda (msg)
                                        (when msg
                                          (mu4e-message-maildir-matches msg "^/OVH/")))
                          :vars '(( user-mail-address   . "thibault@thb.lt"  )
                                  ( mu4e-sent-folder        . "/OVH/Sent" )
                                  ( mu4e-drafts-folder      . "/OVH/Drafts" )
                                  ( mu4e-trash-folder       . "/OVH/Trash" )
                                  ;; ( mu4e-refile-folder      . "/OVH/Archive" )
                                  ( smtpmail-local-domain   . "thb.lt" )
                                  ( smtpmail-smtp-server    . "ssl0.ovh.net" )
                                  ( smtpmail-smtp-user      . "thibault@thb.lt" )
                                  ( smtpmail-stream-type    . tls )
                                  ( smtpmail-smtp-service   . 465 )))

                        ,(make-mu4e-context
                          :name "Académie"
                          :enter-func (lambda () (mu4e-message "Académie"))
                          :match-func (lambda (msg)
                                        (when msg
                                          (mu4e-message-maildir-matches msg "^/Académique/")))
                          :vars '(( user-mail-address       . "thibault.polge@ac-amiens.fr"  )
                                  ( mu4e-sent-folder        . "/Académique/Sent" )
                                  ( mu4e-drafts-folder      . "/Académique/Drafts" )
                                  ( mu4e-trash-folder       . "/Académique/Trash" )
                                  ( smtpmail-local-domain   . "ac-amiens.fr" )
                                  ( smtpmail-smtp-server    . "smtp.ac-amiens.fr" )
                                  ( smtpmail-smtp-user      . "tpolge" )
                                  ( smtpmail-stream-type    . tls )
                                  ( smtpmail-smtp-service   . 465 ))))

        mu4e-bookmarks `(("(m:/OVH/INBOX) or (m:/Académique/INBOX) or (m:/Ac/INBOX)"
                          "Global inbox" ?i)
                         ("(m:/OVH/Archive) or (m:/Académique/Archive) or (m:/Ac/Archive)"
                          "Archives" ?a)
                         ("(flag:flagged)" "Flagged" ?f)
                         ("(m:/OVH/Sent) or (m:/Académique/Sent) or (m:/Ac/Sent)"
                          "Sent" ?s)
                         ("(m:/OVH/Drafts) or (m:/Académique/Drafts) or (m:/Ac/Drafts)"
                          "Drafts"       ?d))
        )

  (add-hook 'mu4e-view-mode-hook (lambda ()
                                   (setq visual-fill-column-width 80)
                                   (visual-line-mode 1)
                                   (visual-fill-column-mode 1)))

  (define-key mu4e-headers-mode-map (kbd "(") 'mu4e-headers-prev-unread)
  (define-key mu4e-headers-mode-map (kbd ")") 'mu4e-headers-next-unread)
  (define-key mu4e-view-mode-map (kbd "(") 'mu4e-view-headers-prev-unread)
  (define-key mu4e-view-mode-map (kbd ")") 'mu4e-view-headers-next-unread)
  (define-key mu4e-view-mode-map  (kbd "c") 'visual-fill-column-mode))

;;;; Password management (password-store)

(auth-source-pass-enable)

;;;; PDF Tools

;; (setq pdf-info-epdfinfo-program (expand-file-name "server/epdfinfo" (borg-worktree "pdf-tools")))

(pdf-tools-install)

(with-eval-after-load 'tex
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
	  (add-to-list 'TeX-view-program-list-builtin
                 '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools")))

(add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode)))

(setq pdf-misc-print-programm (executable-find "lpr")
      pdf-misc-print-programm-args '("-o media=A4" "-o fitplot"))

(define-key pdf-view-mode-map (kbd "s a") 'pdf-view-auto-slice-minor-mode)
(define-key pdf-view-mode-map (kbd "s r") 'pdf-view-reset-slice)

;;;; Regular expression builder

(setq reb-re-syntax 'string)

;;;; scpaste

;; Technomancy's scpaste is a replacement for pastebin,
;; paste.lisp.org, and similar services.  It generates a HTML page out
;; of a buffer or region and publishes it using scp.

(setq scpaste-scp-destination "thblt@k9.thb.lt:/var/www/paste.thb.lt/"
      scpaste-http-destination "https://paste.thb.lt"
      scpaste-user-address "https://thb.lt"
      scpaste-make-name-function 'scpaste-make-name-from-timestamp)

(defun thblt/scpaste-without-noise (f &rest args)
  "A lot of packages add overlays which are useful when editing,
noisy when reading.  We advise scpaste so a few minor modes get
disabled before it runs, and restored afterwards."
  (let ((tmm transient-mark-mode)
        (hig (bound-and-true-p highlight-indent-guides-mode))
        (flyc (bound-and-true-p flycheck-mode))
        (flys (bound-and-true-p flyspell-mode))
        (ssp (bound-and-true-p show-smartparens-mode)))
    (when tmm
      (transient-mark-mode 0))
    (when hig
      (highlight-indent-guides-mode -1))
    (when flyc
      (flycheck-mode -1))
    (flyspell-mode -1)
    (when ssp
      (show-smartparens-mode -1))
    (apply f args) ; Run wrapped function
    (when tmm
      (transient-mark-mode 1))
    (when hig
      (highlight-indent-guides-mode 1))
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
  (load (expand-file-name "init.el" user-emacs-directory)))

(defun thblt/reload-all-emacsen ()
  "Execute `thblt/reload-emacs' on all servers."
  (interactive)
  (dolist (instance (directory-files server-socket-dir nil (rx bol (not (any ".")))))
    (unless (equal instance server-name)
	    (async-shell-command (format "emacsclient -s %s --eval \"(thblt/reload-emacs)\"" instance)))))

;; Also, some utility function:

(defun thblt/server-start (name)
  "Prompt for NAME, then start the Emacs server under that name."
  (interactive "sDaemon name? ")
  (setq server-name name)
  (server-start))

;;;; Report success

;; We finally set the initial contents of the scratch buffer.  This
;; makes it easy to notice when something went wrong (this may not be
;; obvious in daemon mode)

(setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n")

(message "Reached the end of init.el")
