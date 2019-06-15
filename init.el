;;; Introduction

;; This chapter deals with the general use of Emacs, and is limited to
;; general settings and sane defaults.  It's a bit messy, since it's
;; mostly made up of all the bits that don't fit anywhere else.

;; Let's start by saying hello.  Beyond being polite, when starting
;; daemon it helps identifying when the literate configuration has
;; started running.

;;;; Things to do before everything else

;; Never load bytecode if .el is more recent
(setq load-prefer-newer t)

;; Load the package manager
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; Recompile what needs recompiling
(auto-compile-on-load-mode)

;; Put things where it makes sense.
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
      custom-file "/dev/null")

;;;; Defaults

(setq-default
 ;; Use default browser from the system. Using =setsid xdg-open=
 ;; prevents Emacs from killing xdg-open before it actually opened
 ;; anything. See
 ;; [[https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open][here]].
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "setsid"
 browse-url-generic-args '("xdg-open")

 ;; Change the default major mode to =text-mode= instead of
 ;; =fundamental-mode=.  Fundamental has no hooks.
 major-mode 'text-mode)

(load custom-file t)

;;; User interface

;;;; Settings and general configuration

(setq-default
 cursor-type '(bar . 5)
 enable-recursive-minibuffers t
 inhibit-startup-screen t
 use-dialog-box nil
 vc-follow-symlinks t

 truncate-lines t

 disabled-command-function nil)

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

;;;; Fonts and themes

;; Configure the default font:

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 10"))
(set-face-attribute 'default nil
                    :height 100)


(defadvice load-theme (before theme-dont-propagate activate)
  "Disable active themes before loading a new one."
  (mapc #'disable-theme custom-enabled-themes))

;; And load the default theme: [[https://github.com/thblt/eziam-theme-emacs][Eziam]].

(setq eziam-scale-headings nil
      eziam-heading-style 'gray-blocks)

(load-theme 'eziam-light t)

;; Create some shortcut commands to load the Eziam themes:
(defun eziam-dark () (interactive) (load-theme 'eziam-dark t))
(defun eziam-dusk () (interactive) (load-theme 'eziam-dusk t))
(defun eziam-light () (interactive) (load-theme 'eziam-light t))
(defun eziam-white () (interactive) (load-theme 'eziam-white t))

;;;; Modeline
;;;;; Variables

(require 'kurecolor)
(require 'powerline)
(require 'server)

(setq x-underline-at-descent-line t
      powerline-gui-use-vcs-glyph t)

;;;;; Theme

(defun thblt/powerline-theme ()
  "Setup the default mode-line."

  ;; MAINTENANCE NOTES
  ;;
  ;;  - the `mode-line' face isn't used, because the whole modeline
  ;;  - color is determined by the current Evil mode.

  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (face (if active 'mode-line 'mode-line-inactive))
             (last-face) ; The last used face, to show the
                                        ; correct separator after conditional
                                        ; segments
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (delim-face (if active 'thblt/mode-line--delimiter face))
             (space (powerline-raw " " face))
             (open (powerline-raw " [" delim-face))
             (open* (powerline-raw "[" delim-face))
             (close (powerline-raw "]" delim-face))
             (lhs
              (list

               ;; Buffer id
               ;; Modified?
               (when (and buffer-file-name (buffer-modified-p))
                 (powerline-raw " ●"  'thblt/mode-line--buffer-modified))
               ;; Read-only?
               (when buffer-read-only
                 (powerline-raw " "  'thblt/mode-line--buffer-read-only))
               ;; Not read-only, has a file, but isn't modified: spaces where the modified marker will appear
               (when (and buffer-file-name
                          (not (or (buffer-modified-p)
                                   buffer-read-only)))
                 ;; @Notice: we're borrowing the narrow face here
                 (powerline-raw " -" 'thblt/mode-line--buffer-narrowed))

               ;; Buffer name
               (progn
                 (setq last-face 'thblt/mode-line--buffer-id)
                 ( powerline-raw
                   " %[%b%] "
                   `(:weight ,(if buffer-file-name 'bold 'normal) :inherit thblt/mode-line--buffer-id)))

               ;; Narrowing indicator
               (when (buffer-narrowed-p)
                 (powerline-raw "[Narrow] " `(:inherit thblt/mode-line--buffer-narrowed :inherit thblt/mode-line--buffer-id )))

               (funcall separator-left last-face face)
               ;; Position
               (powerline-raw " %2l:%3c [%o]    " face)

               ;; Major mode
               open
               (powerline-major-mode `(:inherit ,face :weight bold 'r))
               ;; Minor modes
               (powerline-minor-modes  face 'l)
               close

               space space space space

               ;; open
               ;; (powerline-raw "⯃ 3 ⯅ 14" face)
               ;; close
               ))
             (rhs (list
                   ;; Version control
                   (when buffer-file-name
                     (concat
                      open
                      (powerline-raw
                       (concat
                        (projectile-project-name)
                        (powerline-vc))
                       face)
                      close))

                   space
                   (when  (window-parameter (selected-window) 'thblt/window-at-bottom-right)
                     (powerline-raw
                      (if server-process
                          (format " %s  " server-name)
                        ""))))

                  ))

        (concat (powerline-render lhs)
                (powerline-fill face (powerline-width rhs))
                (powerline-render rhs)))))))

;;;;; Faces (and cursor!)

(defun thblt/mode-line-set-faces (&rest _) ; I'm hooking this on theme change so it needs to accept arg
  "Configure faces for the mode-line."
  (pl/reset-cache)

  (let* ((default-bg (face-attribute 'default :background))
         (default-fg (face-attribute 'default :foreground))

         (dark (< (kurecolor-hex-get-brightness default-bg) .5))

         (buffid-bg (if dark "#000000" "#ffffff"))

         (inactive (if dark "#111111" "#dddddd"))

         ;; Vc states
         (server-bg                         "#520052")
         (server-fg                         "#ffffff"))

    ;; Modeline
    (face-spec-set 'mode-line
                   `((t
                      :background ,(if dark "#ffffff" "#000022")
                      :foreground ,(if dark "black" "white")
                      :overline ,(if dark "black" "white")
                      :underline ,(if dark "black" "white"))))

    ;; Inactive mode line (invisible)
    (face-spec-set 'mode-line-inactive
                   `((t
                      :background ,inactive
                      :foreground ,inactive
                      :overline ,default-bg
                      :underline ,default-bg

                      )))

    (face-spec-set 'thblt/mode-line--buffer-modified
                   `((t
                      :background ,buffid-bg
                      :foreground "#ff0000")))

    (face-spec-set 'thblt/mode-line--buffer-read-only
                   `((t
                      :background ,buffid-bg
                      :foreground "#ff0000"))) ;

    ;; Narrowing indicator
    (face-spec-set 'thblt/mode-line--buffer-narrowed
                   `((t
                      :background ,buffid-bg
                      :foreground "#888888")))

    ;; Minor mode lighter
    (face-spec-set 'thblt/mode-line--minor-mode
                   `((t)))

    ;; Server ON face
    (face-spec-set 'thblt/mode-line--server
                   `((t
                      :background ,server-bg
                      :foreground ,server-fg)))

    ;; Buffer ID
    (face-spec-set 'thblt/mode-line--buffer-id
                   `((t
                      :background ,buffid-bg
                      :foreground ,(if dark "white" "black"))))
    ;; Delimiter
    (face-spec-set 'thblt/mode-line--delimiter
                   `((t)))
    ))

;;       '(emacs insert lisp motion nil normal operator replace visual))))

;;;;;; Window position tracker

(defun thblt/window-at-bottom-left-p (win)
  "Return non-nil if WIN is at the bottom left of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'left win))))

(defun thblt/window-at-bottom-right-p (win)
  "Return non-nil if WIN is at the bottom right of the frame."
  (not (or
        (window-in-direction 'below win)
        (window-in-direction 'right win))))

(defun thblt/update-window-position-parameters (&optional frame)
  (unless frame (setq frame (selected-frame)))
  (mapc (lambda (win)
          (set-window-parameter win 'thblt/window-at-bottom-left (thblt/window-at-bottom-left-p win))
          (set-window-parameter win 'thblt/window-at-bottom-right (thblt/window-at-bottom-right-p win)))
        (window-list frame nil)))

(add-hook 'window-configuration-change-hook 'thblt/update-window-position-parameters)

;;;;; Installation

(thblt/mode-line-set-faces)
(advice-add 'load-theme :after 'thblt/mode-line-set-faces)

(add-to-list 'after-make-frame-functions 'thblt/update-window-position-parameters)
(unless (daemonp)
  (thblt/update-window-position-parameters)) ; This is required for
                                        ; non-daemon instances
                                        ; where the frame is
                                        ; created before init.el
                                        ; gets to run.

(thblt/powerline-theme)

;;;; Projectile

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(counsel-projectile-mode)

;; - globally ignore undo-files and similar byproducts.
(setq projectile-globally-ignored-file-suffixes (append '(
                                                          ".un~"
                                                          ".~undo-tree~"
                                                          )
                                                        projectile-globally-ignored-files))

(diminish 'projectile-mode)

;; I prefer to treat submodules as separate projects, so don't include
;; then in the main file listing:

(setq projectile-git-submodule-command nil)

;;;; UI Utilities

;;;;; Ace-window

(with-eval-after-load 'ace-window
  ;; We make use of aw-ignored-buffers, so we need the eval-after-load
  (setq aw-scope 'frame
        aw-background nil

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

(general-define-key "C-x o" 'ace-window
                    ;; Emulate window-numbering
                    "M-0" 'thblt/switch-to-minibuffer)
;; "M-1" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 1))
;; "M-2" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 2))
;; "M-3" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 3))
;; "M-4" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 4))
;; "M-5" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 5))
;; "M-6" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 6))
;; "M-7" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 7))
;; "M-8" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 8))
;; "M-9" (lambda () (interactive) (thblt/aw-switch-to-numbered-window 9)))

;;;;; Buffer management (ibuffer)

;; Rebind =C-x C-b= to =ibuffer= instead of =list-buffers=:

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;; Eyebrowse

(require 'eyebrowse)
(eyebrowse-mode)

;;;;; Hydra

(with-eval-after-load 'hydra
  (require 'posframe)
  (setq hydra-hint-display-type 'posframe))

;;;;; Ivy

(setq ivy-use-virtual-buffers t)

(ivy-mode)
(diminish 'ivy-mode)

(general-define-key
 "M-i"     'counsel-imenu
 "M-x"     'counsel-M-x
 "C-x C-f" 'counsel-find-file

 "C-S-s"   'swiper

 "C-x 8 RET" 'counsel-unicode-char)

;;;;; Popwin

;; Popwin “makes you free from the hell of annoying buffers”:

(require 'popwin)
(popwin-mode)


;;;;; Which-key

(which-key-mode)
(diminish 'which-key-mode)

;;;;; Windmove


(setq windmove-wrap-around t)

(general-define-key
 "S-<up>" 'windmove-up
 "S-<left>" 'windmove-left
 "S-<right>" 'windmove-right
 "S-<down>" 'windmove-down)


;;;;; Winner


(winner-mode)


;;;;; Customization helper

;; A small function to identify the face at point.  Nice to have when
;; writing themes, and faster than =C-u C-x ==.
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

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
  ("t w" (lambda () (interactive) (load-theme 'eziam-white t)) (thblt/hydra-indicator"White theme" (member 'eziam-white custom-enabled-themes)))
  ("t l" (lambda () (interactive) (load-theme 'eziam-light t)) (thblt/hydra-indicator"Light theme" (member 'eziam-light custom-enabled-themes)))
  ("t u" (lambda () (interactive) (load-theme 'eziam-dusk t)) (thblt/hydra-indicator"Dusk theme" (member 'eziam-dusk custom-enabled-themes)))
  ("t d" (lambda () (interactive) (load-theme 'eziam-dark t)) (thblt/hydra-indicator"Dark theme" (member 'eziam-dark custom-enabled-themes)))

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

(general-define-key "<f12>" 'hydra-editor-appearance/body)

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
(require 'flyspell-correct-ivy)
(general-define-key :keymaps 'flyspell-mode-map
                    "M-$" 'flyspell-auto-correct-previous-word
                    "C-;" 'flyspell-correct-previous-word-generic)

;;;; Moving around

;;;;; avy

(general-define-key "C-:" 'avy-goto-char-timer
                    "M-g f" 'avy-goto-line)

;;;;; beginend

(require 'beginend)
(beginend-global-mode)
(mapc (lambda (m) (diminish (cdr m)))
      beginend-modes)
(diminish 'beginend-global-mode)

;;;;; mwim

(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
(global-set-key (kbd "<home>") 'mwim-beginning-of-line-or-code)
(global-set-key (kbd "<end>") 'mwim-end-of-line-or-code)

;;;;; nav-flash (don't get lost)

(require 'nav-flash)
(face-spec-set 'nav-flash-face '((t (:inherit pulse-highlight-face))))
(advice-add 'recenter-top-bottom :after (lambda (x) (nav-flash-show)))

;;;; Replace

(general-define-key
 "C-M-%" 'vr/query-replace
 "C-c r" 'vr/replace
 "C-c m" 'vr/mc-mark)

;;;; Minor modes

;;;;; Auto-revert-mode

(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode " 🔃"))

;;;;; Move text

;; Move lines of text with =M-<up>= and =M-<down>=.


(move-text-default-bindings)

;;;;; Recentf

(recentf-mode)

;;;;; TODO Smartparens

(require 'smartparens-config) ;; Load default config
(smartparens-global-mode)
(show-smartparens-global-mode)
(diminish 'smartparens-mode)

;;;;;; Bindings

;; Since the author of Smartparens released
;; [[https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el][his
;; own config]], here it is, copy-pasted and slightly modified to suit
;; my needs:

(general-define-key :map smartparens-mode-map
                    "C-M-f" 'sp-forward-sexp

                    "C-M-b" 'sp-backward-sexp

                    "C-M-d" 'sp-down-sexp
                    "C-M-a" 'sp-backward-down-sexp
                    "C-S-d" 'sp-beginning-of-sexp
                    "C-S-a" 'sp-end-of-sexp

                    "C-M-e" 'sp-up-sexp
                    "C-M-u" 'sp-backward-up-sexp
                    "C-M-t" 'sp-transpose-sexp

                    "C-M-n" 'sp-next-sexp
                    "C-M-p" 'sp-previous-sexp

                    "C-M-k" 'sp-kill-sexp
                    "C-M-w" 'sp-copy-sexp

                    "M-<delete>" 'sp-unwrap-sexp
                    "M-<backspace>" 'sp-backward-unwrap-sexp

                    "C-<right>" 'sp-forward-slurp-sexp
                    "C-<left>" 'sp-forward-barf-sexp
                    "C-M-<left>" 'sp-backward-slurp-sexp
                    "C-M-<right>" 'sp-backward-barf-sexp

                    "M-D" 'sp-splice-sexp
                    "C-M-<delete>" 'sp-splice-sexp-killing-forward
                    "C-M-<backspace>" 'sp-splice-sexp-killing-backward
                    "C-S-<backspace>" 'sp-splice-sexp-killing-around

                    "C-]" 'sp-select-next-thing-exchange
                    "C-<left_bracket>" 'sp-select-previous-thing
                    "C-M-]" 'sp-select-next-thing

                    "M-F" 'sp-forward-symbol
                    "M-B" 'sp-backward-symbol

                    "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2))
                    "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)))

;; (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
;; (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
;; (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
;; (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
;; (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
;; (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
;; (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
;; (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
;; (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
;; (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
;; (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
;; (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
;; (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
;; (defvar hyp-s-x-map)
;; (define-prefix-command 'hyp-s-x-map)
;; (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
;; (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
;; (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
;; (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

;; (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

;; (bind-key ";" 'sp-comment emacs-lisp-mode-map)

;; (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;; ;;;;;;;;;;;;;;;;;;
;; ;; pair management

;; (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;; (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;  markdown-mode
;; (sp-with-modes '(markdown-mode gfm-mode rst-mode)
;;   (sp-local-pair "*" "*"
;;                  :wrap "C-*"
;;                  :unless '(sp--gfm-point-after-word-p sp-point-at-bol-p)
;;                  :post-handlers '(("[d1]" "SPC"))
;;                  :skip-match 'sp--gfm-skip-asterisk)
;;   (sp-local-pair "**" "**")
;;   (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

;; (defun sp--gfm-point-after-word-p (id action context)
;;   "Return t if point is after a word, nil otherwise.
;; This predicate is only tested on \"insert\" action."
;;   (when (eq action 'insert)
;;     (sp--looking-back-p (concat "\\(\\sw\\)" (regexp-quote id)))))

;; (defun sp--gfm-skip-asterisk (ms mb me)
;;   (save-excursion
;;     (goto-char mb)
;;     (save-match-data (looking-at "^\\* "))))

;; ;;; rst-mode
;; (sp-with-modes 'rst-mode
;;   (sp-local-pair "``" "``"))

;; ;;; org-mode
;; (sp-with-modes 'org-mode
;;   (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
;;   (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
;;   (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;   (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;   (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
;;   (sp-local-pair "«" "»"))

;; (defun sp--org-skip-asterisk (ms mb me)
;;   (or (and (= (line-beginning-position) mb)
;;            (eq 32 (char-after (1+ mb))))
;;       (and (= (1+ (line-beginning-position)) me)
;;            (eq 32 (char-after me)))))

;; ;;; tex-mode latex-mode
;; (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
;;   (sp-local-tag "i" "\"<" "\">"))

;; ;;; lisp modes
;; (sp-with-modes sp--lisp-modes
;;   (sp-local-pair "(" nil
;;                  :wrap "C-("
;;                  :pre-handlers '(my-add-space-before-sexp-insertion)
;;                  :post-handlers '(my-add-space-after-sexp-insertion)))



;; (defun my-add-space-after-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (forward-char (sp-get-pair id :cl-l))
;;       (when (or (eq (char-syntax (following-char)) ?w)
;;                 (looking-at (sp--get-opening-regexp)))
;;         (insert " ")))))

;; (defun my-add-space-before-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (backward-char (length id))
;;       (when (or (eq (char-syntax (preceding-char)) ?w)
;;                 (and (looking-back (sp--get-closing-regexp))
;;                      (not (eq (char-syntax (preceding-char)) ?'))))
;;         (insert " ")))))

;; ;;; C++
;; (sp-with-modes '(malabar-mode c++-mode)
;;   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
;; (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
;;                                                     ("* ||\n[i]" "RET")))

;; ;;; PHP
;; (sp-with-modes '(php-mode)
;;   (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
;;                                              (my-php-handle-docstring "RET")))
;;   (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
;;   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

;; (defun my-php-handle-docstring (&rest _ignored)
;;   (-when-let (line (save-excursion
;;                      (forward-line)
;;                      (thing-at-point 'line)))
;;     (cond
;;      ;; variable
;;      ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
;;       (let ((var-name (match-string 1 line))
;;             (type ""))
;;         ;; try to guess the type from the constructor
;;         (-when-let (constructor-args (my-php-get-function-args "__construct" t))
;;           (setq type (or (cdr (assoc var-name constructor-args)) "")))
;;         (insert "* @var " type)
;;         (save-excursion
;;           (insert "\n"))))
;;      ((string-match-p "function" line)
;;       (save-excursion
;;         (let ((args (save-excursion
;;                       (forward-line)
;;                       (my-php-get-function-args nil t))))
;;           (--each args
;;             (when (my-php-should-insert-type-annotation (cdr it))
;;               (insert (format "* @param %s%s\n"
;;                               (my-php-translate-type-annotation (cdr it))
;;                               (car it))))))
;;         (let ((return-type (save-excursion
;;                              (forward-line)
;;                              (my-php-get-function-return-type))))
;;           (when (my-php-should-insert-type-annotation return-type)
;;             (insert (format "* @return %s\n" (my-php-translate-type-annotation return-type))))))
;;       (re-search-forward (rx "@" (or "param" "return") " ") nil t))
;;      ((string-match-p ".*class\\|interface" line)
;;       (save-excursion (insert "\n"))
;;       (insert "* ")))
;;     (let ((o (sp--get-active-overlay)))
;;       (indent-region (overlay-start o) (overlay-end o)))))

;;;;; Undo-Tree

(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;;;; Yasnippet

(setq yas-snippet-dirs
      (list (expand-file-name "~/.emacs.d/etc/snippets" )
            (borg-worktree "yasnippet-snippets/snippets")))

(yas-global-mode)

(diminish 'yas-minor-mode)

;;;; Misc customizations

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


;;;;; Bindings

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-y") 'copy-line)
(global-set-key (kbd "C-h") 'delete-backward-char)

;;;;; Autosave when losing focus

(super-save-mode +1)
(diminish 'super-save-mode " 💾")

;;;;; Delete trailing whitespace when saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Writing prose

;; This chapter deals with two things:

;;  1. Major modes dedicated to writing prose, as opposed to code or
;;     configuration.
;;  2. Non-code bits in code/configuration files: comments and integrated
;;     documentation.

;;;; Common settings and minor modes
;;;;; Abbrev

(add-hook 'text-mode-hook (lambda () (abbrev-mode t)))
(diminish 'abbrev-mode)

;;;;; Unfill

(general-define-key "M-Q" 'unfill-paragraph)

;;;;; Wordwrap/visual line/visual-fill-column

(with-eval-after-load 'simple
  (diminish 'visual-line-mode))

(require 'visual-fill-column)

;;;; Major modes

;;;;; AucTex

(require 'tex-site)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (TeX-fold-mode t)))

(progn
  (setq-default TeX-save-query nil      ; Autosave
                TeX-parse-self t
                TeX-engine 'xetex
                TeX-source-correlate-mode t)) ;; Synctex on

(with-eval-after-load 'reftex-vars
  (progn
    ;; (also some other reftex-related customizations)
    (setq reftex-cite-format
          '((?\C-m . "\\cite[]{%l}")
            (?f . "\\footcite[][]{%l}")
            (?t . "\\textcite[q]{%l}")
            (?p . "\\parencite[]{%l}")
            (?o . "\\citepr[]{%l}")
            (?n . "\\nocite{%l}")))))

;;;;; Org-mode

(setq org-catch-invisible-edits t
      org-hide-leading-stars t
      org-hide-emphasis-markers nil
      org-html-htmlize-output-type 'css
      org-imenu-depth 8
      org-src-fontify-natively t
      org-ellipsis " ▼"
      org-special-ctrl-a/e t
      org-special-ctrl-k t)

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode t)
                           (visual-line-mode t)
                           (which-function-mode t)))

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))



;;;;;; PDF export

(setq org-latex-pdf-process (list "latexmk -CA %f" "latexmk -f -pdf %f"))

(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC"))))

(defun sp--org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))


;; Some cool org extensions:

;; - =toc-org= provides, guess what, automatic TOC generation for
;;   org-mode.  This is better
;;   [[https://github.com/snosov1/toc-org/issues/20#issuecomment-276407541][pinned
;;   to melpa-stable]].

(add-hook 'org-mode-hook 'toc-org-enable)


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

(general-define-key :keymaps 'org-mode-map
                    "<f1> <f1>" 'thblt/org-where-am-i)

;; The *emphasize selected* bindings:

(defvar selected-org-mode-map (make-sparse-keymap))
(define-key selected-org-mode-map (kbd "b") (lambda () (interactive) (org-emphasize ?*)))
(define-key selected-org-mode-map (kbd "i") (lambda () (interactive) (org-emphasize ?/)))

;;;;;; Org-agenda:

(setq org-agenda-files (list "~/Documents/LOG.org")
      org-default-notes-file "~/Documents/LOG.org")

;;;;;; Org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (shell . t)))

;;;;;; Org-ref

;; (=helm-bibtex= and =biblio= are dependencies of =org-ref=)

(setq org-ref-completion-library 'org-ref-ivy-cite
      bibtex-dialect 'biblatex)

;; org-ref must have been (require)d to work.

(add-hook 'org-mode-hook (lambda () (require 'org-ref)))

;;; Writing code
;;;; Settings

;; Some basic settings...

(setq-default comment-empty-lines nil
              tab-width 2
              c-basic-offset 2
              cperl-indent-level 2
              indent-tabs-mode nil)

;; and a few mappings.

(global-set-key (kbd "<f8>") 'ffap)
(global-set-key (kbd "<f5>") 'recompile)

;;;; Minor modes
;;;;; Aggressive indent

(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode "⭾"))

;;;;; Color-identifiers

(add-hook 'prog-mode-hook 'color-identifiers-mode)
(advice-add 'load-theme :after (lambda (&rest _)
                                 (color-identifiers:regenerate-colors)
                                 (color-identifiers:refresh)))
(with-eval-after-load 'color-identifiers-mode
  (add-to-list
   'color-identifiers:modes-alist'
   `(haskell-mode . ("[^.][[:space:]]*"
                     "\\_<\\([[:lower:][:upper:]]\\([_]??[[:lower:][:upper:]\\$0-9]+\\)*\\(_+[#:<=>@!%&*+/?\\\\^|~-]+\\|_\\)?\\)"
                     (nil scala-font-lock:var-face font-lock-variable-name-face)))))

(with-eval-after-load 'color-identifiers-mode
  (diminish 'color-identifiers-mode))

;;;;; Company

(add-hook 'prog-mode-hook 'company-mode)
;;TODO BIND  :bind (:map company-mode-map
;; (("M-TAB" . company-complete-common)))
(with-eval-after-load 'company
  (diminish 'company-mode "⋯ "))

;;;;; Evil Nerd Commenter

(require 'evil-nerd-commenter)
(general-define-key "M-,"   'evilnc-comment-or-uncomment-lines
                    "C-M-," 'evilnc-comment-or-uncomment-paragraphs)

;;;;; Flycheck

;; (add-hook 'prog-mode-hook 'flycheck-mode)

;; (with-eval-after-load 'flycheck
;; (diminish 'flycheck-mode "▲"))

;;;;; Highlight-indent-guides

(setq highlight-indent-guides-method 'fill
      highlight-indent-guides-character ?┃
      highlight-indent-guides-auto-character-face-perc 25)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(with-eval-after-load 'highlight-indent-guides
  (diminish 'highlight-indent-guides-mode))

;;;;; Outline and Outshine

(add-hook 'prog-mode-hook 'outshine-mode)
(add-hook 'haskell-mode-hook (lambda () (setq-local outshine-preserve-delimiter-whitespace t)))

(diminish 'outline-minor-mode)
(with-eval-after-load "outshine"
  (diminish 'outshine-mode))

;;;;; Rainbow mode

;; Rainbow mode is similar to Atom's Pigments plugin or something.
(add-hook 'prog-mode-hook (rainbow-mode))
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)

(with-eval-after-load 'rainbow-mode
  (diminish 'rainbow-mode))

;;;; Programming languages

;;;;; C/C++

(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(setq irony-server-install-prefix (expand-file-name "build" (borg-worktree "irony")))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-irony))

(with-eval-after-load 'irony
  (diminish' irony-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;;;; Haskell

;; Intero mode is a “complete interactive development program for
;; Haskell”:

(add-hook 'haskell-mode-hook 'intero-mode-blacklist)
;; (intero-global-mode)

(setq intero-blacklist '("~/.dotfiles" "~/.xmonad/"))

(general-define-key :keymaps 'haskell-mode-map
                    "<f1> <f1>" 'hayoo-query)

;;;;; Rust

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(add-hook 'rust-mode-hook
          (lambda nil (setq-local compile-command "cargo build")))

;;; Tools

;; This section deals with tools which don't edit anything.

;;;; Borg and their Queen

(defun thblt/borg-build-all nil
  (interactive)
  (mapc (lambda (x) (borg-build x))
        (borg-drones)))

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



;;;; Ebib

(setq ebib-bibtex-dialect 'biblatex)

;;;; ERC

(setq erc-server "irc.freenode.net"
      erc-port 7000
      erc-nick "thblt"
      erc-nick-uniquifier  "`"

      erc-server-auto-reconnect t
      erc-kill-buffer-on-part t

      erc-lurker-hide-list '("JOIN" "PART" "QUIT")
      erc-lurker-threshold-time 900 ; 15mn

      erc-header-line-format nil)

(add-hook 'erc-mode-hook (lambda ()
                           (visual-line-mode)
                           (erc-hl-nicks-mode)
                           (erc-fill-disable)))

(advice-add 'load-theme :after (lambda (&rest _) (when (functionp 'erc-hl-nicks-reset-face-table)
                                                   (erc-hl-nicks-reset-face-table))))

;;;; TODO Magit and Git

(general-define-key
 "C-x g s" 'magit-status
 "C-x g r" 'magit-list-repositories
 "C-x g t" 'git-timemachine)


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

(require 'cl)
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

;;;; Mu4e

;; mu4e is loaded as a regular Emacs package, complete with its binary,
;; built through usual Borg mechanisms.

(setq mu4e-mu-binary (expand-file-name "mu/mu" (borg-worktree "mu4e")))

;; Each of my accounts is synced (by =mbsync=) to a folder at the root
;; of the Maildir (eg, =~/.Mail/Academic/=).  We then need a function
;; to switch contexts based on a regular expression on the current
;; Maildir path.  For some reason, this doesn't come included with
;; mu4e, so here it is, and it probably comes
;; [[https://www.reddit.com/r/emacs/comments/47t9ec/share_your_mu4econtext_configs/d0fsih6/][from
;; here]].

(defun mu4e-message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

;; Then the bulk of the config:

(require 'mu4e-contrib)

(setq mu4e-completing-read-function 'ivy-completing-read

      ;; General settings
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
      mu4e-headers-attach-mark '("" . "")
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
                            (:thread-subject . nil)
                            )

      mu4e-user-mail-address-list '(
                                    "thblt@thb.lt"
                                    "thibault.polge@ac-orleans-tours.fr"
                                    "thibault.polge@etu.univ-paris1.fr"
                                    "thibault.polge@malix.univ-paris1.fr"
                                    "thibault.polge@univ-paris1.fr"
                                    "thibault@thb.lt"
                                    "tpolge@gmail.com"
                                    ))

(add-hook 'mu4e-view-mode-hook (lambda ()
                                 (setq visual-fill-column-width 80)
                                 (visual-line-mode 1)
                                 (visual-fill-column-mode 1)))

(general-define-key :keymaps 'mu4e-headers-mode-map
                    "("      'mu4e-headers-prev-unread
                    ")"      'mu4e-headers-next-unread)
(general-define-key :keymaps 'mu4e-view-mode-map
                    "("      'mu4e-view-headers-prev-unread
                    ")"      'mu4e-view-headers-next-unread
                    "c"      'visual-fill-column-mode)

;; Compose messages with org-mode tables and lists (using =orgalist=):

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'orgalist-mode)

;; Refiling messages: if a message is in =*/INBOX=, refile to =*/Archive=,
;; otherwise leave it where it is.

(setq mu4e-refile-folder
      (lambda (msg)
        (let ((maildir (mu4e-message-field msg :maildir)))
          (message "This is in %s" maildir)
          (if (string-suffix-p "/Inbox" maildir)
              (concat (substring maildir 0 -5) "Archive")
            maildir))))


;;;;; Contexts

(setq mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask)


(eval-after-load 'mu4e
  '(setq mu4e-contexts `(
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
                                           (mu4e-message-maildir-matches msg "^/OVH/")))
                           :vars '(( user-mail-address   . "thibault.polge@ac-orleans-tours.fr"  )
                                   ( mu4e-sent-folder        . "/Ac/Sent" )
                                   ( mu4e-drafts-folder      . "/Ac/Drafts" )
                                   ( mu4e-trash-folder       . "/Ac/Trash" )
                                   ;; ( mu4e-refile-folder      . "/OVH/Archive" )
                                   ( smtpmail-local-domain   . "ac-orleans-tours.fr" )
                                   ( smtpmail-smtp-server    . "smtps.ac-orleans-tours.fr" )
                                   ( smtpmail-smtp-user      . "tpolge" )
                                   ( smtpmail-stream-type    . tls )
                                   ( smtpmail-smtp-service   . 465 ))))))

;; =================
;; IMPORTANT.  Si tu ajoutes un contexte (ou plusieurs), il faut modifier dans dotemacs.org la ligne
;; > mu4e-compose-context-policy 'pick-first)
;; en
;; > mu4e-compose-context-policy 'ask)
;; =================


;;;;; Bookmarks

(setq mu4e-bookmarks `(("(m:/OVH/INBOX) or (m:/Ac/INBOX)"     "Global inbox" ?i)
                       ("(m:/OVH/Archive) or (m:/Ac/Archive)" "Archives"     ?a)
                       ("(flag:flagged)"                      "Flagged"      ?f)
                       ("(m:/OVH/Sent) or (m:/Ac/Sent)"       "Sent"         ?s)
                       ("(m:/OVH/Drafts) or (m:/Ac/Drafts)"   "Drafts"       ?d)))

;;;; Password management (password-store)

(auth-source-pass-enable)

;;;; PDF Tools

;; (=tablist= is a dependency of =pdf-tools=)


(setq pdf-info-epdfinfo-program (expand-file-name "server/epdfinfo" (borg-worktree "pdf-tools")))

(pdf-tools-install)

(with-eval-after-load 'tex
  (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
    (add-to-list 'TeX-view-program-list-builtin
                 '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools")))

(general-define-key :keymaps 'pdf-view-mode-map
                    "s a" 'pdf-view-auto-slice-minor-mode)


;;;; Regular expression builder

;; We use the =string= syntax, as advised on
;; [[https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder][this
;; Mastering Emacs' article]].

(setq reb-re-syntax 'string)


;;;; scpaste

;; Technomancy's =scpaste= is a replacement for pastebin, paste.lisp.org,
;; and similar services.  It generates a HTML page out of a buffer or
;; region and moves it over to a server using scp.

(setq scpaste-scp-destination "thblt@k9.thb.lt:/var/www/paste.thb.lt/"
      scpaste-http-destination "https://paste.thb.lt"
      scpaste-user-address "https://thb.lt"

      scpaste-make-name-function 'scpaste-make-name-from-timestamp)


;; A lot of packages add overlays which are useful when editing, noisy
;; when reading.  We advise scpaste so a few minor modes get disabled
;; before it runs, and restored afterwards.

(defun thblt/scpaste-without-noise (f &rest args)
  (let ((hig highlight-indent-guides-mode)
        (flyc flycheck-mode)
        (flys flyspell-mode))
    (highlight-indent-guides-mode -1)
    (flycheck-mode -1)
    (flyspell-mode -1)
    (apply f args)
    (when hig
      (highlight-indent-guides-mode 1))
    (when flyc
      (flycheck-mode 1))
    (when flys
      (flyspell-mode 1))))

(advice-add 'scpaste :around 'thblt/scpaste-without-noise)
(advice-add 'scpaste-region :around 'thblt/scpaste-without-noise)


;;; Conclusion

;;;; HiDPI support (kindof)

;; This section is made of overrides to improve support for HiDPI
;; monitors.  It must be at the end, to avoid being overriden by default
;; settings.

;; If we're running on a HiDPI machine, we replace the flycheck fringe
;; bitmap with a larger version.

;; (when (string-prefix-p  "maladict" system-name)

;;   (with-eval-after-load "flycheck"
;;     (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
;;       (vector
;;        #b1000000000
;;        #b1100000000
;;        #b1110000000
;;        #b1111000000
;;        #b1111100000
;;        #b1111110000
;;        #b1111111000
;;        #b1111111100
;;        #b1111111110
;;        #b1111111111
;;        #b1111111111
;;        #b1111111110
;;        #b1111111100
;;        #b1111111000
;;        #b1111110000
;;        #b1111100000
;;        #b1111000000
;;        #b1110000000
;;        #b1100000000
;;        #b1000000000)
;;       20 10 'center)))

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
