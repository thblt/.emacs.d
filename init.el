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

(setq eziam-scale-headings nil)
(load-theme 'eziam-dark t)

;; Create some shortcut commands to load the Eziam themes:
(defun eziam-dark () (interactive) (load-theme 'eziam-dark t))
(defun eziam-light () (interactive) (load-theme 'eziam-light t))
(defun eziam-dusk () (interactive) (load-theme 'eziam-dusk t))

;;;; Mode line

;;;;; The mode line itself

(require 'kurecolor)

(setq x-underline-at-descent-line t)

(defun thblt/mode-line-set-faces (&rest args)
  (let* ((default-bg (face-attribute 'default :background))
         (default-fg (face-attribute 'default :foreground))

         ;; @FIXME This is NOT a good way to compute brightnesqs.  Average the three components.
         (dark (< (kurecolor-hex-get-brightness default-bg) .5))
         (brightness-step (if dark .05 -.05))
         ;; active
         (ac-bg (kurecolor-adjust-brightness default-bg (* 12 brightness-step)))
         (ac-fg (if dark "black" "white"))
         ;; inactive
         (in-bg (kurecolor-adjust-brightness default-bg (* 2 brightness-step)))
         (in-fg (kurecolor-adjust-brightness default-bg (* (if dark 5 1) brightness-step))))

    (face-spec-set 'mode-line
                   `((t
                      :background ,ac-bg
                      :foreground ,ac-fg
                      :underline ,ac-bg
                      :overline ,ac-bg)))

    (face-spec-set 'mode-line-inactive
                   `((t
                      :background ,in-bg
                      :foreground ,in-fg
                      :underline ,in-bg
                      :overline ,in-fg)))

    (face-spec-set 'thblt/mode-line-server-name-face
                   `((t
                      :foreground "white"
                      :background ,(kurecolor-hsv-to-hex
                                    .85
                                    .95
                                    (kurecolor-hex-get-brightness ac-bg))
                      :box ,default-fg)))

    (face-spec-set 'thblt/mode-line-inactive-invisible
                   `((t
                      :foreground ,default-bg
                      :background ,default-bg
                      :underline ,default-bg
                      :overline ,default-bg)))))

(thblt/mode-line-set-faces)
(advice-add 'load-theme :after 'thblt/mode-line-set-faces)

(defun thblt/mode-line-get-face (base &optional variant)
  "Select a face for the mode-line."
  (intern (format "thblt/mode-line-%s%s%s-face"
                  base
                  (if active "-active" "")
                  (if variant (concat "-" variant) ""))))

(defun thblt/mode-line-wrap (str)
  (if str (concat " " str " ") ""))

(defun thblt/mode-line-sep (str)
  (if str (concat str " ") ""))

(setq-default
 mode-line-format
 '(
   ;; Window number
   (:eval
    (when (and (bound-and-true-p eyebrowse-mode) ;; Eyebrowse is bound and on
               (window-parameter (selected-window) 'thblt/window-at-bottom-left)) ;;
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      tag
                    (when num (int-to-string num)))))
        (propertize (thblt/mode-line-wrap str) 'face '(:weight bold :background "#FFCC33" :foreground "black" :box t)))))

   " "

   ;; Read-only marker
   (:eval
    (propertize
     (thblt/mode-line-sep (concat
                           (and buffer-read-only "")
                           (and (buffer-file-name) (buffer-modified-p) "💾")))
     'face '(:foreground "red")))

   ;; Buffer name
   (:eval (propertized-buffer-identification "%b"))


   ;; Buffer modes
   "    ["
   (:eval (propertize mode-name 'face 'bold))
   minor-mode-alist
   "]    "

   ;; Position
   (:eval (propertize "  %3l:%2c" 'face 'bold))
   " ("
   (:eval (propertize "%o" 'face 'italic))
   ")"
   "    "

   ;; Project/VC
   (:eval
    (when (or (projectile-project-p)
              vc-mode)
      (concat
       (propertize
        (when (projectile-project-p) (format "%s " (projectile-project-name)))
        'face 'bold)
        (--when-let vc-mode (format " %s" it))
        "    ")))

   ;; Server
   (:eval (thblt/mode-line-sep
           (when (and (and (boundp 'server-process) server-process)
                      (window-parameter (selected-window) 'thblt/window-at-bottom-right))

             (propertize (thblt/mode-line-wrap server-name) 'face 'thblt/mode-line-server-name-face))))))

;;;;; The window position tracker

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

(defun thblt/update-window-position-parameters ()
  (mapc (lambda (win)
          (set-window-parameter win 'thblt/window-at-bottom-left (thblt/window-at-bottom-left-p win))
          (set-window-parameter win 'thblt/window-at-bottom-right (thblt/window-at-bottom-right-p win)))
        (window-list (selected-frame) nil)))

(add-hook 'window-configuration-change-hook 'thblt/update-window-position-parameters)

;;;; Project management with Projectile

;; Let's load Projectile, and:

;; - globally ignore undo-files and similar byproducts.
;; - toggle the =C-p p= and =C-p SPC= bindings (I find the latter easier to
;;   enter, and thus more adequate for "do what I mean");
;;
;; @TODO:
;;
;; - Could Projectile read ignore patterns from ~/.gitignore_global?

(projectile-global-mode)
(counsel-projectile-mode)

(setq projectile-globally-ignored-file-suffixes (append '(
                                                          ".un~"
                                                          ".~undo-tree~"
                                                          )
                                                        projectile-globally-ignored-files))

(diminish 'projectile-mode)

;; I consider submodules to be separate projects, so don't include then
;; in the main file listing:

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

;;;; Evil and friends

(require 'evil)
(require 'evil-lisp-state)
(require 'evil-matchit)
(require 'evil-surround)

(evil-mode)
(global-evil-surround-mode)
(global-evil-matchit-mode)

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
  (diminish 'auto-revert-mode "🔃"))

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

(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)


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

;;;;; Yasnippet

(setq yas-snippet-dirs
      `("~/.emacs.d/etc/snippets"
        ,(borg-worktree "yasnippet-snippets/snippets")))

(yas-global-mode)

(diminish 'yas-minor-mode)

;;;; Misc customizations

;;;;; Use C-h as backspace

(general-define-key "C-h" 'delete-backward-char)

;;;;; TODO Autosave when losing focus

;; This is the initial version, which works (almost) perfectly well:

(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)))

;; TODO: Autosave when switching Emacs windows

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

(dolist (hook '(markdown-mode-hook org-mode-hook))
  (add-hook hook (lambda () (setq visual-fill-column-center-text t))))

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

;; (htmlize is a dependency of Org)

(setq org-catch-invisible-edits t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
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
  (diminish 'org-indent-mode)
  )



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
  (diminish 'company-mode))

;;;;; Evil Nerd Commenter

;; A good replacement for ~comment-dwim~, but unline
;; [[https://github.com/remyferre/comment-dwim-2][~comment-dwim2~]],
;; it can't alternate between commenting and commenting /out/ (adding
;; the comment delimiter at the start or the end of the line).

(general-define-key "M-;"   'evilnc-comment-or-uncomment-lines
                    "C-M-;" 'evilnc-comment-or-uncomment-paragraphs
                    "C-c l" 'evilnc-quick-comment-or-uncomment-to-the-line
                    "C-c c" 'evilnc-copy-and-comment-lines
                    "C-c p" 'evilnc-comment-or-uncomment-paragraphs)

;;;;; Flycheck

(add-hook 'prog-mode-hook 'flycheck-mode)

(with-eval-after-load 'flycheck
  (diminish 'flycheck-mode))

;;;;; Highlight-indent-guides

(setq highlight-indent-guides-method 'fill
      highlight-indent-guides-character ?┃
      highlight-indent-guides-auto-character-face-perc 25)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(diminish 'highlight-indent-guides-mode nil)

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

(add-hook 'haskell-mode-hook 'intero-mode)
;; (intero-global-mode)

(setq intero-blacklist '("~/.dotfiles"))

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

;;;; TODO Borg and their Queen

(defun thblt/borg-build-all nil
  (interactive)
    (mapc (lambda (x) (borg-build x))
          (borg-drones)))

;;;; Ebib

(setq ebib-bibtex-dialect 'biblatex)

;;;; ERC

(setq erc-server "irc.freenode.net"
      erc-port 7000
      erc-nick "thblt"
      erc-nick-uniquifier  "`"

      erc-server-auto-reconnect t

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
  (setq magit-repository-directories (mapcar (lambda (x) `(,x . 0)) projectile-known-projects)))

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
        ("Branch"     10  magit-repolist-column-branch)
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

(general-define-key "<f12>"  'mu4e)
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


;;; Package drafts

;;;; org-numbered-headings.el

;; This /experimental/ package provides automatically numbered headings for
;; Org-Mode using text overlays (it thus doesn't modify the buffer
;; contents in any way).  It supports the =:UNNUMBERED:== keyword, offers a
;; lot of numbering styles, customizable resets, and looks like this:

;;;;; Numbering styles

;; | Name   | Example                     |
;; |--------+-----------------------------|
;; | =Alpha=  | a, c, c ... aa, ab, ac ...  |
;; | =alpha=  | a, c, c ... aa, ab, ac ...  |
;; | =Arabic= | 1, 2, 3, 4, 5, 6...         |
;; | =Greek=  | Α, Β, Γ ...  ΑΑ, ΑΒ, ΑΓ ... |
;; | =greek=  | α, β, γ ...  αα, αβ, αγ ... |
;; | =Roman=  | I, II, III, IV, V, VI...    |
;; | =roman=  | i, ii, iii, iv, v, vi...    |

  ;;; org-numbered-headings.el --- Numbered headings for Org-Mode, who'd have guessed? (using overlays!)

;; Copyright (c) 2018 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintener: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: tools
;; Homepage: https://github.com/thblt/org-numbered-headings
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ;;; Commentary:

;; See README.org.
;;
;; This packages uses the onh- pseudo namespace.  It's now mine.

  ;;; Code:

(defvar-local onh-heading-properties
  '((1 . ((template "Part " '(1 . Roman) ". ")))
    (2 . ((template "Chapter {2@arabic}. " )))
    (3 . ((template "Section {3@arabic}. " )))
    (4 . ((template "({3@arabic}{4@latin}) ")))
    (5 . "{5@greek}/ ")))

(defun onh-format-number (format n)
  (cond
   ((eq format 'arabic) (number-to-string n))
   ((eq format 'roman) (downcase (org-export-number-to-roman n)))
   ((eq format 'Roman) (upcase (org-export-number-to-roman n)))
   ((eq format 'alpha) (char-to-string (+ ?a (- n 1))))
   ((eq format 'Alpha) (char-to-string (+ ?A (- n 1))))
   ((eq format 'greek) (char-to-string (+ ?α (- n 1))))
   ((eq format 'Greek) (char-to-string (+ ?Α (- n 1))))))

(defmacro thblt/step (n)
  `(setq ,n (1+ ,n)))

;; overlay test
(ignore
 (let ((part 0)
       (chapter 0))
   (remove-overlays)
   (org-map-entries
    (lambda nil
      (looking-at org-complex-heading-regexp)
      (let ((o (make-overlay (match-beginning 4) (1+ (match-beginning 4))))
            (l (org-current-level)))
        ;; (overlay-put o 'name "org-numbered-headings")
        (overlay-put o 'before-string
                     (format "%s %s. "
                             (if (= l 1) "Partie" "")
                             (if (= l 1)
                                 (thblt/format-number 'Roman (thblt/step part))
                               (thblt/format-number 'greek (thblt/step chapter)))))))))
 )


;;; Conclusion

;;;; HiDPI support (kindof)

;; This section is made of overrides to improve support for HiDPI
;; monitors.  It must be at the end, to avoid being overriden by default
;; settings.

;; If we're running on a HiDPI machine, we replace the flycheck fringe
;; bitmap with a larger version.

(when (string-prefix-p  "maladict" system-name)
  (setq fringe-mode-explicit t)
  (set-fringe-mode '(16 . 0))

  (with-eval-after-load "flycheck"
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      (vector
       #b1000000000
       #b1100000000
       #b1110000000
       #b1111000000
       #b1111100000
       #b1111110000
       #b1111111000
       #b1111111100
       #b1111111110
       #b1111111111
       #b1111111111
       #b1111111110
       #b1111111100
       #b1111111000
       #b1111110000
       #b1111100000
       #b1111000000
       #b1110000000
       #b1100000000
       #b1000000000)
      20 10 'center)))

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
