(defun solaris--heading-height (x) x)
(setq solarized-high-contrast-mode-line t)
(defvar solaris-theme
  '("Solaris, like Solarized, but with less fruit salad."
    ;; `default :foreground ,base0 :background ,base03
    (let
        ((ol-gray-blocks-1 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.6) :foreground ,base0 :weight normal :slant normal :overline nil :underline ,base01 :extend t))))
         (ol-gray-blocks-2 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.4) :foreground ,magenta-1fg :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-3 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.2) :foreground ,violet-1fg :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-4 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.0) :foreground ,blue-1fg :weight normal :slant italic :overline nil :underline ,base01))))
         (ol-gray-blocks-5 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.0) :foreground ,cyan-1fg :weight bold   :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-6 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.0) :foreground ,green-1fg :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-7 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.0) :foreground ,yellow-1fg :weight normal :slant italic :overline nil :underline ,base01))))
         (ol-gray-blocks-8 `((,class (:inherit variable-pitch :background ,base02 :height ,(solaris--heading-height 1.0) :foreground ,orange-1fg :weight normal :slant italic :overline nil :underline nil)))))
      (custom-theme-set-faces
       theme-name
       ;;;; font lock
       `(font-lock-comment-face ((,class (:foreground ,green :slant italic))))
       `(font-lock-comment-delimiter-face ((,class (:foreground ,green :slant italic))))
       `(font-lock-constant-face ((,class (:slant italic))))
       `(font-lock-doc-face ((,class (:foreground ,blue-2fg))))
       `(font-lock-function-name-face ((,class (:foreground ,base2))))
       `(font-lock-keyword-face ((,class (:weight ,s-maybe-bold))))
       `(font-lock-preprocessor-face ((,class (:foreground ,base1))))
       `(font-lock-string-face ((,class (:slant italic))))
       `(font-lock-type-face ((,class (:underline t))))
       `(font-lock-variable-name-face ((,class ())))
       ;;;; modeline
       ;; `(mode-line ((,class (:foreground ,base01 :background ,base3 :underline ,base02 :overline ,base02))))
       ;; `(mode-line-highlight ((,class (:foreground ,base01 :weight ,s-maybe-bold))))
       ;; `(mode-line-inactive ((,class (:backround ,base02 :foreground ,base01 :underline ,base01 :overline ,base01))))
       ;;       `(mode-line-buffer-id ((,class (:inverse-video t :weight bold))))
       `(header-line ((,class (:background ,base02 :underline ,base01))))
       ;;;; erc
       `(erc-notice-face ((,class (:foreground ,base01))))
       `(erc-prompt-face ((,class (:background ,base02 :foreground ,base0))))
       `(erc-input-face ((,class (:foreground ,base1))))
       `(erc-timestamp-face ((,class (:foreground ,base02))))
       `(erc-my-nick-face ((,class (:foreground ,base2))))
       `(erc-current-nick-face ((,class (:inverse-video t))))
       `(erc-nick-default-face ((,class (:weight ,s-maybe-bold))))
       ;;;; latext
       `(font-latex-string-face ((,class (:background ,base02))))
       ;;;; links
       `(link ((,class (:foreground ,blue :underline t))))
       ;;;; line numbers
       ;; `(line-number ((,class (:weight normal :underline nil :foreground ,s-fringe-fg :background ,s-fringe-bg))))
       ;; mu4e
       `(mu4e-unread-face ((,class (:weight bold :foreground ,base1))))
       ;;;; org
       `(org-level-1 ,ol-gray-blocks-1)
       `(org-level-2 ,ol-gray-blocks-2)
       `(org-level-3 ,ol-gray-blocks-3)
       `(org-level-4 ,ol-gray-blocks-4)
       `(org-level-5 ,ol-gray-blocks-5)
       `(org-level-6 ,ol-gray-blocks-6)
       `(org-level-7 ,ol-gray-blocks-7)
       `(org-level-8 ,ol-gray-blocks-8)
       `(org-todo ((,class (:foreground ,base02 :background ,red :underline ,base01))))
       `(org-done ((,class (:inherit org-todo :background ,green))))
       `(org-block ((,class (:background ,base02 :extend t))))
       `(org-block-begin-line ((,class (:background ,base02 :foreground ,base01 :underline ,base01 :extend t))))
       `(org-block-end-line ((,class (:inherit org-block-begin-line :underline nil :overline ,base01))))
       ;;;; outshine/outline-minor
       `(outline-minor-1 ,ol-gray-blocks-1)
       `(outline-minor-2 ,ol-gray-blocks-2)
       `(outline-minor-3 ,ol-gray-blocks-3)
       `(outline-minor-4 ,ol-gray-blocks-4)
       `(outline-minor-5 ,ol-gray-blocks-5)
       `(outline-minor-6 ,ol-gray-blocks-6)
       `(outline-minor-7 ,ol-gray-blocks-7)
       `(outline-minor-8 ,ol-gray-blocks-8)
       `(outshine-level-1 ,ol-gray-blocks-1)
       `(outshine-level-2 ,ol-gray-blocks-2)
       `(outshine-level-3 ,ol-gray-blocks-3)
       `(outshine-level-4 ,ol-gray-blocks-4)
       `(outshine-level-5 ,ol-gray-blocks-5)
       `(outshine-level-6 ,ol-gray-blocks-6)
       `(outshine-level-7 ,ol-gray-blocks-7)
       `(outshine-level-8 ,ol-gray-blocks-8)
       ;;;; which-function
       `(which-func ((,class (:foreground ,base01))))))))
(provide 'solaris)
