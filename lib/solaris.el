(defun eziam--heading-height (x) x)

(defvar solaris-theme
  '("Solaris, like Solarized, but with less fruit salad."
    ;; `default :foreground ,base0 :background ,base03
    (let
        ((ol-gray-blocks-1 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.6) :foreground ,base03 :weight normal :slant normal :overline nil :underline ,base01 :extend t))))
         (ol-gray-blocks-2 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.4) :foreground ,magenta :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-3 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.2) :foreground ,violet :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-4 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.0) :foreground ,blue :weight normal :slant italic :overline nil :underline ,base01))))
         (ol-gray-blocks-5 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.0) :foreground ,cyan :weight bold   :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-6 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.0) :foreground ,green :weight normal :slant normal :overline nil :underline ,base01))))
         (ol-gray-blocks-7 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.0) :foreground ,yellow ,cyan :weight normal :slant italic :overline nil :underline ,base01))))
         (ol-gray-blocks-8 `((,class (:inherit variable-pitch :background ,base3 :height ,(eziam--heading-height 1.0) :foreground ,orange ,green :weight normal :slant italic :overline nil :underline nil)))))
      (custom-theme-set-faces
       theme-name
     ;;;; modeline
       `(mode-line ((,class (:foreground ,base01 :background ,base3 :underline ,base02 :overline ,base02))))
       `(mode-line-inactive ((,class (:backround ,base02 :foreground ,base01 :underline ,base01 :overline ,base01))))
       `(mode-line-highlight ((,class (:foreground ,base01 :weight ,s-maybe-bold))))
       `(mode-line-buffer-id ((,class (:foreground ,base02 :weight bold))))
     ;;;; font lock
       `(font-lock-comment-face ((,class (:foreground ,blue-2fg :slant italic))))
       `(font-lock-comment-delimiter-face ((,class (:foreground ,blue-2fg :slant italic))))
       `(font-lock-constant-face ((,class (:slant italic))))
       `(font-lock-doc-face ((,class (:foreground ,green))))
       `(font-lock-function-name-face ((,class (:foreground ,base2))))
       `(font-lock-keyword-face ((,class (:weight ,s-maybe-bold))))
       `(font-lock-preprocessor-face ((,class (:foreground ,base1))))
       `(font-lock-string-face ((,class (:slant italic))))
       `(font-lock-type-face ((,class (:underline t))))
       `(font-lock-variable-name-face ((,class ())))
     ;;;; erc
       `(erc-notice-face ((,class (:foreground ,base01))))
       `(erc-prompt-face ((,class (:background ,base02 :foreground ,base0))))
       `(erc-input-face ((,class (:foreground ,base1))))
       `(erc-timestamp-face ((,class (:foreground ,base02))))
       `(erc-my-nick-face ((,class (:foreground ,base2))))
       `(erc-current-nick-face ((,class (:inverse-video t))))
       `(erc-nick-default-face ((,class (:weight ,s-maybe-bold))))
       ;;;; org headings
       `(org-level-1 ,ol-gray-blocks-1)
       `(org-level-2 ,ol-gray-blocks-2)
       `(org-level-3 ,ol-gray-blocks-3)
       `(org-level-4 ,ol-gray-blocks-4)
       `(org-level-5 ,ol-gray-blocks-5)
       `(org-level-6 ,ol-gray-blocks-6)
       `(org-level-7 ,ol-gray-blocks-7)
       `(org-level-8 ,ol-gray-blocks-8)

       ))))

(provide 'solaris)
