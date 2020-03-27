(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-light "The light variant of the Solaris colour theme")
(solarized-with-color-variables
  'light 'solaris-light solarized-light-color-palette-alist solaris-theme)

(provide-theme 'solaris-light)
