(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-zenburn "The zenburn variant of the Solaris colour theme")
(solarized-with-color-variables
  'zenburn 'solaris-zenburn solarized-zenburn-color-palette-alist solaris-theme)

(provide-theme 'solaris-zenburn)
