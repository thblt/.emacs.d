(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-dark "The dark variant of the Solaris colour theme")
(solarized-with-color-variables
  'dark 'solaris-dark solarized-dark-color-palette-alist solaris-theme)

(provide-theme 'solaris-dark)
