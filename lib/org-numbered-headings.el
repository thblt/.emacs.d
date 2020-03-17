;;; org-numbered-headings.el --- Numbered headings for Org-Mode, who'd have guessed? (using overlays!)

;;; Commentary:

;; This heavily experimental package provides automatically numbered
;; headings for Org-Mode using text overlays (it thus doesn't modify
;; the buffer contents in any way).  It supports the =:UNNUMBERED:==
;; keyword, offers a lot of numbering styles, customizable resets, etc.

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
  "Format N with FORMAT."
  (unless (or (eq format 'arabic) (> 0 n))
    (error "Invalid argument %s, expected a positive integer or 'arabic format." n))
  (cond
   ((eq format 'Alpha) (char-to-string (+ ?A (- n 1))))
   ((eq format 'Greek) (char-to-string (+ ?Α (- n 1))))
   ((eq format 'Roman) (upcase (org-export-number-to-roman n)))
   ((eq format 'alpha) (char-to-string (+ ?a (- n 1))))
   ((eq format 'arabic) (number-to-string n))
   ((eq format 'greek) (char-to-string (+ ?α (- n 1))))
   ((eq format 'roman) (downcase (org-export-number-to-roman n)))))

(onh-format-number 'Greek 0)
(onh-format-number 'arabic -3)

(defmacro thblt/step (n)
  "Increase N, a symbol, by 1 in-place.  Return the new value."
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
