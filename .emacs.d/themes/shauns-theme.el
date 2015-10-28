;;; shauns-theme.el --- shaun's emacs theme

;; Copyright (C) 2015 by shau nviguerie


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of vim-colors theme from `color-themes'

;;; Code:
(deftheme shauns
  "shauns-theme")

(custom-theme-set-faces
 'shauns
 
 '(default ((t (:background "#ffffff" :foreground "#000000"))))
 '(mouse ((t (:foregound "#000000"))))
 '(cursor ((t (:foregound "#000000"))))
 '(border ((t (:foregound "black"))))

 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

(set-face-foreground 'font-lock-type-face "MintCream")
(set-face-foreground 'font-lock-comment-face "honeydew")
(set-face-foreground 'font-lock-function-name-face "VioletRed2")
(set-face-foreground 'font-lock-keyword-face "cyan")
(set-face-foreground 'font-lock-string-face "deep pink")
(set-face-foreground 'font-lock-variable-name-face "MediumPurple1")

(set-face-background 'region "MidnightBlue")
(set-face-background 'secondary-selection "dodger blue")
(set-mouse-color "wheat")
(set-face-foreground 'highlight "orange")
(set-face-background 'highlight "blue")
(set-face-foreground 'show-paren-match-face "coral")
(set-face-background 'show-paren-match-face "turquoise")
(set-cursor-color "Deep Pink")
