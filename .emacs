;; for emacs 24.3, the below is required or calls to 'loop' will throw errors
(require 'cl)

;; git commit package
(add-to-list 'load-path "~/.emacs.d")
(require 'git-commit)
(setq git-commit-fill-column-summary 72)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; sets the default directory as the home directory
(setq default-directory "~/")

(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'solarized-dark t)

;; MY OLD CUSTOM SYNTAX highlighting theme

;; (set-background-color "black")
;; (set-foreground-color "green")

;; ;;; Set background to be transparent
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 92 92))

;;; syntax highlighting
;; (set-face-foreground 'font-lock-type-face "MintCream")
;; (set-face-foreground 'font-lock-comment-face "honeydew")
;; (set-face-foreground 'font-lock-function-name-face "VioletRed2")
;; (set-face-foreground 'font-lock-keyword-face "cyan")
;; (set-face-foreground 'font-lock-string-face "deep pink")
;; (set-face-foreground 'font-lock-variable-name-face "MediumPurple1")


;; (set-face-background 'region "MidnightBlue")
;; (set-face-background 'secondary-selection "dodger blue")
;; (set-mouse-color "wheat")
;; (set-face-foreground 'highlight "orange")
;; (set-face-background 'highlight "blue")
;; (set-face-foreground 'show-paren-match-face "coral")
;; (set-face-background 'show-paren-match-face "turquoise")
;; (set-cursor-color "Deep Pink")

;;; show line nums
(require 'linum)
(global-linum-mode 1)


;;; smooth scroll
(setq scroll-conservatively 10000)

;;; Nice size for the default window
(defun get-default-height ()
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

;;; hide toolbar
(tool-bar-mode -1)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'control) ; was alot
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'alt) ;fn is control -- was control
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char))

; other window
(global-set-key [C-tab] 'other-window)

(global-auto-revert-mode t)

(defun squeeze-file ()
  (interactive)
  (delete-trailing-whitespace)
  (delete-trailing-blank-lines))


(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))

;;; maps the key-binding for the above function that removes all white space
(global-set-key [(ctrl x) (w)] 'squeeze-file)
;; delete selection mode
(delete-selection-mode 1)

;;; insert break point
(fset 'insert_bpt
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("import pdb;pdb.set_trace()" 0 "%d")) arg)))

;; map break-point macro to C-x p
(global-set-key [(ctrl x) (p)] 'insert_bpt)

;; comment or uncomment blocks
(global-set-key [(ctrl c) (c)] 'comment-or-uncomment-region)


