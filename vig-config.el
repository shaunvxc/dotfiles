;;; emacs-config --- my emacs config
;;; Commentary:
;;; Code:

(with-no-warnings
   (require 'cl))

(defvar package-archives)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")

   t)
  (package-initialize))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; configure load path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/elpa/company-20160429.1339")

;; any syntax highlight theme lives here..
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (require 'god-mode)

(require 'midnight)
(midnight-delay-set 'midnight-delay "12:28am")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "e707d8881f53535f1cbbfc70d81cf5a7cd23eefe271b084bf4ff0bd21dfd0311" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice "~/src/")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; favorite theme I've found so far
;;(load-theme 'tomorrow-night-eighties t)
(load-theme 'dark-mint t)
;; (load-theme 'github t)
;; ;;; Set background to be transparent
;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
;; (add-to-list 'default-frame-alist '(alpha 85 85))

;;; show line nums
(require 'linum)
(global-linum-mode 1)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; autocomplete using company package
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; use emacs helm
(require 'helm)
(require 'helm-config)


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; helm mode enabled
(helm-mode 1)

;;; smooth scroll
(setq scroll-conservatively 10000)

;; prevent mouse scrolling from sucking ass
(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)

;; turn the friggan beep noise off!
(setq ring-bell-function 'ignore)

;; Emacs will not automatically add new lines
(setq next-line-add-newlines nil)

;; prevent too much line wrapping...
'(fill-column 1000)

(defun get-default-height ()
  "Nice size for the default window."
  (/ (- (display-pixel-height) 120)
          (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

  ;; remove errant white space at the ends of file
(defun squeeze-file ()
  "Deletes all stray whitespace from the current buffer."
  (interactive)
  (delete-trailing-whitespace)
  (delete-trailing-blank-lines))

;; remove any whitespace at the ends of lines
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one."
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


;; keybinding for arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; syntax highlighting for c#
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

(fset 'toggle-pdb
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 112 100 98 59 112 100 98 right 1 67108896 5 134217848 99 111 109 109 101 110 116 tab 111 114 tab return 5 down up down 1] 0 "%d")) arg)))

(fset 'toggle-all-pdb
    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217831 134217831 49 return 134217777 48 48 48 48 48 134217848 116 111 103 tab 112 100 tab return 1 1 1] 0 "%d")) arg)))

(global-set-key [(ctrl x) (t)] 'toggle-all-pdb)

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/.emacs.d/site-lisp/magit/Documentation/"))

(global-set-key (kbd "C-x g") 'magit-status)

; perfect magit display config
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))

(setq split-height-threshold 80)
(setq split-width-threshold 100)

(defun split-window-prefer-vertically (window)
  "If there's only one window (excluding any possibly active minibuffer), then split [current] WINDOW vertically."
  (if (and (one-window-p t)
	   (not (active-minibuffer-window)))
      (let ((split-width-threshold nil))
	(split-window-sensibly window))
    (split-window-sensibly window)))

(setq split-window-preferred-function 'split-window-prefer-vertically)

(add-to-list 'display-buffer-alist
                 '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                        ((inhibit-same-window . t)))))

;; font for all unicode characters
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)

;; use powerline
(require 'powerline)
(powerline-default-theme)

;; org mode configs
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; (defun create-tags (dir-name)
;;      "Create tags file for DIR-NAME."
;;      (interactive "DDirectory: ")
;;      (eshell-command
;;       (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))


;; (defadvice find-tag (around refresh-etags activate)
;;   "Rerun etags and reload tags if tag not found and redo 'find-tag'.
;; If buffer is modified, ask about save before running etags."
;;   (let ((extension (file-name-extension (buffer-file-name))))
;;     (condition-case err
;; 	ad-do-it
;;       (error (and (buffer-modified-p)
;; 		  (not (ding))
;; 		  (y-or-n-p "Buffer is modified, save it? ")
;; 		  (save-buffer))
;; 	     (er-refresh-etags extension)
;; 	     ad-do-it))))


;; (defun er-refresh-etags (&optional extension)
;;   "Refresh etags for all files of EXTENSION.")
;;   (shell-command (format "etags *.%s" (or extension "el")))
;;   (let ((tags-revert-without-query t))  ; don't query, revert silently
;;     (visit-tags-table default-directory nil))
;; )

(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x f") 'find-file-in-project)

(dumb-jump-mode)

;; need to move all key bindings to the same place

(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)

;; unset the annoying minimize keybindings
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; helm-keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x o") 'helm-occur)
(global-set-key (kbd "C-s") 'helm-occur)
;; re-bind isearch forward
(global-set-key (kbd "C-c s") 'isearch-forward)


(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; map break-point macro to C-x p
(global-set-key [(ctrl x) (p)] 'insert_bpt)

;; comment or uncomment blocks
(global-set-key [(ctrl c) (c)] 'comment-or-uncomment-region)
(global-set-key (kbd"C-X SPC") 'pop-global-mark)

;;; insert break point
(fset 'insert_bpt
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("import pdb;pdb.set_trace()" 0 "%d")) arg)))

;; function to fix brace alignment in c# mode
(fset 'fix_cs_braces
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 123 67108896 18 41 right backspace 32 right 5 down] 0 "%d")) arg)))

;; map curly brace alignment macro to C-c f
(global-set-key [(ctrl c) (f)] 'fix_cs_braces)

;; add new lines for C-n if the point is at the end of the buffer
(setq next-line-add-newlines t)
(global-set-key "\M-`" 'other-frame)
(global-set-key (kbd "M-p") 'ace-window)

(nyan-mode)
(provide '.emacs)
;;; vig-config ends here
