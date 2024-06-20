;; SOUR TODO -
;; 1. In term-mode, make keybindings to easily switch between char mode and evil insert mode, to line mode and evil normal mode.
;;    ie when I'm in char mode, it should do Esc -> C-c C-j and when I'm in line mode it should do C-c C-k G A
;; 2. Make a function which does M-x term and immediately M-x rename-buffer

(require 'misc)

(require 'package)
(package-initialize)

;; use-package
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(tool-bar-mode -1) ; Disable the toolbar

(electric-pair-mode 1) ; Close brackets automatically

(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

;; auto-complete
;; (require 'auto-complete)
;; (auto-complete-mode 1) ; Enable auto-complete
;; (ac-config-default)
;; (global-auto-complete-mode nil)

;; Evil mode
(require 'evil)
(evil-mode 1)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'emacs)
(evil-set-initial-state 'bookmark-bmenu-mode 'emacs)
(evil-set-initial-state 'Buffer-menu-mode 'emacs)
(evil-set-initial-state 'global-eldoc-mode 'emacs)
;; (evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'profiler-report-mode 'emacs)

;; Evil packages
(require 'evil-visualstar) ; To search selection with *
(global-evil-visualstar-mode t)

;; Evil keybindings
(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
(define-key evil-normal-state-map (kbd "M-u") 'evil-redo)

;; Scroll half page instead of full page
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)
(global-set-key (kbd "M-j") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)

;; Show which function we are in
(which-function-mode 1)
(setq which-func-unknown "")

;; World clocks
(setq world-clock-list
    '(
      ("US/Hawaii" "Hawaii")
      ("US/Pacific" "U.S. Pacific")
      ("US/Mountain" "U.S. Mountain")
      ("US/Central" "U.S. Central")
      ("US/Eastern" "U.S. Eastern")
      ("Chile/Continental" "Chile")
      ("Europe/London" "UK")
      ("Europe/Copenhagen" "Central Europe")
      ("Israel" "Israel")
      ("Asia/Calcutta" "India")
      ("Asia/Taipei" "China / Taiwan")
      ("Asia/Seoul" "Japan / Korea")
      ))

(setq world-clock-time-format "%a, %b %d %Y %I:%M %p %Z")

;; The vc-refresh-state function in find-file-hook seems to slow down
;; Emacs when opening files.
(remove-hook 'find-file-hook 'vc-refresh-state)

;; fix for slow emacs when saving files
(setq vc-handled-backends nil)

; Move between windows
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

; Better binding for other-window
(global-set-key (kbd "M-o")  'other-window)

; Balance windows
(global-set-key (kbd "M-=")  'balance-windows)

;; Move to first non whitespace char in line
(global-set-key (kbd "M-a") 'back-to-indentation)

;; Change some basic word level keybindings
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

;; Indentation
(setq c-default-style "stroustrup")

;; FM Emacs Mode
(when (getenv "VOB_ROOT")
    (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm-c-mode.el"))
    (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/p4.el"))
    (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm.el")) )

(require 'dired)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Show directories first in dired
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; Dired sort by date by default
(setq dired-listing-switches "-lt")

;; Dired keybindings
(define-key dired-mode-map (kbd "f") 'dired-goto-file)
(define-key dired-mode-map (kbd "j") 'dired-next-line)
(define-key dired-mode-map (kbd "k") 'dired-previous-line)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)
; "p" and "n" are free now

;; Display line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Unhighlight all highlights with M-s h U (Hi-Lock)
(require 'hi-lock)        
(defun my/unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
  (interactive)
  (unhighlight-regexp t))
(define-key search-map "hU" #'my/unhighlight-all-in-buffer)

;; Highlight.el (PACKAGE DISABLED FOR NOW)
;; (defun my/unhighlight-then-highlight-regexp ()
;;   "Unhighlight all highlights then highlight given regexp"
;;   (hlt-unhighlight-region)
;;   (interactive)
;;   (call-interactively 'hlt-highlight-regexp-region)
;;   (hlt-next-highlight))
;; (defun my/unhighlight-then-highlight-symbol-at-point ()
;;   "Unhighlight all highlights then highlight symbol at point"
;;   (hlt-unhighlight-region)
;;   (interactive)
;;   (call-interactively 'hlt-highlight-symbol))
;; (global-set-key (kbd "M-p") 'hlt-previous-highlight)
;; (global-set-key (kbd "M-n") 'hlt-next-highlight)
;; ;; (global-set-key (kbd "C-x h") 'hlt-highlight-regexp-region)
;; (global-set-key (kbd "C-x h") 'my/unhighlight-then-highlight-regexp)
;; (global-set-key (kbd "C-x C-h") 'hlt-unhighlight-region)
;; (global-set-key (kbd "C-x H") 'my/unhighlight-then-highlight-symbol-at-point)

;; Buffer list - switch to other window by default
(defun my/buffer-list-and-switch ()
  "Open buffer list in other window and switch to it automatically"
  (interactive)
  (list-buffers)
  (switch-to-buffer-other-window "*Buffer List*"))
(global-set-key (kbd "C-x b") 'my/buffer-list-and-switch)

;; Automatically balance windows after deleting a window
(defun my/delete-window-automatically-resize ()
  "Delete selected window, then automatically balance windows"
  (interactive)
  (delete-window)
  (balance-windows))

;; Better keybindings for delete window
(global-set-key (kbd "C-x 0") 'my/delete-window-automatically-resize)
(global-set-key (kbd "M-0") 'my/delete-window-automatically-resize)

;; Better keybinding for maximize windows
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Swap C-x b and C-x C-b
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Swap C-x o and C-x C-o
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x o") 'delete-blank-lines)

;; Switch between recently used buffers quickly
(global-set-key (kbd "C-x C-h") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)
(global-set-key (kbd "C-x C-j") 'mode-line-other-buffer)
(global-set-key (kbd "M-6") 'mode-line-other-buffer)
(global-set-key (kbd "C-6") 'mode-line-other-buffer)

;; Show icomplete completions vertically
(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ; ("j" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ; ("k" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

;; Company mode
;; (add-hook 'after-init-hook 'global-company-mode)
;; (company-mode 0)

;; Dabbrev
;; (global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;; (define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)

;;;;;;;;;;; Fancy Dabbrev ;;;;;;;;;;;
;; Load fancy-dabbrev.el:
(require 'fancy-dabbrev)

;; Enable fancy-dabbrev previews everywhere:
(global-fancy-dabbrev-mode)

;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
;; choice, here "TAB" and "Shift+TAB":
(global-set-key (kbd "TAB") 'fancy-dabbrev-expand)
(global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)

;; If you want TAB to indent the line like it usually does when the cursor
;; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
;; instead of `fancy-dabbrev-expand`:
(global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
(global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Term mode TAB completion
;; (add-hook 'term-mode-hook
;;    (lambda ()
;;      (define-key term-mode-map (kbd "TAB") 'term-dynamic-complete-filename)))

;; Change file backup directory
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Leave searched text highlighted
(setq lazy-highlight-cleanup nil)
(setq lazy-highlight-initial-delay 0)

;; Regex search lax whitespace
(setq isearch-regexp-lax-whitespace 1)

;; Immediately change direction when reversing search direction
(setq isearch-repeat-on-direction-change t)

;; Don't issue error when isearch reaches last match
(setq isearch-wrap-pause 'no)

;; isearch highlight full buffer
(setq lazy-highlight-buffer t)
(setq lazy-highlight-buffer-max-at-a-time nil)

;; Show isearch match count
(setq isearch-lazy-count t)

;; Automatically wrap isearch around (PROTECT THIS WITH YOUR LIFE)
(defun isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (goto-char isearch-other-end))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward))
  (recenter))

(defun isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward))
  (recenter))

(define-key isearch-mode-map (kbd "C-s") 'isearch-repeat-forward+)
(define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward+)

;; Isearch search ring
(global-set-key (kbd "M-p") 'isearch-ring-retreat)
(global-set-key (kbd "M-n") 'isearch-ring-advance)

;; Slightly change shortcut for isearch search for symbol at point
(global-set-key (kbd "M-s M-.") 'isearch-forward-symbol-at-point)

;; Shortcut for rgrep
(global-set-key (kbd "C-c C-r") 'rgrep)

;; Rotate windows
(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(global-set-key (kbd "C-c C-o") 'rotate-windows)

;; Show matching parentheses
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

;; Case insensitive search
(setq case-fold-search t)

;; Revert buffer no confirm
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Disable ido mode
(setq ido-everywhere nil)
(ido-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.2)
 '(blink-cursor-mode nil)
 '(company-occurrence-weight-function 'company-occurrence-prefer-closest-above)
 '(company-selection-wrap-around t)
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(fido-mode nil)
 '(global-auto-complete-mode nil)
 '(global-company-mode nil)
 '(global-display-line-numbers-mode t)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(ido-mode nil nil (ido))
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(fancy-dabbrev corfu use-package icomplete-vertical evil-visualstar evil auto-complete gruvbox-theme))
 '(search-exit-option nil)
 '(shell-command-prompt-show-cwd t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal))))
 '(cursor ((t nil))))
