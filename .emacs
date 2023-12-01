;; SOUR TODO -
;; 1. Make a function which searches and highlights a given word. It should unhighlight all other highlights first.
;; 2. In term-mode, make keybindings to easily switch between char mode and evil insert mode, to line mode and evil normal mode.
;;    ie when I'm in char mode, it should do Esc -> C-c C-j and when I'm in line mode it should do C-c C-k G A
;; 3. Make a function which does M-x term and immediately M-x rename-buffer

(require 'misc)

(package-initialize)

(tool-bar-mode -1) ; Disable the toolbar

(electric-pair-mode 1) ; Close brackets

(require 'auto-complete)
(auto-complete-mode 1) ; Enable auto-complete

;; Evil mode
(require 'evil)
(evil-mode 1)
;; (evil-mode 0)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'emacs)
(evil-set-initial-state 'bookmark-bmenu-mode 'emacs)
(evil-set-initial-state 'Buffer-menu-mode 'emacs)

;; Scroll half page instead of full page
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)
(global-set-key (kbd "M-j") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)

;; Move to first non whitespace char in line
(global-set-key (kbd "M-a") 'back-to-indentation)

;; Change some basic word level keybindings
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

;; Indentation
(setq c-default-style "stroustrup")

;; FM Emacs Mode
(load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm-c-mode.el"))
(load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/p4.el"))
(load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm.el"))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Display line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Unhighlight all highlights with M-s h U
(require 'hi-lock)        
(defun my/unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
  (interactive)
  (unhighlight-regexp t))
(define-key search-map "hU" #'my/unhighlight-all-in-buffer)

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

;; Show matching parentheses
(show-paren-mode 1)

;; Revert buffer no confirm
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(ido-mode 'buffer nil (ido))
 '(inhibit-startup-screen t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages '(evil auto-complete gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t nil))))
