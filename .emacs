;; SOUR TODO -
;; 1. Make a function which searches and highlights a given word. It should unhighlight all other highlights first.
;; 2. In term-mode, make keybindings to easily switch between char mode and evil insert mode, to line mode and evil normal mode.
;;    ie when I'm in char mode, it should do Esc -> C-c C-j and when I'm in line mode it should do C-c C-k G A
;; 3. Make a function which does M-x term and immediately M-x rename-buffer

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

;; Move to first non whitespace char in line
(global-set-key (kbd "M-a") 'back-to-indentation)

;; Change some basic word level keybindings
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

;; Indentation
(setq c-default-style "stroustrup")

;; FM Emacs Mode
(if (getenv "VOB_ROOT")
    (progn (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm-c-mode.el"))
	   (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/p4.el"))
	   (load-file (concat (getenv "VOB_ROOT") "/formal/bin/emacs/fm.el")))
    (print "Could not find Formality files"))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

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

;; Highlight.el
(defun my/unhighlight-then-highlight-regexp ()
  "Unhighlight all highlights then highlight given regexp"
  (hlt-unhighlight-region)
  (interactive)
  (call-interactively 'hlt-highlight-regexp-region))
(global-set-key (kbd "M-p") 'hlt-previous-highlight)
(global-set-key (kbd "M-n") 'hlt-next-highlight)
;; (global-set-key (kbd "C-x h") 'hlt-highlight-regexp-region)
(global-set-key (kbd "C-x h") 'my/unhighlight-then-highlight-regexp)
(global-set-key (kbd "C-x C-h") 'hlt-unhighlight-region)
(global-set-key (kbd "C-x H") 'hlt-highlight-symbol)

;; Buffer list - switch to other window by default with C-x C-b
(defun my/buffer-list-and-switch ()
  "Open buffer list in other window and switch to it automatically"
  (interactive)
  (list-buffers)
  (switch-to-buffer-other-window "*Buffer List*"))
(global-set-key (kbd "C-x C-b") 'my/buffer-list-and-switch)

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
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

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
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(fido-mode nil)
 '(global-auto-complete-mode nil)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(ido-mode nil nil (ido))
 '(inhibit-startup-screen t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(company use-package icomplete-vertical highlight evil-visualstar evil auto-complete gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t nil))))
