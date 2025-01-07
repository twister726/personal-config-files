;;  TODO -
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
(evil-set-initial-state 'vterm-mode 'emacs)

;; Evil packages
(require 'evil-visualstar) ; To search selection with *
(global-evil-visualstar-mode t)

;; Evil keybindings
(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
(define-key evil-normal-state-map (kbd "M-u") 'evil-redo)
;(define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
;(define-key evil-normal-state-map (kbd "\)") 'evil-beginning-of-line)

(defun my/tab-jump-or-org-cycle ()
    "jumps to beginning of line in all modes except org mode, where it cycles"
    (interactive)
    (if (equal major-mode 'org-mode)
        (org-cycle)
        (evil-first-non-blank)))
(define-key evil-normal-state-map (kbd "<tab>") 'my/tab-jump-or-org-cycle)

;; Redefine evil mark jump to also center screen (from evil-commands.el)
(evil-define-command my/evil-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR, and then recenter the screen"
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (evil-goto-mark char noerror)
  (evil-first-non-blank)
  (recenter))

(define-key evil-normal-state-map (kbd "'") 'my/evil-goto-mark-line)

;; Redefine evil goto line to also center screen (from evil-commands.el)
(evil-define-motion my/evil-goto-line (count)
  "Go to line COUNT. By default the last line. Then recenter screen"
  :jump t
  :type line
  (evil-ensure-column
    (if (null count)
        (goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- count)))
    (recenter)))

(evil-define-motion my/evil-goto-first-line (count)
  "Go to line COUNT. By default the first line."
  :jump t
  :type line
  (evil-goto-line (or count 1))
  (recenter))

(define-key evil-normal-state-map (kbd "G") 'my/evil-goto-line)
(define-key evil-motion-state-map (kbd "g g") 'my/evil-goto-first-line)

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

;; Dired ls settings
; (setq dired-listing-switches "-aBh  --group-directories-first -lat --time-style=+\"%a %b %d\"")
; (setq dired-listing-switches "-aBh  --group-directories-first -lat")
(setq dired-listing-switches "-aBh -lat")

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
(global-set-key (kbd "C-x C-b") 'my/buffer-list-and-switch)

;; Copy current file name to clipboard
(defun my/copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key (kbd "C-x C-p") 'my/copy-file-name-to-clipboard)

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

;; Quick buffer switching
(global-set-key (kbd "M-2") 'switch-to-buffer)
(global-set-key (kbd "M-4") 'switch-to-buffer-other-window)

;; Quick file switching
(global-set-key (kbd "M-3") 'find-file)
(global-set-key (kbd "M-5") 'find-file-other-window)

;; Swap C-x o and C-x C-o
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x o") 'delete-blank-lines)

;; Switch between recently used buffers quickly
(global-set-key (kbd "C-x C-h") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)
(global-set-key (kbd "C-x C-j") 'mode-line-other-buffer)
(global-set-key (kbd "M-6") 'mode-line-other-buffer)
(global-set-key (kbd "M-7") 'revert-buffer-no-confirm)
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

;; Dabbrev
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)

;;;;;;;;;;; Fancy Dabbrev ;;;;;;;;;;;

;;;;;; My old configuration ;;;;;;
;; Load fancy-dabbrev.el:
;; (require 'fancy-dabbrev)
;; 
;; ;; Enable fancy-dabbrev previews everywhere:
;; (global-fancy-dabbrev-mode)
;; 
;; ;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
;; ;; choice, here "TAB" and "Shift+TAB":
;; (global-set-key (kbd "TAB") 'fancy-dabbrev-expand)
;; (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
;; 
;; ;; If you want TAB to indent the line like it usually does when the cursor
;; ;; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
;; ;; instead of `fancy-dabbrev-expand`:
;; (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
;; (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)

;;;;;; From ideasman_42 Reddit ;;;;;;

;; (use-package fancy-dabbrev
;;   :commands (fancy-dabbrev-mode)
;;   :config
;;   (setq fancy-dabbrev-preview-delay 0.1)
;;   (setq fancy-dabbrev-preview-context 'before-non-word)
;; 
;;   (setq fancy-dabbrev-expansion-on-preview-only t)
;;   ;; (setq fancy-dabbrev-indent-command 'tab-to-tab-stop)
;;   (setq fancy-dabbrev-indent-command 'indent-for-tab-command)
;; 
;;   (define-key evil-insert-state-map (kbd "<tab>") 'fancy-dabbrev-expand-or-indent))

;; Only while in evil insert mode.
;(with-eval-after-load 'evil
;  (add-hook 'evil-insert-state-entry-hook (lambda () (fancy-dabbrev-mode 1)))
;  (add-hook 'evil-insert-state-exit-hook (lambda () (fancy-dabbrev-mode 0))))
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
;(setq lazy-highlight-initial-delay 0)

;; Regex search lax whitespace
(setq isearch-regexp-lax-whitespace 1)

;; Immediately change direction when reversing search direction
(setq isearch-repeat-on-direction-change t)

;; Don't issue error when isearch reaches last match
(setq isearch-wrap-pause 'no)

;; isearch highlight full buffer
;(setq lazy-highlight-buffer t)
;(setq lazy-highlight-buffer-max-at-a-time 100)

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
; (global-set-key (kbd "M-p") 'isearch-ring-retreat)
; (global-set-key (kbd "M-n") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "M-n") 'isearch-ring-advance)

;; Slightly change shortcut for isearch search for symbol at point
(global-set-key (kbd "M-s M-.") 'isearch-forward-symbol-at-point)

;; Shortcut for rgrep
(global-set-key (kbd "C-c C-r") 'rgrep)

;; Isearch change backspace to del last char instead of undo input item
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

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

;; vterm
(use-package vterm
    :ensure t
    :config
    (define-key vterm-mode-map (kbd "<tab>") 'vterm-send-tab)
    (define-key vterm-mode-map (kbd "M-1") 'delete-other-windows)
    (define-key vterm-mode-map (kbd "M-2") 'switch-to-buffer)
    (define-key vterm-mode-map (kbd "M-3") 'find-file)
    (define-key vterm-mode-map (kbd "M-4") 'switch-to-buffer-other-window)
    (define-key vterm-mode-map (kbd "M-5") 'find-file-other-window)
    (define-key vterm-mode-map (kbd "M-0") 'my/delete-window-automatically-resize))

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

;; Org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org hide markup characters
(setq org-hide-emphasis-markers t)

;; Org TODO keywords (from https://doc.norang.ca/org-mode.html)
(setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "BLOCKED" "DONE")))

;; Org TODO keyword faces (from https://github.com/james-stoup/emacs-org-mode-tutorial)
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ; ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ; ("NEXT" . (:foreground "blue" :weight bold))
        ("NEXT" . (:foreground "LightBlue" :weight bold))
        ("WAITING" . (:foreground "Cyan" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ; ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ; ("OBE" . (:foreground "LimeGreen" :weight bold))
        ; ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
        ))

;; IBuffer mode bindings
;(require 'ibuffer)
;(define-key ibuffer-mode-map (kbd "M-o") 'other-window)
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-o") 'other-window))

;; Function to surround region with strings
;; From arialdomartini.github.io
(defun surround-region--surround (delimiters)
  "Surround the active region with hard-coded strings"
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end))
            (opening-delimiter (car delimiters))
            (closing-delimiter (cdr delimiters)))

        (goto-char beginning)
        (insert opening-delimiter)

        (goto-char (+ end (length closing-delimiter)))
	(insert closing-delimiter)))))

(defun surround-region--ask-delimiter ()
  (let ((choices '(("<<< and >>>" . ("<<<" . ">>>"))
                   ("double quotes: \"\"" . ("\"" . "\""))
                   ("markdown source block: ```emacs-lisp" . ("```emacs-lisp" . "```"))
                   ("comment: *\ /*" . ("/*" . "*/"))
                   ("spaces:   " . (" " . " "))
                   ("bold: * *" . ("*" . "*")))))
    (alist-get 
     (completing-read "Your generation: " choices )
     choices nil nil 'equal)))

(defun surround-region-with-hard-coded-strings (delimiters)
  "Surround the active region with hard-coded strings"
  (interactive (list (surround-region--ask-delimiter)))
  (surround-region--surround delimiters))

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ; (doom-themes-neotree-config)
  ;; or for treemacs users
  ; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.2)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(blink-cursor-mode nil)
 '(company-occurrence-weight-function 'company-occurrence-prefer-closest-above)
 '(company-selection-wrap-around t)
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "f828930c293178ba41ebfdec2154bfdb77cdf3df2157a316940860d7f51dda61" "e1da45d87a83acb558e69b90015f0821679716be79ecb76d635aafdca8f6ebd4" "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93" "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(dabbrev-ignored-buffer-regexps '(".*\\.log"))
 '(exwm-floating-border-color "#191b20")
 '(fancy-dabbrev-no-expansion-for '(multiple-cursors-mode Shell-mode))
 '(fancy-dabbrev-no-preview-for '(iedit-mode isearch-mode multiple-cursors-mode Shell-mode))
 '(fci-rule-color "#5B6268")
 '(fido-mode nil)
 '(global-auto-complete-mode nil)
 '(global-company-mode nil)
 '(global-display-line-numbers-mode t)
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(ido-mode nil nil (ido))
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files
   '("~/stars/non_determinism.org" "~/tasks.org" "~/stars/many_to_many_tech_talk.org" "~/stars/dpx_contributing_tasks.org" "~/stars/set_compare_rule.org" "~/stars/report_pre_svf_names.org" "~/stars/matching_issue.org"))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(doom-themes leuven-theme vterm corfu use-package icomplete-vertical evil-visualstar evil auto-complete gruvbox-theme))
 '(pdf-view-midnight-colors '("#282828" . "#f2e5bc"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(search-exit-option nil)
 '(shell-command-prompt-show-cwd t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(cursor ((t nil)))
 '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 2.0 :width normal))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.05)))))
