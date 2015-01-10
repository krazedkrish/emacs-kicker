; emacs kicker --- kick start emacs setup
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/emacs-kicker
;; Created: 2011-04-15
;; Keywords: emacs setup el-get kick-start starter-kit
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; Modified by krazedkrish <krazedkrish@gmail.com>
;; For GNU Emacs
;; For starters replace evil with ergoemacs-mode

(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
;;  (url-retrieve-synchronously
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
     
; enable git shallow clone to save time and bandwidth
(setq el-get-git-shallow-clone t)

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit				; git meet emacs, and a binding
	  :after (progn
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   switch-window			; takes over C-x o
   auto-complete			; complete as you type with overlays
   yasnippet 				; powerful snippet mode
   emmet-mode                           ; zencode + others
   ;;color-theme		                ; nice looking emacs
   color-theme-solarized	        ; check out color-theme-solarize
   ;; color-theme-zenburn		        ; check out color-theme-zenburn
   evil					; vi mode
;;   helm					; powerful completion and selection narrowing framework
   minimap				; sublimetext-style minimap sidebar
   neotree				; emacs tree plugin like NERD tree
   highlight-symbol			; highlight the same symbols in code, navigate in them, or replace string 
   tabbar-ruler				;  
   highlight-indentation		; hightlight the indentations
   highlight-parentheses		; hightlight the parantheses
   multiple-cursors			; use multiple cursors to type
   web-mode				; smart html library supporting template engines
   ;; powerline				; emacs-powerline
   git-gutter                           ;highlight git changes
   
   ;; python special packages
   
   ;; ruby special package

   ;; java + android special packages
))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; load solarized theme
(setq hour 
        (string-to-number 
            (substring (current-time-string) 11 13))) ;;closes (setq hour...
    (if (member hour (number-sequence 6 17))
        (setq theme-to-load 'solarized-light)
        (setq theme-to-load 'solarized-dark))
(when (load-theme theme-to-load t))

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

(tool-bar-mode -1)			; no tool bar with icons
;;(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way

(global-hl-line-mode)			; highlight current line
(global-linum-mode 1)			; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;;(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; have vertical ido completion lists
(setq ido-decorations
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; switch-window configurations
(global-set-key (kbd "C-x o") 'switch-window)

;; enable evil mode
(evil-mode 1)

;; minimap
;;(setq minimap-window-location 'left)

;; neotree
(global-set-key [f12] 'neotree-toggle)

;; highlight-sysmbol configurations
    (global-set-key [(control f3)] 'highlight-symbol-at-point)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)


;; tabbar-ruler configuration
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;  (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;  (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;;  (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;;  (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                        ; scroll bar when your mouse is moving.
  (require 'tabbar-ruler)

;; for minimap
;; ###autoload
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
      (minimap-create)
    (minimap-kill)))

(defun minimap-toggle-retain-size ()
  "Toggle minimap"
  (interactive)
  (if (or (not (boundp 'minimap-exists))
	  (not minimap-exists))
      (progn (minimap-create)
	     (setf minimap-exists t)
	     (set-frame-width (selected-frame) 100))
    (progn (minimap-kill)
	   (setf minimap-exists nil)
	   (set-frame-width (selected-frame) 80))))
(global-set-key [f9] 'minimap-toggle-retain-size)

;; For downloading sulimity
(defun download-get(url)
  (let ((download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      ;; we may have to trim the http response
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file (concat "~/.emacs.d/plug-ins/"
			      (car (last (split-string url "/" t)))))
      (kill-buffer download-buffer)))
  (byte-recompile-directory (expand-file-name "~/.emacs.d/plug-ins") 0))

(add-to-list 'load-path "~/.emacs.d/plug-ins")

;; Ezbl configs
;; (require 'ezbl)

;; Powerline
(unless (require 'powerline nil t)
 (download-get "https://raw.githubusercontent.com/emmel/powerline/master/powerline.el")
 (download-get "https://raw.githubusercontent.com/emmel/powerline/master/powerline-separators.el")
 (download-get "https://raw.githubusercontent.com/emmel/powerline/master/powerline-themes.el")
 (require 'powerline))
;; (require 'powerline)

;;(powerline-default-theme)
(powerline-evil-theme)
;;(powerline-center-evil-theme)
;;(setq powerline-arrow-shape 'curve)   ;; the default
;; (custom-set-faces
;;   '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;;   '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
;; Change Powerline color
;; (set-face-attribute inverse-video nil)
;; (setq powerline-color1 "#073642")
;; (setq powerline-color2 "#002b36")

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#fdf6e3"
;;                     :background "#2aa198"
;;                     :box nil)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil)


;; download and include term-toggle
(unless (require 'term-toggle nil t)
  (download-get "http://www.emacswiki.org/emacs/download/term-toggle.el")
  (require 'term-toggle))
;; bind key
(global-set-key [f4] 'term-toggle)


;; highlight indentation
;;(require 'highlight-indentation)
(add-hook 'prog-mode-hook 'highlight-indentation-mode )

;; enable web-mode for html and template engines
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xcss\\'" . web-mode))

;; git gutter config to start git gutter for global
(global-git-gutter-mode +1)

;; auto-load on your sgml modes:

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;; download and include hideshowvis.el
(unless ( file-exists-p "~/.emacs.d/plug-ins/hideshowvis.el" )
  (download-get "http://www.emacswiki.org/emacs/download/hideshowvis.el"))

;; hideshowviz settings
;; enable hide show viz for code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)


(dolist (hook (list 'emacs-lisp-mode-hook
		    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;; If enabling hideshowvis-minor-mode is slow on your machine use M-x,
;; customize-option, hideshowvis-ignore-same-line and set it to nil. This will
;; then display - icons for foldable regions of one line, too but is faster
;;
;; To enable displaying a + symbol in the fringe for folded regions,
;; use:
;;
;; (hideshowvis-symbols)

;; in your ~/.emacs
;;
;; It is not enabled by default because it might interfere with custom
;; hs-set-up-overlay functions
;;


;; load python special configurations
;;======================================================================

;; load ruby special configurations
;;======================================================================

;; load java special configurations
;;======================================================================

;; global configurations
;;======================================================================
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "C-S-z"))
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(setq make-backup-files nil)

(delete-selection-mode 1)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-visual-line-mode t)

(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1))
)
(global-set-key [f5] 'toggle-bars-view)

;; switching window buffers
(global-set-key [s-left] 'windmove-left) 
(global-set-key [s-right] 'windmove-right) 
(global-set-key [s-up] 'windmove-up) 
(global-set-key [s-down] 'windmove-down)

;; default indentation to 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-set-key [f6] 'toggle-truncate-lines)
(global-set-key [C-f11] 'speedbar)
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") '(lambda () (interactive)
(text-scale-adjust
(- text-scale-mode-amount))
(text-scale-mode -1)))

;;======================================================================
;; other user configs
(setq user-full-name "krazedkrish"
      user-full-address "krazedkrish@gmail.com")

(set-language-environment "UTF-8")

;; hightlight entire bracket expression
(setq show-paren-style 'expression)
(show-paren-mode 1)
