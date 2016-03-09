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



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
 '((:name smex				; a better (ido like) M-x
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
   color-theme		                ; nice looking emacs
   color-theme-solarized	        ; check out color-theme-solarize
   color-theme-zenburn		        ; check out color-theme-zenburn
   ;;helm					; powerful completion and selection narrowing framework
   minimap				; sublimetext-style minimap sidebar
   neotree				; emacs tree plugin like NERD tree
   highlight-symbol			; highlight the same symbols in code, navigate in them, or replace string 
   tabbar-ruler				;  
   highlight-indentation		; hightlight the indentations
   highlight-parentheses		; hightlight the parantheses
   web-mode				; smart html library supporting template engines
   ;; git-gutter                           ;highlight git changes
   ;; projectile ;project interaction library
   smooth-scroll  ; smooth scroll   
   ;;python special packages
   jedi					; autocomplete for python
   markdown-mode ; Markdown mode
   which-key
   powerline
   ;;outshine

   ;; ruby special package
   emacs-rails-reloaded			; rails plugin for emacs
   flymake-ruby
   rvm
   inf-ruby
   projectile
   projectile-rails
   robe-mode
   flymake-easy				; required for flymake-css
   flymake-css				; for css validation
   flymake-sass
   flymake-coffee
   flymake-haml
   flymake-html-validator
   haml-mode
   yaml-mode
   slim-mode
   sass-mode
   coffee-mode

   ;; java + android special packages
   jdee                         ; autocomplete for java
   ;;jde-flymake                  ; flymake for java using jdee & ;;jikes
   ;;java-complete                ; auto complete for java
   android-mode                 ; android-mode for android
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

;; For downloading extra things
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

;; load material theme according to time
;; org: https://github.com/cpaulik/emacs-material-theme
;; download and include material theme.el
(unless ( file-exists-p "~/.emacs.d/plug-ins/material-theme.el" )
  (download-get "https://github.com/cpaulik/emacs-material-theme/raw/master/material-light-theme.el")
  (download-get "https://github.com/cpaulik/emacs-material-theme/raw/master/material-theme.el"))
(setq custom-theme-directory "~/.emacs.d/plug-ins/")
;;(load-theme 'material-light t)
(setq hour 
        (string-to-number 
            (substring (current-time-string) 11 13))) ;;closes (setq hour...
    (if (member hour (number-sequence 6 17))
        (setq theme-to-load 'material-light)
        (setq theme-to-load 'material))
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

;; replace default ido with flx-ido
(unless (require 'flx-ido nil t)
  (download-get "https://raw.githubusercontent.com/lewang/flx/master/flx.el")
  (download-get "https://raw.githubusercontent.com/lewang/flx/master/flx-ido.el")
  (require 'flx-ido))
(flx-ido-mode 1)

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
;; (evil-mode 1)
;;(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;;(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;;(ergoemacs-mode 1)

;; minimap
;;(setq minimap-window-location 'left)

;; neotree
(global-set-key [f12] 'neotree-toggle)
(setq neo-window-width 35)

;; highlight entire bracket expression
(setq show-paren-style 'expression)
(show-paren-mode 1)

;; highlight-sysmbol configurations
    (global-set-key [(control f3)] 'highlight-symbol-at-point)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

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

;; Ezbl configs
;; (require 'ezbl)

;; download and include term-toggle
(unless (require 'term-toggle nil t)
  (download-get "http://www.emacswiki.org/emacs/download/term-toggle.el")
  (require 'term-toggle))
;; bind key
(global-set-key [f4] 'term-toggle)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

;; watch-words
(defun watch-words ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX ?-?\\(ME\\)?\\|TODO\\|BUGS?\\|TIPS?\\|TESTING\\|WARN\\(ING\\)?S?\\|WISH\\|IMP\\|NOTE\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'watch-words)
(add-hook 'org-mode 'watch-words)


;;(add-hook 'prog-mode-hook
;;          (lambda () (yafolding-mode)))


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

(add-hook 'prog-mode-hook 'hideshowvis-enable)
(add-hook 'markdown-mode-hook 'hideshowvis-enable)
(add-hook 'web-mode-hook 'hideshowvis-enable)
;; (add-hook 'python-mode-hook 'hideshowvis-en
;; (dolist (hook (list 'emacs-lisp-mode-hook
;; 		    'c++-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))

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
(load "~/.emacs.d/python-config.el")

;; load ruby special configurations
;;======================================================================
(load "~/.emacs.d/ruby-config.el")

;; load java special configurations
;;======================================================================
(load "~/.emacs.d/java-config.el")


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

;; comment action change
(global-unset-key (kbd "C-S-/"))             ;
(global-set-key (kbd "C-S-/") 'comment-line) ;


;; full buffer selct
;; (global-unset-key (kbd "C-s"))
(global-set-key(kbd "C-a") 'mark-whole-buffer)


;; goto line
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'goto-last-change)

(setq frame-title-format "%b")

(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1))
)
(global-set-key [f5] 'toggle-bars-view)

;; switching window buffers
(global-set-key [M-left] 'windmove-left) 
(global-set-key [M-right] 'windmove-right) 
(global-set-key [M-up] 'windmove-up) 
(global-set-key [M-down] 'windmove-down)

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

;; key binding
(global-set-key [f7] 'tabbar-mode)
(global-set-key [(control shift prior)] 'tabbar-backward-group)
(global-set-key [(control shift next)] 'tabbar-forward-group)
(define-key global-map [(control tab)] 'tabbar-forward)
(define-key global-map (kbd "C-<next>") 'tabbar-forward)
(define-key global-map (kbd "C-S-<iso-lefttab>") 'tabbar-backward)
(define-key global-map (kbd "C-<prior>") 'tabbar-backward)

;; web mode settings

(setq web-mode-enable-current-column-highlight t)

;;======================================================================

;; tabbar-rule + mode-icons + powerline

;; for mode-icons-mode
;; DOWNLOAD AND PLACE THE ICONS FOLDER IN "~/.emacs.d/plug-ins/"
(unless ( file-exists-p "~/.emacs.d/plug-ins/mode-icons.el" )
  (download-get "https://raw.githubusercontent.com/ryuslash/mode-icons/master/mode-icons.el"))
(require 'mode-icons)
;;(load-file "~/.emacs.d/plug-ins/mode-icons.el")
(mode-icons-mode)

;; (setq tabbar-ruler-fancy-current-tab-separator 'wave)
;; (setq tabbar-ruler-fancy-tab-separator 'zigzag)
(setq tabbar-ruler-fancy-current-tab-separator 'zigzag)
(setq tabbar-ruler-fancy-tab-separator 'chamfer)
(load "~/.emacs.d/tabbar.el")
;; (load "~/.emacs.d/config/modeline.el")
(require 'powerline)
(load "~/.emacs.d/powerline-iconic-theme.el")
(powerline-iconic-theme)
(load-library "url-handlers")

;;======================================================================
;; other user configs
(setq user-full-name "krazedkrish"
      user-full-address "krazedkrish@gmail.com")

(set-language-environment "UTF-8")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(delete-selection-mode t)
 '(org-CUA-compatible nil)
 '(org-replace-disputed-keys nil)
 '(recentf-mode t)
 '(shift-select-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
