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
;;   escreen            			; screen for emacs, C-\ C-h
;;   php-mode-improved			; if you're into php...
   switch-window			; takes over C-x o
   auto-complete			; complete as you type with overlays
   yasnippet 				; powerful snippet mode
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   ;;color-theme		                ; nice looking emacs
   color-theme-solarized	        ; check out color-theme-solarized
   evil					; vi mode
;;   helm					; powerful completion and selection narrowing framework
   minimap				; sublimetext-style minimap sidebar
   neotree				; emacs tree plugin like NERD tree
   highlight-symbol			; highlight the same symbols in code, navigate in them, or replace string 
   tabbar-ruler				;  
   ezbl					;
   nxhtml				; editing .rhtml or .html.erb
   emacs-rails-reloaded			; rails plugin for emacs
   ;; powerline				; emacs-powerline
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
(when (load-theme 'solarized-dark t))

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
(global-set-key [f8] 'neotree-toggle)

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
(defun download-git (url)
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
      (kill-buffer download-buffer))))

(add-to-list 'load-path "~/.emacs.d/plug-ins")

;; (unless (require 'sublimity nil t)
;;   (download-git "https://raw.githubusercontent.com/zk-phi/sublimity/master/sublimity.el"))
;; (unless (require 'sublimity-scroll nil t)
;;   (download-git "https://raw.githubusercontent.com/zk-phi/sublimity/master/sublimity-scroll.el"))
;; (unless (require 'sublimity-map nil t)
;;   (download-git "https://raw.githubusercontent.com/zk-phi/sublimity/master/sublimity-map.el"))
;; ;; (unless (require 'sublimity-attractive nil t)
;; ;;   (download-git "https://raw.githubusercontent.com/zk-phi/sublimity/master/sublimity-attractive.el"))

;; ;; Actual sublimity configurations
;; ;;(sublimity-mode 1)
;; (defun toggle-sublimity()
;;   (interactive)
;;   (if sublimity-mode (sublimity-mode 0)(sublimity-mode 1))
;; )
;; (global-set-key [f9] 'toggle-sublimity)
;; ;;(setq sublimity-scroll-weight 5
;; ;;      sublimity-scroll-drift-length 10)

;; (setq sublimity-map-size 20)
;; (setq sublimity-map-fraction 0.3)
;; (setq sublimity-map-text-scale -7)

;; (add-hook 'sublimity-map-setup-hook
;;           (lambda ()
;;             (setq buffer-face-mode-face '(:family "Monospace"))
;;             (buffer-face-mode)))

;;(sublimity-map-set-delay 3)

;;(setq sublimity-attractive-centering-width 110)

;;(sublimity-attractive-hide-bars)
;;(sublimity-attractive-hide-vertical-border)
;;(sublimity-attractive-hide-fringes)
;;(sublimity-attractive-hide-modelines)

;; Ezbl configs
(require 'ezbl)

;; Powerline
(unless (require 'powerline nil t)
 (download-git "https://raw.githubusercontent.com/emmel/powerline/master/powerline.el")
 (download-git "https://raw.githubusercontent.com/emmel/powerline/master/powerline-separators.el")
 (download-git "https://raw.githubusercontent.com/emmel/powerline/master/powerline-themes.el")
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
