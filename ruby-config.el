;; ruby special configurations

;; indentations
;; web indentations
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
;  (web-mode-attr-indent-offset 2)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; reference http://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
;; hook flymake-ruby with ruby-mode
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; sane indentation
(setq ruby-deep-indent-paren nil)

;; inf-ruby
;; run ruby inside shell
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; rvm
;; RVM for managing ruby versions
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; flx-ido-mode activates the ido mode augmenting with flexible matching.

;; You may want to use following snippet to display ido completions vertically instead of horizontally, as is the default behaviour.

;; ;; Display ido results vertically, rather than horizontally
;;   (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;   (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;;   (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;   (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;     (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;     (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;;   (add-hook 'ido-setup-hook 'ido-define-keys)

;; projectile
;; (add-hook 'ruby-mode-hook projectile-on)

;; projectile-ruby
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; robe-mode
;; (require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; using rvm so instructing robe to auto-trigger rvm-activate-corresponding-ruby.
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; for flymake-css
(require 'flymake-css)
(add-hook 'css-mode-hook 'flymake-css-load)

;; for flymake-sass
;; (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-hook 'sass-mode-hook 'flymake-sass-load)

;; for flymake-coffee
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;; for flymake-haml
(add-hook 'haml-mode-hook 'flymake-haml-load)
