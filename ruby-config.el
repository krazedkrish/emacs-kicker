;; ruby special configurations
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
