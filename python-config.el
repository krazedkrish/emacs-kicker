;; enable white space for python
(add-hook 'python-mode-hook 'whitespace-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; jedi configs for autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(coffee-tab-width 2)
'(web-mode-code-indent-offset 2)
'(web-mode-css-indent-offset 2)
'(web-mode-markup-indent-offset 2)
'(web-mode-style-padding 2)
'(web-mode-script-padding 2)
)

