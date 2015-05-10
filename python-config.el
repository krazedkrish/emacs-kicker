;; enable white space for python
(add-hook 'python-mode-hook 'whitespace-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; jedi configs for autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
