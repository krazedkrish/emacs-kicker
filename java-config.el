(setq jde-check-version-flag nil)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(unless (fboundp 'semantic-format-prototype-tag-java-mode)
  (defalias 'semantic-format-prototype-tag-java-mode 'semantic-format-tag-prototype-java-mode))
  (require 'hippie-exp)

(autoload 'jde-mode "jde" "JDE mode." t)
  (setq auto-mode-alist 
        (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))

(progn (setq android-ndk-root-path "/share/android-sdk-linux/android-ndk-r10d")
    (setq android-sdk-root-path "/share/android-sdk-linux"))
(setq android-default-package "com.zxy")
