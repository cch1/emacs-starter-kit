(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "/Users/cch1/.emacs.d/custom.el")
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(safe-local-variable-values
   '((eval setq clojure-align-binding-forms
	   (append clojure-align-binding-forms
		   '("mlet" "rlet")))
     (eval setq clojure-align-forms-automatically nil))))
(setq-default fill-column 144)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "Black" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
 '(idle-highlight ((t (:box (:line-width -1 :color "grey75"))))))
