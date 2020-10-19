(when (eq system-type 'darwin)
  ;; (require 'ls-lisp)
  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; Either the above two commands or the following three commands should
  ;; be used on darwin due to incompatibilities with the built-in ls command.
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil))

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Personal Hand-crafted customizations
;; Inspiration:
;; http://emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/
;; http://www.aaronbedra.com/emacs.d/
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'cch-generic)
(require 'cch-emacs-lisp)
(require 'cch-clojure)
(require 'cch-clojurescript)
(require 'cch-javascript)
(require 'cch-yaml)
(require 'cch-xml)
(require 'cch-ruby)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3f3f3f" "#ea3838" "#7fb07f" "#fe8b04" "#62b6ea" "#e353b9" "#1fb3b3" "#d5d2be"])
 '(custom-enabled-themes '(tango-dark))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(fci-rule-color "#222222")
 '(linum-format "%3i")
 '(package-selected-packages
   '(gh-md grip-mode yaml-mode sws-mode smex rotate pretty-symbols prettify-greek paredit pandoc-mode org nodejs-repl mustache-mode mbo70s-theme markdown-mode magit logview json-mode jade-mode intero inf-clojure idle-highlight-mode highlight-parentheses grunt go-mode exec-path-from-shell emidje csv-mode color-theme-solarized color-theme-sanityinc-solarized color-theme-emacs-revert-theme color-theme-dg color-theme-complexity color-theme-actress bubbleberry-theme badger-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ac-js2))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
(put 'upcase-region 'disabled nil)
