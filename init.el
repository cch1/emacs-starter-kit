(when (eq system-type 'darwin)
  ;; (require 'ls-lisp)
  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; Either the above two commands or the following three commands should
  ;; be used on darwin due to incompatibilities with the built-in ls command.
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil)
  (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-14")))
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

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
