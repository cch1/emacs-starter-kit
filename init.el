(when (eq system-type 'darwin)
  ;; (require 'ls-lisp)
  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; Either the above two commands or the following three commands should
  ;; be used on darwin due to incompatibilities with the built-in ls command.
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-use-ls-dired nil))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    highlight-parentheses
    yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (add-to-list 'load-path "~/.emacs.d/vendor/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(slime-net-coding-system (quote utf-8-unix)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Monaco"))))
 '(hl-line ((t (:inherit nil :weight extra-bold))) t))


;; Personal Hand-crafted customizations 
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(require 'cch-generic)
(require 'cch-clojure)
