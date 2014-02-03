(x-focus-frame nil) ;; open window in front of terminal
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ;; starter-kit
    ;; starter-kit-ruby
    ;; starter-kit-lisp
    ;; starter-kit-eshell
    paredit
    highlight-parentheses
    clojure-mode
    midje-mode
    cider
    yaml-mode
    ;;    color-theme
    ;;    color-theme-wombat
    ;;    color-theme-wombat+
    ;;    color-theme-gruber-darker
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (add-to-list 'load-path "~/.emacs.d/vendor/")
;; (require 'cider)

;; (add-hook 'window-setup-hook 'maximize-frame t)

;; (put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(dired-use-ls-dired nil)
 '(inhibit-startup-screen t)
 '(slime-net-coding-system (quote utf-8-unix)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Monaco"))))
 '(hl-line ((t (:inherit nil :weight extra-bold)))))
