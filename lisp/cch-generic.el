(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    highlight-parentheses
    rotate
    smex
    saveplace
    pretty-symbols
    company
    magit)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Stifle spew on magit usage
(setq magit-last-seen-setup-instructions "1.4.0")

;; Inspiration: https://github.com/technomancy/emacs-starter-kit/blob/v2/starter-kit-misc.el
(ido-mode t) ;; ido is built-in since at least 24.3
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; smex goodness for executing commands
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is 'regular' M-x.

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(require 'mouse)

(xterm-mouse-mode t)

(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook
	  (lambda ()
	    (when window-system
		(set-frame-size (selected-frame) 150 50)) ;; make initial frame a reasonable size
	    (global-company-mode)))

(add-hook 'after-make-frame-functions
	  (lambda ()
	    (when window-system
		(set-frame-size (selected-frame) 150 50)))) ;; make subsequent frames a reasonable size

;; open window in front of terminal
(x-focus-frame nil)

;; Allow moving amongst windows with natural arrow keys + Shift
(windmove-default-keybindings)

;; Bind window rotation keys
(global-set-key (kbd "s-<up>") 'rotate-window)
(global-set-key (kbd "s-<down>") 'rotate-layout)
;; Bind fullscreen to be OSX-like
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
;; Bind mouse motion
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)


;; Enable whitespace mode
(global-whitespace-mode t)

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?α greek "\\<alpha\\>" (emacs-lisp-mode clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?β greek "\\<beta\\>" (emacs-lisp-mode clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?δ greek "\\<delta\\>" (emacs-lisp-mode clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?∂ greek "\\<partial\\>" (emacs-lisp-mode clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?∘ greek "\\<comp\\>" (clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?⊕ logical "\\<xor\\>" (emacs-lisp-mode clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?∘ greek "\\<comp\\>" (clojure-mode)))
(setq pretty-symbol-categories (list 'lambda 'greek 'relational 'logical))

(require 'saveplace)
(setq-default save-place t)

(setq js-indent-level 2)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      visible-bell t
;;      debug-on-error t
      apropos-do-all t
      sentence-end-double-space t
      whitespace-style (quote (face trailing empty)) ;; Only show "lonely" whitespace
      scroll-step 1
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(line-number-mode 1)
(column-number-mode 1)

(provide 'cch-generic)

;; (server-start)
