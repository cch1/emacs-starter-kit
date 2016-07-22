(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-pinned-packages '((cider . "melpa-stable")))
(setq package-load-list '((cider t) all))

(package-initialize)

(setq package-enable-at-startup nil) ; because we just initialized manually

(unless package-archive-contents
    (package-refresh-contents))

(defvar my-packages
  '(paredit
    log4j-mode
    highlight-parentheses
    rotate
    smex
    saveplace
    prettify-greek
    company
    magit
    org)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
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
(when window-system (x-focus-frame nil))

(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; Allow moving amongst windows with natural arrow keys + Super (Command/Flower on OSX)
(windmove-default-keybindings 'super)

;; Bind window rotation keys
(global-set-key (kbd "S-s-<up>") 'rotate-window)
(global-set-key (kbd "S-s-<down>") 'rotate-layout)
;; Bind fullscreen to be OSX-like
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
;; Bind font scaling to conventional keys
(global-set-key (kbd "s-<kp-add>") 'text-scale-increase)
(global-set-key (kbd "s-<kp-subtract>") 'text-scale-decrease)
;; Bind mouse motion
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<kp-enter>") 'newline)

;; Enable whitespace mode
(global-whitespace-mode t)

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?∘ greek "\\<comp\\>" (clojure-mode)))
(add-to-list 'pretty-symbol-patterns '(?∘ greek "\\<comp\\>" (clojure-mode)))
(setq pretty-symbol-categories (list 'lambda 'greek 'relational 'logical))

(require 'prettify-greek)
(defvar cch/prettify-logical '(("and" . ?∧) ("or" . ?∨) ("not" . ?¬) ("xor" . ?⊕)
			       ("nor" . ?⊽) ("nand" . ?⊼)))
(defvar cch/prettify-relational '(("not=" . ?≠) (">=" . ?≥) ("<=" . ?≤)))
(defvar cch/prettify-set '(("union" . ?∪) ("intersection" . ?∩)))
(defvar cch/prettify-extra '(
			       ("delta" . ?∂) ;; what is this really?
			       ))

(require 'saveplace)
(setq-default save-place t)

(setq js-indent-level 2)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
;;      debug-on-error t
      apropos-do-all t
      sentence-end-double-space t
      whitespace-style (quote (face trailing empty)) ;; Only show "lonely" whitespace
      scroll-step 1
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(line-number-mode 1)
(column-number-mode 1)

;;; https://github.com/anschwa/emacs.d
(setq-default x-stretch-cursor t)

(provide 'cch-generic)

;; (server-start)

;; work around a bug in OSX with visible bell not restoring screen properly
(setq visible-bell nil
      ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil 'invert-face 'mode-line)))
