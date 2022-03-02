;; Reference: https://ianyepan.github.io/posts/setting-up-use-package/
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(custom-set-variables
 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))
(package-initialize)

(setq package-enable-at-startup nil) ; because we just initialized manually

(unless package-archive-contents
    (package-refresh-contents))

;; Reference: https://medium.com/helpshift-engineering/configuring-emacs-from-scratch-use-package-c30382297877
;; Reference: https://ianyepan.github.io/posts/setting-up-use-package/
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(global-auto-revert-mode 1)

'(load-theme 'tango-dark t) ; built into Mac app for Emacs 27.2 and probably much earlier

;; https://github.com/kuanyui/moe-theme.el/tree/b23975ba57a68f69551424552f484227db8a7b97
;; Hideous highlight ruins this customization of the moe theme...
(use-package moe-theme
  :ensure t
  :load-path "themes"
  :init
  (setq moe-theme-kit t
	moe-theme-highlight-buffer-id t
	moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0)
	moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0)
	moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
  :config
  (require 'moe-theme-flavours)
  ;; (moe-theme-flavour-tomorrow)
  ;; (moe-theme-select-color 'red)
  (let ((dark-background "#1d1f21")
	(dark-current-line "#282a2e")
	(dark-selection "#373b41")
	(dark-foreground "#c5c8c6")
	(dark-comment "#969896")
	(dark-red "#cc6666")
	(dark-orange "#de935f")
	(dark-yellow "#f0c674")
	(dark-green "#b5bd68")
	(dark-aqua "#8abeb7")
	(dark-blue "#81a2be")
	(dark-purple "#b294bb")
	(light-background "#ffffff")
	(light-current-line "#efefef")
	(light-selection "#d6d6d6")
	(light-foreground "#4d4d4c")
	(light-comment "#8e908c")
	(light-red "#c82829")
	(light-orange "#f5871f")
	(light-yellow "#eab700")
	(light-green "#718c00")
	(light-aqua "#3e999f")
	(light-blue "#4271ae")
	(light-purple "#8959a8"))
    (setq moe-dark-doc dark-blue
	  moe-dark-doc-string "#c82829"))
  (load-theme 'moe-dark t)
  :custom-face
  (default ((t (:inherit nil :extend nil :stipple nil :background "Black" :foreground "#c6c6c6"
			 :inverse-video nil :box nil :strike-through nil :overline nil :underline nil
			 :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
  (idle-highlight ((t (:box (:line-width -1 :color "grey75"))))))

;; https://emacsthemes.com/themes/solarized-themes.html
'(use-package solarized-theme
   :ensure t
   :load-path "themes"
   :init (setq solarized-use-more-italic t
	       solarized-high-contrast-mode-line t)
   :config (load-theme 'solarized-dark t))

;; https://emacsthemes.com/themes/tangotango-theme.html
'(use-package tangotango-theme
   :ensure t
   :load-path "themes"
   :config (load-theme 'tangotango t))

(use-package fic-mode
  :ensure t
  :diminish fic-mode
  :hook (prog-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package editorconfig
  :ensure t
  :diminish t
  :config (editorconfig-mode 1))

(use-package markdown-mode
  :ensure t
  ;; :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.md\\'" . gfm-mode))

;; https://tbellisiv.gitbooks.io/clojure-emacs-cider-intro/content/Spiking_Cider/Eldoc.html
(use-package eldoc
  :ensure t
  :hook ((prog-mode cider-mode cider-repl-mode) . eldoc-mode))

;; https://tbellisiv.gitbooks.io/clojure-emacs-cider-intro/content/Spiking_Cider/Paredit.html
(use-package paredit
  :ensure t
  :hook (((emacs-lisp-mode) . paredit-mode)
	 ((lisp-mode lisp-interaction-mode) . paredit-mode)
	 ((cider-mode cider-repl-mode) . paredit-mode)
	 ((clojure-mode clojurescript-mode clojurec-mode) . paredit-mode)
	 ((eval-expression-minibuffer-setup-hook) . paredit-mode)
	 (ruby-mode))
  :config (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
	 ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package whitespace
  :ensure t
  :hook (prog-mode . whitespace-mode))

(defvar my-packages
  '(logview
    rotate
    org
    pandoc-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; https://tbellisiv.gitbooks.io/clojure-emacs-cider-intro/content/Spiking_Cider/Auto_Completion.html
(use-package company
  :ensure t
  :hook ((prog-mode cider-mode cider-repl-mode) . company-mode))

;; https://tbellisiv.gitbooks.io/clojure-emacs-cider-intro/content/Spiking_Cider/Indentation.html
(use-package aggressive-indent
  :ensure t
  :hook ((prog-mode cider-mode cider-repl-mode emacs-lisp) . aggressive-indent-mode))

(use-package highlight-parentheses
  :ensure t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
	 ((clojure-mode clojurescript-mode clojurec-mode) . highlight-parentheses-mode)))

  ;; https://gitlab.com/ideasman42/emacs-idle-highlight-mode/
(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :hook (prog-mode)
  :config (setq idle-highlight-exclude-point t))

(use-package subword
  :diminish subword-mode
  :hook (((prog-mode cider-repl-mode) . subword-mode)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)) ; This is 'regular' M-x.
  :config
  (global-unset-key (kbd "C-z")) ; C-x before C-z to really suspend-frame
  (smex-initialize))

;; Inspiration: https://github.com/technomancy/emacs-starter-kit/blob/v2/starter-kit-misc.el
(use-package ido ; ido is built-in since at least 24.3
  :ensure t
  :init (setq ido-enable-prefix nil
	      ido-enable-flex-matching t
	      ido-auto-merge-work-directories-length nil
	      ido-create-new-buffer 'always
	      ido-use-filename-at-point 'guess
	      ido-use-virtual-buffers t
	      ido-handle-duplicate-virtual-buffers 2
	      ido-max-prospects 10)
  :config
  (ido-mode t))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(require 'mouse)

(xterm-mouse-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun set-frame-parameters ()
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html
  (if (display-graphic-p)
      (progn
	(add-to-list 'default-frame-alist (cons 'width 0.5))
	(add-to-list 'default-frame-alist (cons 'height 0.5))
	(add-to-list 'default-frame-alist (cons 'top 0.2))
	(add-to-list 'default-frame-alist (cons 'left 0.2))
	(add-to-list 'default-frame-alist (cons 'width 0.75))
	(add-to-list 'default-frame-alist (cons 'height 0.75))
	(add-to-list 'default-frame-alist (cons 'left-fringe 4))
	(add-to-list 'default-frame-alist (cons 'right-fringe 2))
	(add-to-list 'initial-frame-alist (cons 'top 0.1))
	(add-to-list 'initial-frame-alist (cons 'left 0.1)))))

(set-frame-parameters)

;; http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; Allow moving amongst windows with natural arrow keys + Super (Command/Flower on OSX)
(use-package windmove
  :demand
  :config
  (windmove-default-keybindings 'super))

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

(use-package prog-mode
  :hook (((prog-mode cider-mode) . prettify-symbols-mode)))

(use-package prettify-greek
  :ensure t)

(defvar cch/prettify-logical '(("and" . ?∧) ("or" . ?∨) ("not" . ?¬) ("xor" . ?⊕)
			       ("nor" . ?⊽) ("nand" . ?⊼)))
(defvar cch/prettify-relational '(("not=" . ?≠) (">=" . ?≥) ("<=" . ?≤)))
(defvar cch/prettify-set '(("union" . ?∪) ("intersection" . ?∩)))
(defvar cch/prettify-extra '(
			     ("delta" . ?∂) ;; what is this really?
			     ))

(use-package saveplace
  :ensure t
  :init
  (setq save-place-file (concat user-emacs-directory "places"))
  :config
  (setq-default save-place t))

(setq js-indent-level 2)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      debug-on-error nil
      apropos-do-all t
      sentence-end-double-space t
      whitespace-style (quote (face trailing empty)) ;; Only show "lonely" whitespace
      scroll-step 1
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(line-number-mode 1)
(column-number-mode 1)

;;; https://github.com/anschwa/emacs.d
(setq-default x-stretch-cursor t)

(use-package bug-reference
  :ensure t
  :hook ((text-mode . bug-reference-mode)
	 (prog-mode . bug-reference-prog-mode)))

;; (server-start)

;; PReviously it just was to work around a bug in OSX with visible bell not.
;; restoring screen properly.  Now I prefer it to an actual beep
(setq visible-bell nil
      ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Get full path of file buffers in window/frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Auto-save to system temp dir
;; https://www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'cch-generic)
