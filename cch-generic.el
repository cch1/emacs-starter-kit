(defvar my-packages
  '(paredit
    highlight-parentheses
    saveplace)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(ido-mode t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(require 'mouse)

(xterm-mouse-mode t)

(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook
	  (lambda ()
	    (when window-system
		(set-frame-size (selected-frame) 150 50)))) ;; make initial frame a reasonable size

(add-hook 'after-make-frame-functions
	  (lambda ()
	    (when window-system
		(set-frame-size (selected-frame) 150 50)))) ;; make subsequent frames a reasonable size

;; open window in front of terminal
(x-focus-frame nil)

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Allow moving amongst windows with natural arrow keys + Shift
(windmove-default-keybindings)

;; Enable whitespace mode
(global-whitespace-mode t)

(require 'saveplace)
(setq-default save-place t)

(setq js-indent-level 2)

(setq inhibit-startup-screen t
      visible-bell t
      apropos-do-all t
      sentence-end-double-space t
      whitespace-style (quote (face trailing empty)) ;; Only show "lonely" whitespace
      scroll-step 1
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(provide 'cch-generic)
