(ido-mode t)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(require 'mouse)
(xterm-mouse-mode t)

;; (add-hook 'window-setup-hook 'maximize-frame t)

;; open window in front of terminal
(x-focus-frame nil)

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(windmove-default-keybindings)

;; Enable whitespace mode
(global-whitespace-mode t)

;; Only show "lonely" whitespace
(setq whitespace-style (quote (face trailing empty)))

(setq js-indent-level 2)
(setq scroll-step 1)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))))
(add-hook 'yaml-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

(provide 'cch-generic)
