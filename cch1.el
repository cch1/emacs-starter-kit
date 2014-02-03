(require 'mouse)
(xterm-mouse-mode t)

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Enable whitespace mode
(global-whitespace-mode t)

;; Only show "lonely" whitespace
(setq whitespace-style (quote (face trailing empty)))
(setq js-indent-level 2)
(setq scroll-step 1)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks '(lambda () (save-excursion (delete-trailing-whitespace))))))
(add-hook 'yaml-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks '(lambda () (save-excursion (delete-trailing-whitespace))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; Trim trailing whitespace on write buffer.
            ;; Note that trailing newlines at the end of the file are NOT trimmed.
            (add-hook 'local-write-file-hooks '(lambda () (save-excursion (delete-trailing-whitespace))))
            (highlight-parentheses-mode t)
            (paredit-mode t)))

(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; CIDER mode settings
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-display-in-current-window t)



