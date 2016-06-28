(defvar my-clojure-packages
  '(paredit
    highlight-parentheses
    pretty-symbols
    clojure-mode
    idle-highlight-mode
    cider
;    midje-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-clojure-packages)
  (unless (package-installed-p p)
    (package-install p)))

(add-to-list 'load-path (locate-user-emacs-file "custom-packages/midje-mode"))
(require 'midje-mode)
;(require 'clojure-jump-to-file)
(setq midje-praise-quotes nil)

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<fn\\>" (clojure-mode)))

(defun lisp-editing-behavior ()
  (highlight-parentheses-mode t)
  (idle-highlight-mode t)
  (pretty-symbols-mode t)
  (paredit-mode t)
  (local-set-key (kbd "{") 'paredit-open-curly))

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; Trim trailing whitespace on write buffer.
            ;; Note that trailing newlines at the end of the file are NOT trimmed.
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
	    (lisp-editing-behavior)))

(add-hook 'cider-repl-mode-hook 'lisp-editing-behavior)

;; CIDER mode settings
(setq nrepl-hide-special-buffers t) ;; When using switch-to-buffer, pressing SPC after the command will make the hidden buffers visible.
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
;; (setq cider-repl-display-in-current-window t)
(setq cider-auto-select-error-buffer t)
(setq cider-stacktrace-default-filters '(tooling dup))
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-print-length 100)
(setq cider-repl-history-file "~/.emacs.d/cache/cider-history")
(setq cider-prompt-save-file-on-load nil) ; C-c C-k
(setq cider-use-overlays nil) ; Show eval results in minibuffer instead of inline


(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

(provide 'cch-clojure)
