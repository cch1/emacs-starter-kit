(defvar my-clojure-packages
  '(paredit
    highlight-parentheses
    pretty-symbols
    clojure-mode
    auto-complete
    ac-nrepl
    cider
    midje-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<fn\\>" (clojure-mode)))

;; https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; Trim trailing whitespace on write buffer.
            ;; Note that trailing newlines at the end of the file are NOT trimmed.
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
            (highlight-parentheses-mode t)
	    (pretty-symbols-mode t)
            (paredit-mode t)))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)

;; CIDER mode settings
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-display-in-current-window t)

(provide 'cch-clojure)
