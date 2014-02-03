(defvar my-clojure-packages
  '(paredit
    highlight-parentheses
    clojure-mode
    cider
    midje-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; Trim trailing whitespace on write buffer.
            ;; Note that trailing newlines at the end of the file are NOT trimmed.
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
            (highlight-parentheses-mode t)
            (paredit-mode t)))

(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; CIDER mode settings
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-display-in-current-window t)

(provide 'cch-clojure)
