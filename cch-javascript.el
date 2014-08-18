;; http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/

(defvar my-javascript-packages
  '(js2-mode
    ac-js2)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-javascript-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<fn\\>" (clojure-mode)))

(setq js2-highlight-level 3)

(add-hook 'js-mode-hook
          (lambda ()
            ;; Trim trailing whitespace on write buffer.
            ;; Note that trailing newlines at the end of the file are NOT trimmed.
	    (paredit-mode t)
	    (js2-minor-mode)
	    (define-key js-mode-map "{" 'paredit-open-curly)
	    (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
	    (pretty-symbols-mode t)))
(add-hook 'js2-mode-hook 'ac-js2-mode)

(provide 'cch-javascript)
