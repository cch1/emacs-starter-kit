;; http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/

(defvar my-javascript-packages
  '(js2-mode
    ac-js2
    json-mode
    jade-mode
    skewer-mode
    nodejs-repl)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-javascript-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<fn\\>" (clojure-mode)))

(setq js2-highlight-level 3)

(add-hook 'js2-mode-hook
	  (lambda ()
	    (skewer-mode)
	    (ac-js2-mode)
	    (define-key js-mode-map "{" 'paredit-open-curly)
	    (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
	    (pretty-symbols-mode t)))

(add-hook 'nodejs-repl-mode-hook
	  (lambda ()
	    (paredit-mode t)))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(provide 'cch-javascript)
