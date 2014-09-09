;; http://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/

(defvar my-javascript-packages
  '(js2-mode
    flycheck
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

(setq js2-highlight-level 3)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-include-node-externs t)
(setq-default js2-global-externs '("describe" "it" "assert"))

;; http://truongtx.me/2014/02/22/emacs-using-paredit-with-non-lisp-mode/
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(require 'pretty-symbols)
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<fn\\>" (clojure-mode)))

(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))
	    (skewer-mode)
	    (ac-js2-mode)
	    (flycheck-mode t)
	    (my-paredit-nonlisp)
	    (define-key js-mode-map "{" 'paredit-open-curly)
	    (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
	    (company-mode 0) ;; I hope this works
	    (pretty-symbols-mode 0) ;; Doesn't seem to work with some of the other modes...
	    ))

(add-hook 'nodejs-repl-mode-hook
	  (lambda ()
	    (my-paredit-nonlisp)))

(provide 'cch-javascript)
