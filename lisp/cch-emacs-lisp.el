(defvar my-emacs-lisp-packages
  '(paredit
    highlight-parentheses
    pretty-symbols)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-emacs-lisp-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'pretty-symbols)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (paredit-mode t)
				  (pretty-symbols-mode t)))

(provide 'cch-emacs-lisp)
