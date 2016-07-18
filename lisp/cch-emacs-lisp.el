(defvar my-emacs-lisp-packages
  '(paredit
    highlight-parentheses)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-emacs-lisp-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'pretty-symbols)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (paredit-mode t)
				  (prettify-symbols-mode 1)))

(provide 'cch-emacs-lisp)
