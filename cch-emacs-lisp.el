(defvar my-emacs-lisp-packages
  '(paredit
    highlight-parentheses
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-emacs-lisp-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

(provide 'cch-emacs-lisp)
