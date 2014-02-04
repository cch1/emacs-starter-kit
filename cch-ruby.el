(defvar my-ruby-packages
  '(paredit
    highlight-parentheses
    ruby-mode)
  "A list of ruby packages to ensure are installed at launch.")

(dolist (p my-ruby-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))))

(provide 'cch-ruby)
