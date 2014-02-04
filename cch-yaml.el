(defvar my-yaml-packages
  '(yaml-mode)
  "A list of YAML packages to ensure are installed at launch.")

(dolist (p my-yaml-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'yaml-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))))

(provide 'cch-yaml)
