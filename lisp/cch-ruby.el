(use-package ruby-mode
  :ensure t
  :mode
  "Rakefile"
  "\\.rb\\'")

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks (lambda () (save-excursion (delete-trailing-whitespace))))))

(provide 'cch-ruby)
