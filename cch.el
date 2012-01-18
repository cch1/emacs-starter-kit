;; Enable whitespace mode
(global-whitespace-mode t)

;; Only show "lonely" whitespace
(setq whitespace-style (quote (face trailing empty)))

;; Trim trailing whitespace on write buffer.  Note that trailing
;; newlines at the end of the file are NOT trimmed.
(add-hook 'clojure-mode-hook
          (lambda () (add-hook 'local-write-file-hooks '(lambda () (save-excursion (delete-trailing-whitespace))))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (ruby-electric-mode t)
            (add-hook 'local-write-file-hooks '(lambda () (save-excursion (delete-trailing-whitespace))))))


(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
