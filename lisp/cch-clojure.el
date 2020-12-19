(defvar my-clojure-packages
  '(paredit
    aggressive-indent
    highlight-parentheses
    clojure-mode
    idle-highlight-mode
    cider
    eldoc
    company
    logview)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-clojure-packages)
  (unless (package-installed-p p)
    (package-install p)))

(defun lisp-editing-behavior ()
  (highlight-parentheses-mode t)
  (idle-highlight-mode t)
  (setq scroll-conservatively 101)
  (setq prettify-symbols-alist (append cch/prettify-logical cch/prettify-relational
  				       cch/prettify-set
  				       prettify-greek-lower prettify-greek-upper
  				       '(("#{}" . ?∅)
  					 ("fn" . ?ƒ) ("=>" . ?⟹)
  					 ("partial" . ?∂) ("comp" . ?∘) ("complement" . ?∁))))
  (prettify-symbols-mode t)
  (subword-mode t)
  (paredit-mode t)
  (aggressive-indent-mode t)
  (eldoc-mode t)
  (local-set-key (kbd "{") 'paredit-open-curly))

(eval-after-load 'clojure-mode
  ;; We need this level of control for some essential keywords that must be identified by regexp
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("		; anonymous function
		    (0 (progn (compose-region (match-beginning 1)
					      (match-end 1) "λ")
			      nil)))
		   ("\\(#\\){[^}]"	; sets, but not the empty set
		    (0 (progn (compose-region (match-beginning 1)
					      (match-end 1) "∈")
			      nil)))
		   ("\\(->\\)[^>]"	; thread, but not thread-last
		    (0 (progn (compose-region (match-beginning 1)
					      (match-end 1) "→")
			      nil)))
		   ("\\(->>\\)"		; thread-last
		    (0 (progn (compose-region (match-beginning 1)
					      (match-end 1) '(?→ (Br . Bc) ?→))
			      nil))))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'before-save-hook (lambda () (save-excursion
						;; re-indent without prettify
						(prettify-symbols-mode -1)
						(indent-region (point-min) (point-max))
						;; Trim trailing whitespace on write buffer.
						;; Note that trailing newlines at the end of the file are NOT trimmed.
						(delete-trailing-whitespace)))
		      nil 'local)
	    (add-hook 'after-save-hook (lambda () (save-excursion
					       ;; re-indent with prettify
					       (prettify-symbols-mode 't)
					       (indent-region (point-min) (point-max))
					       (set-buffer-modified-p nil)))
		      nil 'local)
	    ;; https://github.com/clojure-emacs/clojure-mode/#indentation-options
	    ;; http://cider.readthedocs.io/en/latest/indent_spec/
	    (lisp-editing-behavior)))

(add-hook 'cider-repl-mode-hook 'lisp-editing-behavior)
(add-hook 'cider-clojure-interaction-mode 'lisp-editing-behavior)

;; CIDER mode settings
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
;; (setq cider-repl-display-in-current-window t)
(setq cider-auto-select-error-buffer t)
(setq cider-stacktrace-default-filters '(tooling dup))
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-print-length 100)
(setq cider-repl-history-file "~/.emacs.d/cache/cider-history")
(setq cider-prompt-save-file-on-load nil) ; C-c C-k
(setq cider-use-overlays nil) ; Show eval results in minibuffer instead of inline
(setq cider-repl-display-help-banner nil) ; suppress start-up help banner
(setq cider-redirect-server-output-to-repl nil) ; per Bozhidar https://github.com/clojure-emacs/cider/pull/1907#issuecomment-475938304
(setq cider-clojure-cli-global-options "-A:dev")

(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

(setq logview-additional-submodes (quote
    (("RK"
      (format . "TIMESTAMP LEVEL THREAD NAME")
      (levels . "SLF4J")
      (timestamp)
      (aliases)))))


(defun lcs-prompt-for-jack-in-options (orig-fn project-type)
  (interactive)
  (let ((res (funcall orig-fn project-type)))
    (read-string "Additional options: " res)))

(advice-add 'cider-jack-in-global-options :around #'lcs-prompt-for-jack-in-options)

(provide 'cch-clojure)
