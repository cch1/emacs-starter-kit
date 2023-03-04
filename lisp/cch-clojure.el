;; https://github.com/clojure-emacs/clojure-mode/#indentation-options
;; http://cider.readthedocs.io/en/latest/indent_spec/
(defun before-save-unprettify ()
  (save-excursion
    ;; re-indent without prettify
    (prettify-symbols-mode -1)
    (indent-region (point-min) (point-max))
    ;; Trim trailing whitespace on write buffer.
    ;; Note that trailing newlines at the end of the file are NOT trimmed.
    (delete-trailing-whitespace)))

(defun after-save-reprettify ()
  (save-excursion
    ;; re-indent with prettify
    (prettify-symbols-mode 't)
    (indent-region (point-min) (point-max))
    (set-buffer-modified-p nil)))

(defun lisp-editing-behavior ()
  ;; https://github.com/jwiegley/use-package/issues/668
  (add-hook 'before-save-hook #'before-save-unprettify nil t)
  (add-hook 'after-save-hook  #'after-save-reprettify nil t)
  ;; prettify-symbols-alist is buffer-local and must be set in a hook.
  (setq prettify-symbols-alist (append cch/prettify-logical cch/prettify-relational
  				       cch/prettify-set
  				       prettify-greek-lower prettify-greek-upper
  				       '(("#{}" . ?∅)
  					 ("fn" . ?ƒ)
  					 ("partial" . ?∂) ("comp" . ?∘) ("complement" . ?∁))))
  (local-set-key (kbd "{") 'paredit-open-curly))

(defvar clojurey-mode-keywords
  '(("\\(#\\)("		; anonymous function
     (0 (progn (compose-region (match-beginning 1)
			       (match-end 1) "λ")
	       nil)))
    ("\\(#\\){[^}]"	; sets, but not the empty set
     (0 (progn (compose-region (match-beginning 1)
			       (match-end 1) "∈")
	       nil)))))

(defun lcs-prompt-for-jack-in-options (orig-fn project-type)
  (interactive)
  (let ((res (funcall orig-fn project-type)))
    (read-string "Additional options: " res)))

;; https://github.com/admiralbumblebee/cider-rebl
;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
;; (add-hook 'cider-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
;;             (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . lisp-editing-behavior))
  :init
  (mapc (lambda (mode) (font-lock-add-keywords mode clojurey-mode-keywords)) '(clojure-mode clojurescript-mode clojurec-mode))
  (setq scroll-conservatively 101))

(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-auto-select-error-buffer t
	cider-popup-stacktraces nil
	cider-prompt-save-file-on-load nil ; C-c C-k
	cider-redirect-server-output-to-repl nil ; per Bozhidar https://github.com/clojure-emacs/cider/pull/1907#issuecomment-475938304
	cider-repl-buffer-size-limit 100000
	cider-repl-display-help-banner nil ; suppress start-up help banner
	;; cider-repl-display-in-current-window t
	cider-repl-history-file "~/.emacs.d/cache/cider-history"
	cider-repl-pop-to-buffer-on-connect t
	cider-repl-popup-stacktraces t
	cider-repl-print-length 200
	cider-stacktrace-default-filters '(tooling dup)
	cider-use-overlays nil ; Show eval results in minibuffer instead of inline
	nrepl-buffer-name-show-port t)
  :bind ((:map cider-repl-mode-map
	       ("M-s-<up>" . cider-repl-previous-input)
	       ("M-s-<down>" . cider-repl-next-input)
	       ("M-RET" . cider-repl-closing-return)))
  :config
  (advice-add 'cider-jack-in-global-options :around #'lcs-prompt-for-jack-in-options)
  (lisp-editing-behavior))

(put 'cider-clojure-cli-aliases 'safe-local-variable (lambda (x) t))
(put 'cider-clojure-cli-global-options 'safe-local-variable (lambda (x) t))

(provide 'cch-clojure)
