;; https://github.com/clojure-emacs/clojure-mode/#indentation-options
;; http://cider.readthedocs.io/en/latest/indent_spec/
;; Note: before-save/after-save prettify hooks are installed globally via
;; cch/prettify-save-hooks in cch-generic.el, hooked to prettify-symbols-mode-hook.
(defun lisp-editing-behavior ()
  ;; https://github.com/jwiegley/use-package/issues/668
  ;; prettify-symbols-alist is buffer-local and must be set in a hook.
  (setq prettify-symbols-alist (append cch/prettify-logical cch/prettify-relational
  				       cch/prettify-set
  				       prettify-greek-lower prettify-greek-upper
  				       '(("#{}" . ?∅)
  					 ("fn" . ?ƒ)
  					 ("partial" . ?∂) ("comp" . ?∘) ("complement" . ?∁))))
  (local-set-key (kbd "{") 'paredit-open-curly)
  ;; prettify-symbols-mode bakes its font-lock keywords from
  ;; prettify-symbols-alist at activation time (via prog-mode-hook), which
  ;; fires before clojure-mode-hook runs lisp-editing-behavior and populates
  ;; the alist above.  Toggling the mode here forces it to re-read the now-
  ;; complete alist and recompile its keywords, so all symbols render.
  (when prettify-symbols-mode
    (prettify-symbols-mode -1)
    (prettify-symbols-mode +1)))

(defvar clojurey-mode-keywords
  '(("\\(#\\)("		; anonymous function
     (0 (progn (compose-region (match-beginning 1)
			       (match-end 1) "λ")
	       nil)))
    ("\\(#\\){[^}]"	; sets, but not the empty set
     (0 (progn (compose-region (match-beginning 1)
			       (match-end 1) "∈")
	       nil)))))

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
  ;; clojure-indent-style controls how continuation lines of a call form are
  ;; indented when arguments don't fit on the first line.  The three options:
  ;;
  ;;   'always-indent   – always 2 spaces from the opening paren (Lisp style)
  ;;   'align-arguments – align under first arg if it's on the same line,
  ;;                      otherwise fall back to 2-space indent (clojure-mode default)
  ;;   'always-align    – always align under the first argument (cljfmt default)
  ;;
  ;; cljfmt uses 'always-align, so we match it here to avoid indentation
  ;; conflicts between live editing and the cljfmt pre-commit hook.
  ;;
  ;; References:
  ;;   https://github.com/clojure-emacs/clojure-mode#indentation-options
  ;;   https://github.com/weavejester/cljfmt#indentation-types  (see :inner/:block)
  ;;   https://docs.cider.mx/cider/config/indentation.html
  :custom
  (clojure-indent-style 'always-align)
  :hook ((clojure-mode . lisp-editing-behavior))
  :init
  (mapc (lambda (mode) (font-lock-add-keywords mode clojurey-mode-keywords)) '(clojure-mode clojurescript-mode clojurec-mode))
  (setq scroll-conservatively 101))

(defun lcs-prompt-for-jack-in-options (orig-fn &rest args)
  (let ((current-prefix-arg '(4)))
    (apply orig-fn args)))

(defun enso/cider-fix-tooltips () ; https://github.com/clojure-emacs/cider/issues/3748#issuecomment-4061298624
  (when (eq window-system 'ns) ;; is macos
    (setq-local use-system-tooltips t)))

(use-package cider
  :ensure t
  :pin melpa-stable
  :init ; before the package loads
  (setq cider-auto-select-error-buffer t
	cider-popup-stacktraces nil
	cider-prompt-save-file-on-load nil ; C-c C-k
	cider-redirect-server-output-to-repl nil ; per Bozhidar https://github.com/clojure-emacs/cider/pull/1907#issuecomment-475938304
	cider-repl-buffer-size-limit 100000
	cider-repl-display-help-banner nil ; suppress start-up help banner
	;; cider-repl-display-in-current-window t
	cider-repl-history-file 'per-project ; https://docs.cider.mx/cider/repl/configuration.html#repl-history
	cider-repl-history-size 1000
	cider-repl-pop-to-buffer-on-connect 'display-only
	cider-repl-popup-stacktraces t
	cider-repl-print-length 200
	cider-stacktrace-default-filters '(tooling dup)
	cider-use-overlays nil ; Show eval results in minibuffer instead of inline
	cider-clojure-cli-global-aliases ""
	nrepl-buffer-name-show-port t)
  ;; cider-dynamic-indentation controls whether CIDER queries the connected
  ;; nREPL for :style/indent metadata on vars and uses it to dynamically
  ;; override clojure-indent-function properties at runtime.
  ;;
  ;; This is disabled for two reasons:
  ;;
  ;; 1. cljfmt ignores :style/indent metadata entirely -- it only uses its own
  ;;    :extra-indents rules from .cljfmt.edn.  If CIDER applies a library's
  ;;    :style/indent hint and it differs from .cljfmt.edn, Emacs and cljfmt
  ;;    disagree: the pre-commit hook reformats what Emacs just wrote.
  ;;
  ;; 2. Dynamic indentation overrides define-clojure-indent entries set in
  ;;    .dir-locals.el (which mirror .cljfmt.edn).  A CIDER connection would
  ;;    silently undo that carefully maintained alignment.
  ;;
  ;; With this set to nil, CIDER defers to the static define-clojure-indent
  ;; rules, keeping live editing, on-save re-indentation, and cljfmt in sync.
  ;;
  ;; References:
  ;;   https://docs.cider.mx/cider/config/indentation.html
  ;;   https://github.com/weavejester/cljfmt#indentation-types
  :custom
  (cider-dynamic-indentation nil)
  :hook ((cider-mode . enso/cider-fix-tooltips))				;
  :bind ((:map cider-repl-mode-map
	       ("M-s-<up>" . cider-repl-previous-input)
	       ("M-s-<down>" . cider-repl-next-input)
	       ("M-RET" . cider-repl-closing-return)))
  :config
  (advice-add 'cider-jack-in :around 'lcs-prompt-for-jack-in-options)
  (advice-add 'cider-jack-in-clj :around 'lcs-prompt-for-jack-in-options)
  (lisp-editing-behavior))

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :custom (eca-chat-auto-add-repomap t))

(put 'cider-clojure-cli-aliases 'safe-local-variable (lambda (x) t))
(put 'cider-clojure-cli-global-options 'safe-local-variable (lambda (x) t))

(provide 'cch-clojure)
