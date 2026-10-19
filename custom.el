(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "/Users/cch1/.emacs.d/custom.el")
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(package-selected-packages
   '(ac-js2 afternoon-theme aggressive-indent alect-themes ample-theme ample-zen-theme anti-zenburn-theme badger-theme bind-key bubbleberry-theme
	    bufferfile cider clojure-ts-mode color-theme-actress color-theme-complexity color-theme-dg color-theme-emacs-revert-theme
	    color-theme-sanityinc-solarized color-theme-solarized company-terraform csv-mode diminish direnv dockerfile-mode eca editorconfig
	    elisp-dev-mcp exec-path-from-shell fic-mode gcmh gh-md git-gutter-fringe go-mode grip-mode grunt highlight-parentheses hl-todo
	    idle-highlight-mode ido-vertical-mode inf-clojure intero jade-mode json-mode ligature logview magit markdown-mode mbo70s-theme
	    moe-theme molokai-theme mustache-mode nodejs-repl origami ox-gfm pandoc-mode paredit prettify-greek pretty-symbols rotate smex
	    solarized-theme sws-mode tango-plus-theme tangotango-theme terraform-doc transient which-key yaml-mode))
 '(safe-local-variable-values
   '((eval define-clojure-indent (u/log 0) (u/trace 0) (h/html 0) (m/search 0) (m/find 0) (m/match 0) (m/rewrite 0) (meander/search 0)
	   (meander/find 0) (meander/match 0) (meander/rewrite 0) (pxx 1) (assert! 0) (synchronize 0))
     (eval append clojure-align-binding-forms '("match")) (eval append clojure-align-binding-forms '("mlet" "rlet" "match"))
     (org-publish-project-alist
      ("gh-notes" :base-directory "." :base-extension "org" :exclude "setup.org" :publishing-directory "../gh-pages" :recursive t
       :publishing-function org-html-publish-to-html :headline-levels 4 :auto-preamble t)
      ("gh-assets" :base-directory "." :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf" :publishing-directory "../gh-pages"
       :recursive t :publishing-function org-publish-attachment)
      ("gh-pages" :components ("gh-notes" "gh-assets")))
     (eval setq clojure-align-forms-automatically nil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idle-highlight ((t (:box (:line-width -1 :color "grey75"))))))
