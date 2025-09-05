(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "/Users/cch1/.emacs.d/custom.el")
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval append clojure-align-binding-forms '("match")) (eval append clojure-align-binding-forms '("mlet" "rlet" "match"))
     (org-publish-project-alist
      ("gh-notes" :base-directory "." :base-extension "org" :exclude "setup.org" :publishing-directory "../gh-pages" :recursive t
       :publishing-function org-html-publish-to-html :headline-levels 4 :auto-preamble t)
      ("gh-assets" :base-directory "." :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf" :publishing-directory "../gh-pages"
       :recursive t :publishing-function org-publish-attachment)
      ("gh-pages" :components ("gh-notes" "gh-assets")))
     (eval setq clojure-align-forms-automatically nil))))
(setq-default fill-column 144)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idle-highlight ((t (:box (:line-width -1 :color "grey75")))) t))
