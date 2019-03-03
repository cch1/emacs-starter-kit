(defvar my-clojurescript-packages
  '()
  "A list of packages to ensure are installed at launch.")

(dolist (p my-clojure-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; CIDER mode settings
(setq cider-cljs-lein-repl
	"(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

(provide 'cch-clojurescript)
