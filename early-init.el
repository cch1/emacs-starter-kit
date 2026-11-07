;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Work around native-comp failing on macOS 27 (GoldenGate): the libgccjit
;; bundled with the emacsformacosx.com Emacs.app derives the deployment target
;; from the Darwin kernel major minus 9 (27 -> "18.0"), which clang rejects.
;; Passing an explicit target to the native compiler driver fixes it.  Loading
;; `comp' first binds the variable so async (background) compiles inherit it.
;; Remove once https://github.com/caldwell/build-emacs/issues/158 is fixed.

;;; Code:

(when (and (eq system-type 'darwin)
           (featurep 'native-compile))
  (require 'comp)
  (setq native-comp-driver-options
        (cons "-mmacosx-version-min=11" native-comp-driver-options)))

;;; early-init.el ends here
