;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Doom Emacs
;;
;; Some magit dependencies are expecting functions defined in Emacs 29, and
;; breaking, so pinning these packages as per:
;; https://emacs.stackexchange.com/questions/75827/doom-emacs-error-running-hook-global-git-commit-mode-because-void-variable
(package! transient
  :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
  :recipe (:host github :repo "magit/transient"))

(package! with-editor
  :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
  :recipe (:host github :repo "magit/with-editor"))


;; Python
(package! python-docstring) ; provides syntax highlighting and fill-paragraph
                            ; functionality

;; gRPC
(package! protobuf-mode) ; work with .proto files

;; Kubernetes
(package! kele)

;; Org
(unpin! org-roam) ; use latest version of org-roam
(package! org-roam-ui) ; enable org-roam-ui server
(package! org-modern) ; nicer fontification than doom's +pretty
