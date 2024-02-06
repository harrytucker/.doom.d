;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Doom Emacs

;; Behaviour Driven Development (BDD)
(package! feature-mode)

;; Python
(package! python-docstring) ; provides syntax highlighting and fill-paragraph
                            ; functionality

;; gRPC
(package! protobuf-mode) ; work with .proto files

;; Kubernetes
(package! kele)
(package! jsonnet-mode)

;; Org
(unpin! org-roam) ; use latest version of org-roam
(package! org-roam-ui) ; enable org-roam-ui server
(package! org-modern) ; nicer fontification than doom's +pretty
(package! engrave-faces) ; comprehensive latex code output

;; Miscellaneous
(package! command-log-mode)
