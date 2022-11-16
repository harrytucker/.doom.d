;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Python
(package! python-docstring) ; provides syntax highlighting and fill-paragraph
                            ; functionality

;; gRPC
(package! protobuf-mode) ; work with .proto files

;; Kubernetes
(package! kubernetes)
(package! kubernetes-evil)

;; Org
(unpin! org-roam) ; use latest version of org-roam
(package! org-roam-ui) ; enable org-roam-ui server
