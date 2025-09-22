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
(package! engrave-faces) ; comprehensive latex code output

;; AI Tools
(package!
  llm-tool-collection :recipe
  (:host github
   :repo "skissue/llm-tool-collection"))
(package!
  macher :recipe
  (:host github
   :repo "kmontag/macher"))

;; Miscellaneous
(package! command-log-mode)
(package! prism)
(package! flyover)
(package! eat)
