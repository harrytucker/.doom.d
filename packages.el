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

;; PostgreSQL
(package! ; dependency of pgmacs
  pg :recipe
  (:host github
   :repo "emarsden/pg-el"))
(package!
  pgmacs :recipe
  (:host github
   :repo "emarsden/pgmacs"))

;; AI Tools
(unpin! gptel) ; use latest version of gptel, many bugs abound
(package!
  macher :recipe
  (:host github
   :repo "kmontag/macher"))
(package!
  eca :recipe
  (:host github
   :repo "editor-code-assistant/eca-emacs"
   :files ("*.el")))
;; Agent Shell Stuff
(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Miscellaneous
(package! command-log-mode)
(package! prism)
(package! flyover)
(package! eat)
