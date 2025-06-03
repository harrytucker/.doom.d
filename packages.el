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

;; AI!

;; Trying out GPTEL as it seems more advanced than the plugins I've been using
;; up until now.
(package! gptel :recipe (:nonrecursive t))

;; Elysium is a gptel add-on that allows you to merge in code changes with a
;; side-by-side chat buffer.
(package! elysium)

;; Miscellaneous
(package! command-log-mode)
