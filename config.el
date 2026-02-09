;;; -*- lexical-binding: t; -*-

;; Emacs User Identification
(setq user-full-name "Harry Tucker"
      user-mail-address "tucker.harry@outlook.com")

;; Load modular configuration files using org-babel-load-file
;; This allows us to keep configuration organized in separate .org files
(org-babel-load-file (expand-file-name "modules/ui.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/editor.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/shell.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/programming.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/writing.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/tools.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/workarounds.org" doom-user-dir))
