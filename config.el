;;; -*- lexical-binding: t; -*-

(setq user-full-name "Harry Tucker"
      user-mail-address "tucker.harry@outlook.com")

(org-babel-load-file (expand-file-name "modules/ui.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/editor.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/shell.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/programming.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/writing.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/tools.org" doom-user-dir))
(org-babel-load-file (expand-file-name "modules/workarounds.org" doom-user-dir))
