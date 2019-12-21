;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; UI Configuration
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Fira Code" :size 14))
(setq display-line-numbers-type 'relative) ; this makes using vi bindings a bit nicer

;; Window Configuration
(setq neo-window-fixed-size nil)

;; Hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
