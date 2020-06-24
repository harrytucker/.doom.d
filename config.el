(setq user-full-name "Harry Tucker")

(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)
