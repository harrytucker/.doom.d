(setq user-full-name "Harry Tucker" ; personal info
      user-mail-address "tucker.harry@outlook.com")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; rainbow brackets!

(setq doom-theme 'doom-dracula                  ; set doom theme
      doom-themes-treemacs-theme "doom-colors"  ; set treemacs theme
      doom-font (font-spec                      ; set font family
                 :family "JetBrains Mono"
                 :size 14))

(setq doom-modeline-major-mode-icon t        ; enable modeline major-mode icon
      doom-modeline-major-mode-color-icon t) ; use coloured icons

(evil-set-undo-system 'undo-tree) ; tree-based undo and redo functionality

; vcxsrv on windows has issues with this, so only enable on MacOS
(if IS-MAC (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(require 'sql)
(sql-set-product 'postgres) ; use postgres dialect for sql

(setq lsp-rust-server 'rust-analyzer
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-cargo-run-build-scripts t)

(add-hook 'python-mode-hook #'python-docstring-mode)

(require 'ox-latex)   ; required for config
(require 'ox-bibtex)

(add-to-list 'org-latex-packages-alist '("" "booktabs")) ; include in org-latex
(add-to-list 'org-latex-packages-alist '("" "tabularx")) ; export

(add-hook 'org-mode-hook #'auto-fill-mode)

(setq pdf-view-use-scaling t          ; MacOS specific workarounds
      pdf-view-use-imagemagick nil)
