(setq user-full-name "Harry Tucker" ; personal info
      user-mail-address "tucker.harry@outlook.com")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; rainbow brackets!

(setq doom-theme 'doom-dracula                  ; set doom theme
      doom-themes-treemacs-theme "doom-colors"  ; set treemacs theme
      doom-font (font-spec                      ; set font family
                 :family "JetBrains Mono"
                 :size 14))

(setq display-line-numbers-type 'relative)

(setq doom-modeline-major-mode-icon t        ; enable modeline major-mode icon
      doom-modeline-major-mode-color-icon t) ; use coloured icons

(evil-set-undo-system 'undo-tree) ; tree-based undo and redo functionality

(require 'sql)
(sql-set-product 'postgres) ; use postgres dialect for sql

(setq lsp-rust-server 'rust-analyzer) ; set language server

(setq lsp-rust-analyzer-proc-macro-enable t        ; enable proc macros
      lsp-rust-analyzer-cargo-run-build-scripts t) ; enable build scripts

(add-hook 'python-mode-hook 'python-docstring-mode) ; load python-docstring-mode
                                                    ; when opening a python
                                                    ; buffer

(require 'ox-latex)   ; required for config
(require 'ox-bibtex)

(add-to-list 'org-latex-packages-alist '("" "minted")) ; include in org-latex
(add-to-list 'org-latex-packages-alist '("" "color"))  ; export

(setq org-latex-listings 'minted org-latex-pdf-process ; enable shell-escapes
                                                       ; for minted
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "booktabs")) ; include in org-latex
(add-to-list 'org-latex-packages-alist '("" "tabularx")) ; export

(setq font-latex-fontify-sectioning 1.3) ; increase section font scaling

(map! (:when (featurep! :lang latex) ; custom keymap using local leader
       (:map LaTeX-mode-map
        :localleader
        :desc "Compile" "c" #'TeX-command-run-all
        :desc "Insert environment" "e" #'LaTeX-environment
        :desc "Insert section" "s" #'LaTeX-section
        :desc "Format document" "f" #'LaTeX-fill-buffer
        :desc "Fold buffer" "," #'TeX-fold-buffer
        :desc "Unfold buffer" "." #'TeX-fold-clearout-buffer)))

(require 'tex-fold) ; required for config

(add-hook 'LaTeX-mode-hook #'TeX-fold-mode)     ; enable TeX-fold-mode
(add-hook 'after-find-file 'TeX-fold-buffer t)  ; auto-fold sections on load
(add-hook 'LaTeX-mode-hook 'font-latex-update-sectioning-faces) ; update section
                                                                ; font sizes

(add-hook 'LaTeX-mode-hook #'orgtbl-mode) ; enable orgtbl mode for LaTeX

(setq TeX-view-program-selection '((output-pdf "PDF Tools") ; pdf tool
        (output-pdf "Zathura")                              ; preferences
        ((output-dvi has-no-display-manager) "dvi2tty")
        ((output-dvi style-pstricks) "dvips and gv")
        (output-dvi "xdvi")
        (output-pdf "Evince")
        (output-html "xdg-open")
        (output-pdf "preview-pane")))

(setq pdf-view-use-scaling t          ; MacOS specific workarounds
      pdf-view-use-imagemagick nil)
