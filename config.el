(setq user-full-name "Harry Tucker" ; personal info
      user-mail-address "tucker.harry@outlook.com")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; rainbow brackets!

(setq doom-theme 'doom-dracula                  ; set doom theme
      doom-themes-treemacs-theme "doom-colors"  ; set treemacs theme
      doom-font (font-spec                      ; set font family
                 :family "JetBrains Mono"
                 :size 14))

(setq fancy-splash-image (concat doom-user-dir "images/power-splash.png"))

(setq doom-modeline-major-mode-icon t        ; enable modeline major-mode icon
      doom-modeline-major-mode-color-icon t) ; use coloured icons

(evil-set-undo-system 'undo-tree) ; tree-based undo and redo functionality

;; vcxsrv on windows has issues with this, so only enable on MacOS
(if IS-MAC (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(use-package! sql
  :defer
  :config
  (sql-set-product 'postgres))

(use-package! lsp-eslint
  :defer
  :config
  (setq lsp-eslint-auto-fix-on-save t))

(use-package! rustic
  :defer
  :config
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t))

(use-package! dap-mode
  :defer
  :config
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

(use-package! python-docstring-mode
  :hook python-mode)

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

(require 'ox-latex)   ; required for config
(require 'ox-bibtex)

(use-package! org
  :defer
  :config
  (custom-set-faces!
    '(org-level-1
      :height 1.2
      :inherit outline-1)
    '(org-level-2
      :height 1.1
      :inherit outline-2)))

(use-package! org-modern
  :after org
  :config
  (setq org-modern-table nil
        org-modern-hide-stars nil
        org-modern-label-border 0.3)
  (global-org-modern-mode))

(use-package! org
  :defer
  :config
  ;; Better syntax highlighting in exported LaTeX
  (setq org-latex-src-block-backend 'minted)
  ;; Enable additional packages for exported LaTeX, takes the form:
  ;;    ("options" "package" SNIPPET-FLAG COMPILERS)
  (setq org-latex-packages-alist '(("" "booktabs")
                                   ("" "tabularx")
                                   ("" "color")
                                   ("newfloat" "minted")))
  ;; Wrap text at 80 characters for better Git diffs and readability
  (add-hook! 'org-mode-hook #'auto-fill-mode))

(use-package! org-tree-slide
  :after org
  :config
  ;; Hide formatting characters, use top-level headings as slides
  (setq org-hide-emphasis-markers t
        org-tree-slide-skip-outline-level 2)
  ;; Use the fancy presentation profile, shiny animations!
  (org-tree-slide-presentation-profile))

(use-package! org
  :defer
  :config
  (add-to-list 'org-latex-classes
               '("mimore"
                 "\\documentclass{mimore}\n[NO-DEFAULT-PACKAGES\]\n[PACKAGES\]\n[EXTRA\]"
                 ("\\section{%s}" . "\\section\*{%s}")
                 ("\\subsection{%s}" . "\\subsection\*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection\*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph\*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph\*{%s}"))))

(use-package! org
  :defer
  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . pdf-tools)))

(use-package! org-roam
  :defer
  :config
  ;; Hide common link types from org-roam graph
  (setq org-roam-graph-link-hidden-types
        '("file"
          "http"
          "https")))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  ;; Sync UI theme with Emacs, follow current the buffer, update on save, and
  ;; open browser on start
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! kele
  :config
  (kele-mode 1))

(map! :leader
      :desc "Kubernetes" "k" #'kele-dispatch)

(setq pdf-view-use-scaling t          ; MacOS specific workarounds
      pdf-view-use-imagemagick nil)
