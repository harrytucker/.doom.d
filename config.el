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

;; vcxsrv on windows has issues with this, so only enable on MacOS
(if IS-MAC (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(require 'sql)
(sql-set-product 'postgres) ; use postgres dialect for sql

(defconst protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "protobuf-style"
                                  protobuf-style t)))

(setq lsp-rust-server 'rust-analyzer
      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-cargo-run-build-scripts t)

(require 'dap-mode)
(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))

(add-hook 'python-mode-hook #'python-docstring-mode)

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

(add-to-list 'org-latex-packages-alist '("" "booktabs")) ; include in org-latex
(add-to-list 'org-latex-packages-alist '("" "tabularx")) ; export

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

(setq org-latex-src-block-backend 'minted)

(add-hook 'org-mode-hook #'auto-fill-mode)

(setq org-tree-slide-skip-outline-level 2)
(setq org-hide-emphasis-markers t)
(org-tree-slide-presentation-profile)

(add-to-list 'org-latex-classes
             '("mimore"
               "\\documentclass{mimore}
\[NO-DEFAULT-PACKAGES\]
\[PACKAGES\]
\[EXTRA\]"
               ("\\section{%s}" . "\\section\*{%s}")
               ("\\subsection{%s}" . "\\subsection\*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection\*{%s}")
               ("\\paragraph{%s}" . "\\paragraph\*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph\*{%s}")))

(add-to-list 'org-file-apps '("\\.pdf\\'" . pdf-tools))

(setq org-roam-graph-link-hidden-types
      '("file"
        "http"
        "https"))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(map! :leader
      (:prefix "o"
       :desc "Kubernetes" "k" #'kubernetes-overview))

(setq pdf-view-use-scaling t          ; MacOS specific workarounds
      pdf-view-use-imagemagick nil)
