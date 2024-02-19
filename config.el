;; Emacs User Identification
(setq user-full-name "Harry Tucker"
      user-mail-address "tucker.harry@outlook.com")

;; Set themes for Doom and Treemacs, and select the JetBrains Mono font
(setq doom-theme 'doom-dracula
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec
                 :family "JetBrains Mono"
                 :size 14))

(defun random-element-of-list (items)
  "Return a random element from a given list."
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun splash-images ()
  "Return a list of available splash images."
  (let*
      ((splash-directory (concat doom-user-dir "images/")))
    (directory-files splash-directory 'full (rx ".png" eos))))

;; Sets the splash image to a random PNG file from the splash image directory
(setq fancy-splash-image (random-element-of-list (splash-images)))

;; Enables modeline icons with colour support
(setq doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t)

;; Highlights delimiter pairs with varying colours
(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

;; Lower the delay for displaying potential key chords
(setq which-key-idle-delay 0.2)

;; Allows undoing and redoing as a tree of changes, instead of being limited to
;; linear changes
(evil-set-undo-system 'undo-tree)

;; If running on a Mac, automatically fullscreen on launch
(if (featurep :system 'macos)
    (add-to-list 'default-frame-alist'(fullscreen . fullboth)))

;; Experimenting with Eshell's Plan 9 emulation, see here:
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(after! eshell
  (require 'em-smart)
  (eshell-smart-initialize))

;; Eshell's visual command handling is part of a separate library called
;; em-term, so defer loading this until em-term has been loaded
(after! em-term
  (add-to-list 'eshell-visual-commands "saml2aws"))

(use-package! projectile
  :defer
  :config
  ;; When looking for Go project files, I don't care about vendored dependencies
  (add-to-list 'projectile-globally-ignored-directories "*vendor"))

;; Provides configuration for working with SQL within Emacs
(use-package! sql
  :defer
  :config
  (sql-set-product 'postgres))

;; Provides configuration for using 'eslint' with Emacs
(use-package! lsp-eslint
  :defer
  :config
  ;; Automatically fix trivial lint failures
  (setq lsp-eslint-auto-fix-on-save t))

;; Provides configuration for working with the Rust programming language
(use-package! rustic
  :defer
  :config
  ;; Use 'rust-analyser', and enable options that allow further completion
  ;; support for crates that rely on macros and build scripts
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t))

;; Provides better Emacs support for Python docstrings
(use-package! python-docstring-mode
  :hook python-mode)

;; Provides configuration for using the Debug Adapter Protocol from Emacs
(use-package! dap-mode
  :defer
  :init
  ;; Provides key map for interacting with 'dap-mode' for debugging supported
  ;; languages
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

        ;; breakpoints
        :prefix ("db" . "Breakpoint")
        :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
        :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
        :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)
  :config
  ;; Rust template for DAP debugging
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

;; Provides configuration for working with remote machines over SSH using Tramp
(use-package! tramp
  :defer
  :config
  ;; Use sshx by default for speed
  ;; Don't use /bin/sh as the default shell
  (let ((+tramp-shell "/bin/bash"))
    (setq tramp-default-remote-shell +tramp-shell)
    (setq vterm-tramp-shells `(("ssh" ,+tramp-shell)
                               ("sshx" ,+tramp-shell)
                               ("docker" ,+tramp-shell)))))

;; Provides configuration for working with 'org-mode'
(use-package! org
  ;; Wrap text at 80 characters for better Git diffs and readability
  :hook (org-mode . auto-fill-mode)
  ;; Org is a large package, defer loading but also load during downtime to
  ;; prevent hangs on first load
  :defer-incrementally t
  :config
  ;; Hide emphasis markers that wrap text (i.e. bold, italics)
  (setq org-hide-emphasis-markers t)

  ;; Record timestamps on task completion for org-agenda usage
  (setq org-log-done 'time)
  (setq org-agenda-start-with-log-mode t)

  ;; Capture scrum notes into new files, adapted from:
  ;; https://stackoverflow.com/a/11903246
  (defun capture-retro-file (path)
    (let ((name (read-string "Sprint Number: ")))
      (expand-file-name (format "Sprint %s Retrospective.org" name) path)))

  ;; Add capture templates for scrum tasks
  (add-to-list 'org-capture-templates
               `("r" "Sprint Retrospective" entry
                 (file (lambda () (capture-retro-file "~/org/Reports")))
                 (file ,(concat doom-user-dir "templates/sprint.org"))))

  ;; Use 'pdf-tools' as the default viewer for exported Org documents
  (add-to-list 'org-file-apps '("\\.pdf\\'" . pdf-tools))
  ;; Enlarge top and second level heading fonts
  (custom-set-faces!
    '(org-level-1
      :height 1.2
      :inherit outline-1)
    '(org-level-2
      :height 1.1
      :inherit outline-2))
  ;; Collect agenda from Org and Org Roam
  (setq org-agenda-files '("~/org"
                           "~/org/roam"
                           "~/org/roam/daily"))
  ;; Enable export support for LaTeX and BibTeX formats
  (require 'ox-latex)
  (require 'ox-bibtex)


  ;; Tectonic is a self-contained LaTeX engine, and downloads dependencies on the
  ;; fly, so I can avoid using a massive install of texlive-full.
  ;;
  ;; It also handles re-runs automatically for tools like Bibtex.
  (setq org-latex-pdf-process
        `(,(concat "tectonic --outdir=%o %f -Z search-path=" doom-user-dir)))

  ;; Better syntax highlighting in exported LaTeX
  (setq org-latex-src-block-backend 'engraved)
  ;; Enable additional packages for exported LaTeX, takes the form:
  ;;    ("options" "package" SNIPPET-FLAG COMPILERS)
  (setq org-latex-packages-alist '(("" "booktabs")
                                   ("" "tabularx")
                                   ("" "color")))
  ;; Define 'mimore' LaTeX document class for use in exports
  (add-to-list 'org-latex-classes
               '("mimore"
                 "\\documentclass{mimore}\n\[NO-DEFAULT-PACKAGES\]\n\[PACKAGES\]\n\[EXTRA\]"
                 ("\\section{%s}" . "\\section\*{%s}")
                 ("\\subsection{%s}" . "\\subsection\*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection\*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph\*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph\*{%s}")))
  (setq org-latex-default-class "mimore"))


;; Provides 'org-modern' configuration in place of Doom's (org +pretty)
(use-package! org-modern
  :after org
  :config
  ;; Disable table formatting and star hiding, increase label border
  (setq org-modern-table nil
        org-modern-hide-stars nil
        org-modern-label-border 0.3)
  ;; Enable org-modern globally
  (global-org-modern-mode))

;; Provides support for presenting directly from 'org-mode' buffers
(use-package! org-tree-slide
  :after org
  :config
  ;; Hide formatting characters, use top-level headings as slides
  (setq org-tree-slide-skip-outline-level 2)
  ;; Use the fancy presentation profile, shiny animations!
  (org-tree-slide-presentation-profile))

;; Provides configuration for 'org-roam', an Emacs knowledge graph
(use-package! org-roam
  :after org
  :config
  ;; Hide common link types from org-roam graph
  (setq org-roam-graph-link-hidden-types
        '("file"
          "http"
          "https")))

;; Provides 'websocket', a dependency of 'org-roam-ui'
(use-package! websocket
  :after org-roam)

;; Provides 'org-roam-ui' a web frontend for 'org-roam'
(use-package! org-roam-ui
  :after org-roam
  :config
  ;; Sync UI theme with Emacs, follow current the buffer, update on save, and
  ;; open browser on start
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(map! :leader
      :prefix ("o" . "open")
      :desc "Open calendar" "c" #'cfw:open-org-calendar)

;; Enable scaling for HiDPI displays
(use-package! pdf-tools
  :defer
  :config
  (setq pdf-view-use-scaling t))
