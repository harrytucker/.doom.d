;; Emacs User Identification
(setq user-full-name "Harry Tucker"
      user-mail-address "tucker.harry@outlook.com")

;; Set themes for Doom and Treemacs, and select the JetBrains Mono font
(setq doom-theme 'doom-dracula
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec
                 :family "JetBrainsMono Nerd Font"
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

;; Highlights scope with various colours, replaces rainbow-delimiters-mode
(add-hook! 'prog-mode-hook #'prism-mode)
(add-hook! 'python-mode-hook #'prism-whitespace-mode)
(add-hook! 'yaml-mode-hook #'prism-whitespace-mode)

;; Lower the delay for displaying potential key chords
(setq which-key-idle-delay 0.2)

;; Allows undoing and redoing as a tree of changes, instead of being limited to
;; linear changes
(evil-set-undo-system 'undo-tree)

;; Experimenting with Eshell's Plan 9 emulation, see here:
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(after! eshell
  (require 'em-smart)
  (eshell-smart-initialize))

;; Point bash-completion at the Homebrew installed Bash version as MacOS ships
;; an outdated Bash version.
(after! bash-completion
  (setq bash-completion-prog "/opt/homebrew/bin/bash"))

(use-package! projectile
  :defer
  :config
  ;; When looking for Go project files, I don't care about vendored dependencies
  (add-to-list 'projectile-globally-ignored-directories "*vendor"))

(use-package! corfu
  :defer
  :config
  ;; Automatically select the first valid completion candidate
  (setq corfu-preselect 'prompt))

;; Provides configuration for working with SQL within Emacs
(use-package! sql
  :defer
  :config
  (sql-set-product 'postgres))

;; Use pg_format for formatting SQL files upon saving the buffer
(set-formatter! 'pg_format '("pg_format" "--comma-break" "--keep-newline" "-") :modes '(sql-mode))
(setq-hook! 'sql-mode-hook +format-with 'pg_format)
(add-hook! 'sql-mode-hook #'apheleia-mode)

;; Doom disables SQL formatting by default, so you need to override this.
(delete 'sql-mode +format-on-save-disabled-modes)

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
        lsp-rust-analyzer-cargo-run-build-scripts t)
  ;; For some reason this has been lowered to 2 which conflicts with rustfmt,
  ;; override this back to 4 spaces
  (setq rustic-indent-offset 4))

;; Provides better Emacs support for Python docstrings
(use-package! python-docstring-mode
  :hook python-mode)

(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

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
        ;; PDF process expects a list of commands here as pdflatex normally
        ;; needs to be repeated.
        ;;
        ;; Since tectonic handles this automatically, we only need one item. The
        ;; `,` marker is used to partially evaluate the list so that
        ;; `doom-user-dir` is correctly concatenated using its value.
        `(,(concat "tectonic --outdir=%o %f -Z search-path=" doom-user-dir "latex")))

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
      :desc "Open calendar" "C" #'cfw:open-org-calendar)

;; You'll need to require the auth-source library. It's built into Emacs.
(require 'auth-source)

;; Fetch API credentials from ~/.authinfo or ~/.netrc
(let ((credential (auth-source-user-and-password "api.github.com")))
  (setq grip-github-user (car credential)
        grip-github-password (cadr credential)))

;; Configure `gptel` to use your model of choice, I'm using Claude 3.7 Sonnet
;; on Github Copilot.
(use-package! gptel
  :config
  ;; configure model and GitHub backend
  (setq gptel-model 'claude-sonnet-4
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  (add-hook! 'gptel-post-stream-hook 'gptel-auto-scroll))

;; Enable scaling for HiDPI displays
(use-package! pdf-tools
  :defer
  :config
  (setq pdf-view-use-scaling t))

;; Enable precision scrolling globally
(pixel-scroll-precision-mode)

;; Rebind hash key
(map!
  :desc "Override M-3 to insert # rather than change workspace when in insert mode"
  :i "M-3"
  #'(lambda () (interactive) (insert "#")))

(if (featurep :system 'macos) (setq Man-sed-command "gsed"))
