(setq user-full-name "Harry Tucker")

(require 'ox-latex)

(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(setq org-latex-listings 'minted
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("" "tabularx"))

(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq doom-themes-treemacs-theme "doom-colors")

(setq display-line-numbers-type 'relative)

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(map! (:when (featurep! :lang latex)
       (:map LaTeX-mode-map
        :localleader
        :desc "Compile LaTeX document" "c" #'TeX-command-run-all)))

(if (getenv "WSL_DISTRO_NAME")
    (setq projectile-indexing-method 'native))

(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)
