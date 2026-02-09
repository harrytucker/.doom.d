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

(setq fancy-splash-image (random-element-of-list (splash-images)))

(setq doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t)

;; Scope-based syntax highlighting (replaces rainbow-delimiters)
(add-hook! 'prog-mode-hook #'prism-mode)
(add-hook! '(python-mode-hook
             python-ts-mode-hook
             yaml-mode-hook
             yaml-ts-mode-hook)
           #'prism-whitespace-mode)

;; Faster which-key popup
(setq which-key-idle-delay 0.2)

;; Smooth scrolling
(pixel-scroll-precision-mode)

;; Auto-focus help windows
(setq help-window-select t)
