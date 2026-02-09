(evil-set-undo-system 'undo-tree)

(use-package! projectile
  :defer
  :config
  (add-to-list 'projectile-globally-ignored-directories "*vendor"))

(use-package! corfu
  :defer
  :config
  (setq corfu-preselect 'prompt))
