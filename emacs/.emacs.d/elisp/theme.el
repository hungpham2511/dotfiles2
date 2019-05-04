(require 'org)

(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
(set-face-font 'default "Monospace-11") ; default font
;; (set-face-font 'default "Inconsolata-14") ; default font
(global-hl-line-mode)			; highlight current line
(global-set-key (kbd "C-c e") 'eval-region)

(use-package gruvbox-theme
  :ensure t
  :defer
  :config
  (message "Configure gruvbox")
  )

(use-package solarized-theme
  :ensure t
  :defer
  :config
  (message "Configure solarized")
  )

(use-package all-the-icons
  :ensure t
  :defer
  )

(use-package kaolin-themes
  :ensure t
  :defer
  :config
  (message "Configure kaolin")
  )

(use-package zenburn-theme
  :ensure t
  :defer
  :config
  (message "Configure zenburn")
  )

(use-package doom-themes
  :ensure t
  :demand
  :config
  (message "Configure doom-themes")
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  ;; (load-theme 'doom-city-lights t)
  )

(provide 'theme)
