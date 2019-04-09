(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
;; (set-face-font 'default "Monospace-11") ; default font
(set-face-font 'default "Inconsolata-14") ; default font
(global-hl-line-mode)			; highlight current line
(global-set-key (kbd "C-c e") 'eval-region)

(use-package gruvbox-theme
  :ensure t)

;; for some reason this package is needed to run highlight symbol
(use-package solarized-theme
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package kaolin-themes
  :ensure t)

(use-package zenburn-theme
  :ensure t)


(provide 'theme)
