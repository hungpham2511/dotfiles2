(require 'org)

(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
(set-face-font 'default "Monospace-13") ; default font
;; (set-face-font 'default "Inconsolata-14") ; default font
(global-hl-line-mode)			; highlight current line
(global-set-key (kbd "C-c e") 'eval-region)

(use-package smart-mode-line
  :ensure t
  :after dracula-theme
  :config
  (progn
    (sml/setup)
    (setq sml/theme 'respectful)
    )
  )

(use-package dracula-theme
  :ensure t
  )

(use-package all-the-icons
  :ensure t
  )

(provide 'theme)
