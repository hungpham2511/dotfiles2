(require 'org)

;;; Code:
(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
(fringe-mode '(4 . 0))                  ; left fringe only, and a very small one
;; (set-face-font 'default "Monospace-10") ; default font
(set-face-font 'default "Hack-11:Style=Regular") ; default font
;; (set-face-font 'default "Fira Code Retina-9:style=Retina,Monospaced")
(global-hl-line-mode)			; highlight current line

(use-package dracula-theme
  :straight t)

(use-package all-the-icons
  :straight t)

(use-package solarized-theme
  :straight t
  :ensure t
  :config
  ;; wombat color-theme with misc face definition
  (solarized-create-theme-file-with-palette 'dark 'my-solarized-dark
    '("#002b36" "#fdf6e3"
      "#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900")

    '((custom-theme-set-faces
       theme-name
       `(org-code ((,class (:foreground ,blue))))
       ))
    )

  (message "Loaded solarized theme")
  )

(use-package smart-mode-line
  :straight t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)
    (setq sml/theme 'dark))
  )



(provide 'config-theme)
;;; theme.el ends here
