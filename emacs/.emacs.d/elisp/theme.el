(require 'org)

;;; Code:
(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
;; (set-face-font 'default "Monospace-10") ; default font
(set-face-font 'default "Hack-9:Style=Regular") ; default font
;; (set-face-font 'default "Fira Code Retina-9:style=Retina,Monospaced")
(global-hl-line-mode)			; highlight current line
(global-set-key (kbd "C-c e") 'eval-region)

(use-package smart-mode-line
  :straight t
  :after dracula-theme
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)
    (setq sml/theme 'dark)))

(use-package dracula-theme
  :straight t)

(use-package all-the-icons
  :straight t)

;; (use-package solarized
;;   :straight (emacs-color-theme-solarized
;; 	     :type git
;; 	     :host github
;; 	     :repo "sellout/emacs-color-theme-solarized")
;;   :config
;;   (load-theme 'solarized t)
;;   )


(provide 'theme)
