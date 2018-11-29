;;; package -- Overal Configuration
;;; Commentary:
; This file contains some overall configurations of my Emacs
;;; Code:
;; System configuraitons
(setq inhibit-splash-screen t)		; no splash screen, thanks
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(menu-bar-mode -1)
;; (set-face-font 'default "Monospace-11") ; default font
(set-face-font 'default "Inconsolata-14") ; default font
(global-hl-line-mode)			; highlight current line
(global-set-key (kbd "C-c e") 'eval-region)

(define-key global-map (kbd "C-x <backspace>") 'delete-frame)
(define-key global-map (kbd "C-x <RET>") 'make-frame)

;; Very useful mode, show balancing parens.

;; -----------------------------------------------------------------
(setq dired-listing-switches "-lah")

(provide 'overall-configs)
;;; overall_configurations ends here
