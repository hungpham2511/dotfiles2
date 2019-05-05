;;; package -- Overal Configuration
;;; Commentary:
; This file contains some overall configurations of my Emacs

;;; Code:
;; System configuraitons

(use-package ag
  :ensure t
  :defer t
  :config (setq ag-highlight-search t)
  :bind ("M-s 0" . ag)
  )

;; magit: the git porcelain
(use-package magit
  :ensure t
  :bind ("C-x C-z" . magit-status))

;; Install evil and its friends
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("C-f" . evil-avy-goto-char))
  :demand
  :config
  (evil-mode t)

  ;; Define additional scrolling functionality for evil-mode
  (defun scroll-down-half-page ()
    "Scroll down half page."
    (interactive)
    (evil-scroll-down 0))
  (defun scroll-up-half-page ()
    "Scroll down half page."
    (interactive)
    (evil-scroll-up 0))

  ;; key binding in evil and global-map
  (define-key evil-normal-state-map (kbd "C-i") 'scroll-up-half-page)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-down-half-page)
  (define-key evil-normal-state-map (kbd "C-j H") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "C-j V") 'evil-window-split)

  (define-key evil-normal-state-map (kbd "C-j j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-j k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-j o") 'other-window)

  (define-key evil-normal-state-map (kbd "C-j M-j") 'evil-window-increase-width)
  (define-key evil-normal-state-map (kbd "C-j M-k") 'evil-window-decrease-width)
  (define-key evil-normal-state-map (kbd "C-j M-h") 'evil-window-increase-height)
  (define-key evil-normal-state-map (kbd "C-j M-l") 'evil-window-decrease-height)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-i")     'evil-scroll-up)

  )

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :config
  ;; Evilnc keys must set up before org-capture
  (evilnc-default-hotkeys))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  ;; To surround a code segment with $, hightlight it then press `csm`.
  (add-hook 'org-mode-hook
	    (lambda () (push '(?m . ("$" . "$")) evil-surround-pairs-alist))))

(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode t)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-force-normal-state))

(use-package evil-magit
  :ensure t
  :after magit evil)

(setq dired-listing-switches "-lah")

(use-package smex
  :ensure t)

;; Nice switching windows
(use-package counsel
  :ensure t
  :demand
  :after smex
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("<f1>" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-ag)))

(use-package ivy
  :diminish t
  :ensure t
  :bind (("C-x C-c" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-plus))))


(provide 'overall-configs)
;;; overall_configurations ends here
