;; package -- Overal Configuration
;;; Commentary:
; This file contains some overall configurations of my Emacs

;;; Code:
;; System configuraitons
(require 'general)

;; Prevent littering from ~ files
(use-package no-littering
  :demand
  :straight t)

;; A very nice package for searching. I think this one beat everything
;; else.
(use-package rg
  :straight t
  :demand
  :after wgrep
  :config
  (rg-enable-default-bindings))

;; Use to show the available key bindings. Press the prefix key
;; combination then wait for 1 second.
(use-package which-key
  :straight t
  :demand
  :config
  (which-key-mode))

;; magit: the git porcelain
(use-package magit
  :straight t
  :config
  (general-define-key
   :states '(normal visual insert)
   :keymaps 'magit-file-mode-map
   "C-x C-z" 'magit-status
   "C-c g" 'magit-file-dispatch
   "C-c M-g" 'magit-dispatch)
  )

;; Install evil and its friends
(use-package evil
  :straight t
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
  (global-unset-key (kbd "C-j"))
  (define-key evil-normal-state-map (kbd "C-i") 'scroll-up-half-page)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-down-half-page)
  (define-key evil-normal-state-map (kbd "C-j H") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "C-j V") 'evil-window-split)
  (define-key evil-normal-state-map (kbd "C-j K") 'evil-window-delete)

  (define-key evil-normal-state-map (kbd "C-j j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-j k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-j o") 'other-window)

  ;; need to define keys in global map because sometimes
  ;; evil-normal-state-map is not enough.
  (define-key global-map (kbd "C-j j") 'evil-window-down)
  (define-key global-map (kbd "C-j k") 'evil-window-up)
  (define-key global-map (kbd "C-j h") 'evil-window-left)
  (define-key global-map (kbd "C-j l") 'evil-window-right)
  (define-key global-map (kbd "C-j o") 'other-window)

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
  
  ;; key for editing
  (general-define-key
   :states 'insert
   "C-n" 'next-line
   "C-p" 'previous-line
   "C-k" 'kill-line
   "C-e" 'end-of-line
   "C-a" 'beginning-of-line
   )

  )

(use-package evil-nerd-commenter
  :after evil
  :straight t
  :config
  (general-define-key
   :states '(normal visual)
   ",ci" 'evilnc-comment-or-uncomment-lines
   ",cl" 'evilnc-comment-or-uncomment-to-the-line
   ",cc" 'evilnc-copy-and-comment-lines
   ",cp" 'evilnc-comment-or-uncomment-paragraphs
   )
  )

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1)
  ;; To surround a code segment with $, hightlight it then press `csm`.
  (add-hook 'org-mode-hook
	    (lambda () (push '(?m . ("$" . "$")) evil-surround-pairs-alist))))


;; Sometimes doesn't work. need to rerun this script.
(use-package key-chord
  :straight t
  :after evil
  :demand
  :config
  (key-chord-mode t)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-force-normal-state))

(use-package evil-magit
  :straight t
  :after magit evil)

;; A move for navigation, jummping around basically
(use-package ace-jump-mode
  :straight t
  :config
  (message "ace-jump-mode loaded"))

(straight-use-package 'ace-link)
(setq dired-listing-switches "-lah")

(use-package smex
  :straight t)

;; Nice switching windows
(use-package counsel
  :straight t
  :demand
  :after smex ivy
  :general
  ("C-s" 'swiper-isearch)
  ("M-x" 'counsel-M-x)
  ("C-x C-f"  'counsel-find-file)
  ("C-c k" 'counsel-ag))

(use-package ivy
  :diminish t
  :straight t
  :demand
  :general
  ("C-x C-c" 'ivy-switch-buffer)
  ("C-c C-r" 'ivy-resume)
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

(use-package neotree
  :straight t
  :defer
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 40)

  (evil-define-key 'normal neotree-mode-map
    (kbd "TAB") 'neotree-enter
    (kbd "RET") 'neotree-enter
    (kbd "SPC") 'neotree-quick-look
    "q" 'neotree-hide
    "j" 'neotree-next-line
    "k" 'neotree-previous-line
    "H" 'neotree-hidden-file-toggle
    "g" 'neotree-refresh
    "A" 'neotree-stretch-toggle
    "C" 'neotree-change-root))

(use-package projectile
  :straight t
  :after org
  :init (message "Configuring projectile")
  :general 
  ("s-," 'projectile-command-map)
  
  :config (setq projectile-indexing-method 'hybrid
                projectile-completion-system 'ivy)
  (projectile-mode +1))


(provide 'config-general)
;;; overall_configurations ends here
