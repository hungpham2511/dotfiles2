;; package -- Overal Configuration
;;; Commentary:
; This file contains some overall configurations of my Emacs

;;; Code:
;; System configuraitons


(use-package ag
  :straight t
  :defer t
  :config (setq ag-highlight-search t)
  :bind ("M-s 0" . ag))

(use-package wgrep
  :straight t)

;; magit: the git porcelain
(use-package magit
  :straight t
  :bind ("C-x C-z" . magit-status))

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

  )

(use-package evil-nerd-commenter
  :after evil
  :straight t
  :config
  ;; Evilnc keys must set up before org-capture
  (evilnc-default-hotkeys))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1)
  ;; To surround a code segment with $, hightlight it then press `csm`.
  (add-hook 'org-mode-hook
	    (lambda () (push '(?m . ("$" . "$")) evil-surround-pairs-alist))))

(use-package key-chord
  :straight t
  :after evil
  :config
  (key-chord-mode t)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-force-normal-state))

(use-package evil-magit
  :straight t
  :after magit evil)

(use-package hydra
  :straight t)


(defhydra hydra-move (:columns 3)
   "Move around"
   ("j" next-line "Next line")
   ("k" previous-line "Previous line")
   ("l" forward-char "Forward char" )
   ("h" backward-char "Backward char")
   ("a" beginning-of-line "Beginning of line")
   ("e" move-end-of-line "End of line")

   ("u" (lambda ()
	  (interactive)
	  (scroll-down 15)) "Scroll up")

   ("d" (lambda ()
	  (interactive)
	  (scroll-up 15)) "Scroll down")

   ("s" isearch-forward "Isearch forward")

   ;; Converting M-v to V here by analogy.
   ("SPC" nil "Quit")
   )


(defhydra hydra-projectile (:color teal
			    :columns 4)
  "Projectile"
  ("f"   projectile-find-file                "Find File")
  ("r"   projectile-recentf                  "Recent Files")
  ("z"   projectile-cache-current-file       "Cache Current File")
  ("x"   projectile-remove-known-project     "Remove Known Project")

  ("t"   projectile-find-tag                 "Find tag")

  ("d"   projectile-find-dir                 "Find Directory")
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")
  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

  ("o"   projectile-multi-occur              "Multi Occur")
  ("s"   projectile-switch-project           "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")
  ("SPC"   nil "Cancel" :color blue)
  )



(defhydra hydra-window ()
   "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete
 "
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("n" next-line)
   ("p" previous-line)
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" backward-char)
   ("f" forward-char)
   ("F" follow-mode)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" org-open-at-point)
   ("i" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("SPC" nil)
   )


(straight-use-package 'general)

(global-unset-key (kbd "M-SPC"))


(general-define-key
 "M-SPC j" 'hydra-move/body
 "M-SPC p" 'hydra-projectile/body
 "M-SPC w" 'hydra-window/body )


(setq dired-listing-switches "-lah")



(use-package smex
  :straight t)

;; Nice switching windows
(use-package counsel
  :straight t
  :demand
  :after smex
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("<f1>" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-ag)))

(use-package ivy
  :diminish t
  :straight t
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
