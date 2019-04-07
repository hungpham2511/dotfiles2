; Commentary:
(require 'package)

;;; Code:
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; directory containing my personal elisp scripts
(add-to-list 'load-path "~/.emacs.d/elisp/")

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))


;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Configure core configurations: evil, magit, etc
(use-package overall-configs
  :load-path "~/.emacs.d/elisp"
  :demand)

;; Configure appearances
(use-package theme
  :load-path "~/.emacs.d/elisp"
  :demand
  :config
  ;; (load-theme 'gruvbox-dark-hard t)
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme)
  )

;; setup stage 3: python configuration
(use-package jedi-core
  :ensure t)
(require 'python-configs nil t)  ;; require is similar to include.

;; setup stage 4: configure org setup
(use-package my-org-configs
  :load-path "~/.emacs.d/elisp"
  :demand
  :config
  (evil-define-key 'normal bibtex-mode-map (kbd "SPC") 'org-bibtex-generate-org-entry)
  (evil-define-key 'normal org-mode-map (kbd "SPC") 'org-sort-by-year))

;; My personal library, some snippets thats it
(use-package config-misc
  :load-path "~/.emacs.d/elisp"
  :demand
  ;; :after evil org
  :bind (("<f5>" . org-random-entry-TODO)
	 ("<f6>" . org-random-entry-TOREAD)
	 ("<f7>" . org-random-entry-STUDY)
	 ("<f1>" . find-dired-dropbox-iname)
	 :map org-mode-map
	 ("C-c C-q" . counsel-org-tag-2))
  :config
  (progn
    (setq browse-url-browser-function 'browse-url-chrome)))

;; Programming/modes installation
(use-package programming
  :load-path "~/.emacs.d/elisp"
  :demand)

(use-package config-latex
  :load-path "~/.emacs.d/elisp"
  :demand)


(use-package config-org
  :load-path "~/.emacs.d/elisp"
  :demand)

;; (require 'ob-latex)
;; (use-package ox-latex
;;   :config
;;   (progn
;;     (setq org-latex-pdf-process (list "latexmk -pdf %f"))
;;     (unless (boundp 'org-latex-classes)
;;       (setq org-latex-classes nil))
;;     (add-to-list 'org-latex-classes
;; 		 '("ieeetran"
;; 		   "\\documentclass{IEEEtran}
;; \\usepackage{bm}
;; \\usepackage{mathrsfs}
;; \\usepackage{siunitx}
;; \\renewcommand{\\vec}[1]{\\bm{\\mathrm{#1}}}
;; \\usepackage[ruled, linesnumbered]{algorithm2e}

;; \\SetKwIF{If}{ElseIf}{Else}{if}{ then}{elif}{else}{}%
;; \\SetKwFor{For}{for}{ do}{}%
;; \\SetKwFor{ForEach}{foreach}{ do}{}%
;; \\SetKwInOut{Input}{Input}%
;; \\SetKwInOut{Output}{Output}%
;; \\AlgoDontDisplayBlockMarkers%
;; \\SetAlgoNoEnd%
;; \\SetAlgoNoLine%
;; \\DontPrintSemicolon

;; \\usepackage{amsthm}

;; \\theoremstyle{plain}
;; \\newtheorem{theorem}{Theorem}
;; \\newtheorem{lem}{Lemma}
;; \\newtheorem{proposition}{Proposition}

;; "

;; 		   ("\\section{%s}" . "\\section*{%s}")
;; 		   ("\\subsection{%s}" . "\\subsection*{%s}")
;; 		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;     (add-to-list 'org-latex-classes
;; 		 '("koma-article"
;; 		   "\\documentclass[draft=false, parskip=half, toc=selection, BCOR=8.25mm, DIV=15]{scrartcl}
;; \\usepackage{bm}
;; \\usepackage{mathrsfs}
;; \\usepackage{siunitx}
;; \\usepackage{graphicx} 
;; \\usepackage{tikz}
;; \\renewcommand{\\vec}[1]{\\bm{\\mathrm{#1}}}"
;; 		   ("\\section{%s}" . "\\section*{%s}")
;; 		   ("\\subsection{%s}" . "\\subsection*{%s}")
;; 		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; ;; Completion package
;; (use-package company
;;   :ensure t
;;   :bind (
;; 	 ("C-M-i" . company-complete)
;; 	 :map python-mode-map
;; 	 ("C-M-i" . company-complete)
;; 	 :map emacs-lisp-mode-map
;; 	 ("C-M-i" . company-complete)
;; 	 )
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1))

;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'smartparens-config)
;;     (sp-local-pair 'org-mode "$" "$")
;;     (eval-after-load 'org-mode     '(require 'smartparens-org))
;;     (eval-after-load 'python-mode   '(require 'smartparens-python))
;;     (show-smartparens-global-mode)
;;     (smartparens-global-mode)
;;     (set-face-foreground 'sp-show-pair-match-face "#8ec07c")))
;; ;; emacs company jedi work with ros
;; (use-package company-jedi
;;   :load-path "/home/hung/.emacs.d/git/emacs-company-jedi"
;;   :config (progn
;; 	    (defun my/python-mode-hook ()
;; 	      (add-to-list 'company-backends 'company-jedi))
;; 	    (add-hook 'python-mode-hook 'my/python-mode-hook)
;; 	    (add-hook 'python-mode-hook 'jedi:setup)
;; 	    (setq jedi:complete-on-dot t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("dc9a8d70c4f94a28aafc7833f8d05667601968e6c9bf998791c39fcb3e4679c9" "5a970147df34752ed45bfdf0729233abfc085d9673ae7e40210c5e2d8f624b08" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/org/papers.org" "/home/hung/org/research.org" "/home/hung/org/refile.org" "/home/hung/org/study.org" "/home/hung/org/people.org" "/home/hung/org/test.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (all-the-icons ox-reveal reveal-ox ivy-bibtex irony-eldoc cmake-mode magit gruvbox-theme docker-tramp scad-mode plantuml-mode haskell-mode ein writeroom-mode matlab-mode cython-mode ggtags all-the-icons-dired dired-sidebar ob-blockdiag ob-ipython my-misc-config company-anaconda anaconda-mode overall_configurations overall-configurations org-element smartparens cdlatex hydra ess lua-mode yasnippet-snippets company-mode company-jedi markdown-mode yaml-mode paredit evil-magit company-irony irony traad pymacs projectile rebecca-theme solarized-theme org-pdfview highlight-symbol smex avy use-package)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:width expanded)))))
