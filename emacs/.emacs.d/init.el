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
  :init
  (message "Configure appearances"))

;; setup stage 3: python configuration
(use-package jedi-core
  :ensure t
  :config
  (jedi:install-server))

(use-package python-configs
  :load-path "~/.emacs.d/elisp"
  :demand
  :after jedi-core)

;; setup stage 4: configure org setup
;; My personal library, some snippets thats it
(use-package config-misc
  :load-path "~/.emacs.d/elisp"
  :demand
  :after org
  ;; :after evil org
  :bind (("<f5>" . org-random-entry-TODO)
	 ("<f6>" . org-random-entry-TOREAD)
	 ("<f7>" . org-random-entry-STUDY)
	 ("<f1>" . find-dired-dropbox-iname)
	 :map org-mode-map
	 ("C-c C-q" . counsel-org-tag-2))
  :config
  (setq browse-url-browser-function 'browse-url-chrome))

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

(use-package my-org-configs
  :load-path "~/.emacs.d/elisp"
  :after org
  :config
  (evil-define-key 'normal bibtex-mode-map (kbd "SPC") 'org-bibtex-generate-org-entry)
  (evil-define-key 'normal org-mode-map (kbd "SPC") 'org-sort-by-year))

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
    ("8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538" "3018b767e4d5b0ddf37c909b2ea2a29c249d5a88645b4243d5b97fe2d67c71ab" "36c2b7efdc064944eb067e56c7ec65808a6cee0f63ce068b693fb30b110e57e5" "8ba0a9fc75f2e3b4c254183e814b8b7b8bcb1ad6ca049fde50e338e1c61a12a0" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "3dd98b7c43041526b35a4466c165d55400207ea9a82fd765c5c4bffffe1a7bd9" "2f524d307a2df470825718e27b8e3b81c0112dad112ad126805c043d7c1305c6" "3b2d3c38bac1160e32b8fb79ebc95e5464df4f6866fdb17c188b0adf4d02342a" "5c85b6f7f76fe0e0979da4c650dee525ae5185b134cb0fdfb12eeb580ea4dafb" "dc9a8d70c4f94a28aafc7833f8d05667601968e6c9bf998791c39fcb3e4679c9" "5a970147df34752ed45bfdf0729233abfc085d9673ae7e40210c5e2d8f624b08" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" default)))
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
    (jsx-mode arduino-mode go-mode cider clojure-mode evil-nerd-commenter js2-mode slime py-autopep8 dockerfile-mode docker all-the-icons ox-reveal reveal-ox ivy-bibtex irony-eldoc cmake-mode magit gruvbox-theme docker-tramp scad-mode plantuml-mode haskell-mode ein writeroom-mode matlab-mode cython-mode ggtags all-the-icons-dired dired-sidebar ob-blockdiag ob-ipython my-misc-config company-anaconda anaconda-mode overall_configurations overall-configurations org-element smartparens cdlatex hydra ess lua-mode yasnippet-snippets company-mode company-jedi markdown-mode yaml-mode paredit evil-magit company-irony irony traad pymacs projectile rebecca-theme solarized-theme org-pdfview highlight-symbol smex avy use-package)))
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
