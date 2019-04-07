;;; Commentary:
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


;; setup stage 1: load the EVIL and its friends
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("C-f" . evil-avy-goto-char))
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
  (define-key evil-normal-state-map (kbd "C-i") 'scroll-up-half-page)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-down-half-page)
  (define-key global-map (kbd "C-i") 'scroll-up-half-page)
  (define-key global-map (kbd "C-d") 'scroll-down-half-page)

  ;; move-through windows
  (define-key evil-normal-state-map (kbd "C-j")
    (lambda () (interactive) (other-window 1)))
  (define-key global-map (kbd "C-j")
    (lambda () (interactive) (other-window 1)))

  (define-key evil-normal-state-map (kbd "C-k")
    (lambda () (interactive) (other-window -1)))
  (define-key global-map (kbd "C-k")
    (lambda () (interactive) (other-window -1)))

  ;; move-in occur mode
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-i")     'evil-scroll-up))

(use-package evil-nerd-commenter
  :ensure t
  :config
  ;; Evilnc keys must set up before org-capture
  (evilnc-default-hotkeys))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; To surround a code segment with $, hightlight it then press `csm`.
  (add-hook 'org-mode-hook
	    (lambda () (push '(?m . ("$" . "$")) evil-surround-pairs-alist))))

;; setup stage 2: misc configurations, not important but useful
(require 'overall-configs nil t)

(use-package haskell-mode ;; haskell-mode, isn't it very obvious
  :ensure t)

(use-package scad-mode  ;; minor-mode for openscad file edit
  :ensure t)

(use-package avy  ;; Very cool for jumping around.
  :ensure t
  :bind
  (("C-'" . avy-goto-char)))

(use-package counsel
  :ensure t
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("<f1>" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-ag)))

(use-package ivy
  :diminish t
  :ensure t
  :bind
  (("C-x C-c" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  (:map ivy-mode-map
	("C-'" . ivy-avy)
	)
  :config
  (progn
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
        '((t   . ivy--regex-plus)))))


;; setup stage 3: python configuration
(use-package jedi-core
  :ensure t)
(require 'python-configs nil t)  ;; require is similar to include.

(use-package ox-reveal
  :ensure t
  :config

  (setq org-reveal-root "/home/hung/Downloads/reveal.js-3.7.0")

  )

;; setup stage 4: configure org setup
(use-package my-org-configs
  :load-path "~/.emacs.d/elisp"
  :demand
  :config
  (evil-define-key 'normal bibtex-mode-map (kbd "SPC") 'org-bibtex-generate-org-entry)
  (evil-define-key 'normal org-mode-map (kbd "SPC") 'org-sort-by-year))

;; My personal library, some snippets thats it
(use-package my-misc-configs
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

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

;; for some reason this package is needed to run highlight symbol
(use-package solarized-theme
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t)

;; pdf-tools for viewing pdf in emacs
(use-package pdf-tools
  :ensure t
  :bind (:map pdf-view-mode-map
	      ("k" . pdf-view-scroll-down-or-previous-page)
	      ("j" . pdf-view-scroll-up-or-next-page)
	      ("h" . image-backward-hscroll)
	      ("l" . image-forward-hscroll)
	      ("C-s" . isearch-forward))
  :config
  (pdf-tools-install))

(use-package  highlight-symbol
  :ensure t
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)
	 ))

(use-package tex
  :ensure auctex
  :config
  (progn
    (setq TeX-auto-save t)
    (setq-default TeX-master nil)
    (setq TeX-PDF-mode t)

    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'cdlatex-mode)

    ;; This is to allow correct render of ASCII drawing from
    ;; org-mode-latex-export.
    (setq TeX-auto-untabify 't)

    ;; LaTeX-mode-map is only defined after LaTeX-mode is called the
    ;; first time. Thus defining the key directly does not work. The
    ;; below code add a hook that define the key after LaTeX-mode is
    ;; enable instead.
    (add-hook 'LaTeX-mode-hook
	      (lambda () (define-key LaTeX-mode-map (kbd "C-c c") 'TeX-command-run-all)))

    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
	      #'TeX-revert-document-buffer)

    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    ;; Latex forward/inverse search
    (add-hook 'LaTeX-mode-hook
	      (lambda ()
		(TeX-source-correlate-mode t)
		(setq TeX-source-correlate-start-server t)
		(setq TeX-source-correlate-method 'synctex)
		(setq TeX-view-program-selection
		      (quote ((output-pdf "PDF Tools")))) ) ) ))

(use-package reftex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :ensure t)

(use-package ag
  :ensure t
  :defer t
  :config (setq ag-highlight-search t)
  :bind ("M-s 0" . ag)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))))

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-global-mode 1))

(use-package ivy-bibtex
  :ensure t
  :bind (("<f2>" . ivy-bibtex))
  :config
  (progn
    (setq ivy-re-builders-alist
	  '((ivy-bibtex . ivy--regex-ignore-order)
	    (t . ivy--regex-plus)))
    (setq bibtex-completion-bibliography
	  '(
	    "~/Dropbox/BookandPaper/biblio/library.bib"
	    "~/Dropbox/BookandPaper/biblio/library_misc.bib"
	    ))

    (setq bibtex-completion-notes-path "~/org/papers.org")

    ;; this allows bibtex-search to find tags in addition to other field
    (setq bibtex-completion-additional-search-fields '(tags journal))

    (setq bibtex-completion-library-path '("~/Dropbox/BookandPaper/Papers"))
    )
  )

(use-package org-habit)

(use-package org-pdfview
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c x" . org-capture)
	 ("C-c C-x C-j" . org-clock-goto)
	 :map org-mode-map
	 ("C-c c" . org-export-latex-custom))
  :config
  (progn
    (message "Start configure org-mode")
    (setq org-image-actual-width 600)
    (setq org-agenda-search-view-always-boolean t)
    (setq org-agenda-files (quote ("/home/hung/org/papers.org"
				   "/home/hung/org/research.org"
				   "/home/hung/org/study.org"
				   "/home/hung/org/play.org"
				   "/home/hung/org/test.org"
				   "/home/hung/org/refile.org")))
    ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")
    (setcdr (assoc "\\.x?html?\\'" org-file-apps) "google-chrome %s")
    (setcdr (assoc "\\.pdf\\'" org-file-apps) (lambda (file link) (org-pdfview-open link)))

    (setq org-modules (cons 'org-habit org-modules))
    (setq org-tags-exclude-from-inheritance '("project"))

    ;; org-agenda [important]
    (setq org-agenda-custom-commands
	  '(
	    ("j" "(new) work agenda"
	     ((tags "+project/-DONE-someday-probnever") ;; Select entries with :project: tag and not :DONE: tag
	      (stuck "") ;; Run stuck command to see org stuck project
	      (agenda "" )	      ;; Run agenda
	      (tags-todo "+PRIORITY=\"A\"/TODO")  ;; syntax for mathcing PROPERTY="val"/TODO_keywords
	      (todo "TODO")
	      (todo "WaitingFor")
	      )
	     ;; Custom setting for this command
	     ((org-agenda-span 'week)
	      (org-agenda-files '("/home/hung/org/research.org"
				  "/home/hung/org/refile.org"
				  "/home/hung/org/study.org"
				  "/home/hung/org/papers.org"
				  "/home/hung/org/people.org"
				  "/home/hung/org/test.org"))
	      (org-agenda-sorting-strategy
	      '(
		(agenda habit-down time-up priority-down category-down)
	     	(todo habit-up category-up priority-down tag-down)
	     	(tags effort-up category-up priority-down)
	     	;; (search category-keep)
		))))
	    ("n" "work agenda" ;; Description
	     ;; Commands
	     ((tags "+project/-DONE-someday-probnever") ;; Select entries with :project: tag and not :DONE: tag
	      (stuck "") ;; Run stuck command to see org stuck project
	      (agenda "" )	      ;; Run agenda
	      (tags "-refile-study/TODO|workingon") ;; Select TODO/Working On to display
	      (tags "-refile-study/WaitingFor") ;; Select TODO/Working On to display
	      (tags "+study/TODO|workingon")
	      (tags "+refile/TODO")
	      (todo "someday"))
	     ;; Custom setting for this command
	     ((org-agenda-span 'day)
	      (org-agenda-files '("/home/hung/org/research.org"
				  "/home/hung/org/refile.org"
				  "/home/hung/org/study.org"
				  "/home/hung/org/people.org"
				  "/home/hung/org/test.org")))
	     (org-agenda-sorting-strategy
	      '((agenda habit-down time-up priority-down category-down)
		(todo category-up priority-down tag-down)
		;; (tags priority-down todo-state-up category-down)
		(tags category-keep priority-down todo-state-up )
		(search category-keep)))
	     )
	    ("p" "paper view" ;; Description
	     ;; Commands
	     ((todo "TOREAD|READING")
	      (todo "someday"))
	     ;; Custom setting
	     ((org-agenda-span 'day)
	      (org-agenda-files '("/home/hung/org/papers.org"))
	      (org-agenda-sorting-strategy
	       '((agenda habit-down time-up priority-down category-down)
		 (todo priority-down category-up tag-down)
		 (tags priority-down todo-state-up category-down)
		 (search category-keep)))))))

    ;; The below code does not work, all project with SCHEDULED is
    ;; indentified as non-stuck even after I have marked DONE.
    ;; (setq org-stuck-projects
    ;;       '("+project/-someday-DONE" ("workingon" "WaitingFor" "TODO" "pTODO")
    ;;       nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-stuck-projects
	  '("+project/-someday-DONE-probnever" ("workingon" "WaitingFor" "TODO" "pTODO")
	    nil ""))

    ;; org-capture
    ;; TODO: Do I need all these templates?
    (setq org-capture-templates
	  (quote (("t" "todo" entry (file "~/org/refile.org")
		   "* TODO %?\n%U\n%a\n")
		  ("n" "note" entry (file "~/org/refile.org")
		   "* %? :NOTE:\n%U\n%a\n")
		  ("j" "Journal" entry (file+datetree "~/org/journal.org")
		   "* %?\n%U\n")
		  ("m" "Meeting" entry (file "~/org/refile.org")
		   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		  ("a" "Mail to" entry (file "~/org/refile.org")
		   "* TODO EMAIL to %? :EMAIL:\n%U" :clock-in t :clock-resume t)
		  ("h" "Habit" entry (file "~/org/refile.org")
		   "* NEXT %?\n%U\n%a\nSCHEDULED:
	       %(format-time-string \"%<<%Y-%m-%d %a
	       .+1d/3d>>\")\n:PROPERTIES:\n:STYLE:
	       habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
    ;; org-capture fin

    ;; org-mode configuration
    (setq org-deadline-warning-days 14)
    (setq org-clock-into-drawer t)
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
    (setq org-default-notes-file "~/org/refile.org")
    (setq org-agenda-files '("/home/hung/org/research.org"
			     "/home/hung/org/refile.org"
			     "/home/hung/org/study.org"
			     "/home/hung/org/people.org"
			     "/home/hung/org/test.org"))

    ;;if the variable org-export-use-babel is nil, header settings
    ;;will not be considered. Thus this variable need to be True.
    (setq org-export-use-babel t)
    ;; The above setting is commented as it causes some confusion in
    ;; the exporting process.
    (setq org-highlight-latex-and-related '(latex script entities))
    (setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

    ;;; display/update images in the buffer after I evaluate
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (latex . t)
       (plantuml . t)))

    (add-to-list
     'org-src-lang-modes '("plantuml" . plantuml))
    (setq org-src-fontify-natively t)))

(require 'ob-latex)
(use-package ox-latex
  :config
  (progn
    (setq org-latex-pdf-process (list "latexmk -pdf %f"))
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    (add-to-list 'org-latex-classes
		 '("ieeetran"
		   "\\documentclass{IEEEtran}
\\usepackage{bm}
\\usepackage{mathrsfs}
\\usepackage{siunitx}
\\renewcommand{\\vec}[1]{\\bm{\\mathrm{#1}}}
\\usepackage[ruled, linesnumbered]{algorithm2e}

\\SetKwIF{If}{ElseIf}{Else}{if}{ then}{elif}{else}{}%
\\SetKwFor{For}{for}{ do}{}%
\\SetKwFor{ForEach}{foreach}{ do}{}%
\\SetKwInOut{Input}{Input}%
\\SetKwInOut{Output}{Output}%
\\AlgoDontDisplayBlockMarkers%
\\SetAlgoNoEnd%
\\SetAlgoNoLine%
\\DontPrintSemicolon

\\usepackage{amsthm}

\\theoremstyle{plain}
\\newtheorem{theorem}{Theorem}
\\newtheorem{lem}{Lemma}
\\newtheorem{proposition}{Proposition}

"

		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

    (add-to-list 'org-latex-classes
		 '("koma-article"
		   "\\documentclass[draft=false, parskip=half, toc=selection, BCOR=8.25mm, DIV=15]{scrartcl}
\\usepackage{bm}
\\usepackage{mathrsfs}
\\usepackage{siunitx}
\\usepackage{graphicx} 
\\usepackage{tikz}
\\renewcommand{\\vec}[1]{\\bm{\\mathrm{#1}}}"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Completion package
(use-package company
  :ensure t
  :bind (
	 ("C-M-i" . company-complete)
	 :map python-mode-map
	 ("C-M-i" . company-complete)
	 :map emacs-lisp-mode-map
	 ("C-M-i" . company-complete)
	 )
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; ;; I no longer use emacs for C/C++ development. I use a IDE for this task.

;; ;; Setup for auto complete functionality in C/C++
;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;     (lambda ()
;;        (add-to-list 'company-backends 'company-irony)
;;        (setq company-backends (delete 'company-clang company-backends)))
;;     ))

;; (use-package irony
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'objc-mode-hook 'irony-mode)
;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;;   )

;; (use-package irony-eldoc
;;   :ensure t
;;   :config
;;    (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'org-mode "$" "$")
    (eval-after-load 'org-mode     '(require 'smartparens-org))
    (eval-after-load 'python-mode   '(require 'smartparens-python))
    (show-smartparens-global-mode)
    (smartparens-global-mode)
    (set-face-foreground 'sp-show-pair-match-face "#8ec07c")))

(use-package projectile
  :init (global-unset-key (kbd "C-c p"))
  :bind ("C-c p p" . projectile-switch-project)
  ;; :ensure t
  :config (progn
	    (projectile-mode t)
	    (setq projectile-enable-caching t)
	    (setq projectile-completion-system 'ivy)))

(use-package key-chord
  :ensure t
  :after evil
  :config (progn
	    (key-chord-mode t)
	    (setq key-chord-two-keys-delay 0.1)
	    (key-chord-define evil-insert-state-map "jk" 'evil-force-normal-state)))

(use-package magit
  :ensure t
  :bind ("C-x C-z" . magit-status))

(use-package evil-magit
  :ensure t
  :after magit evil)

;; emacs company jedi work with ros
(use-package company-jedi
  :load-path "/home/hung/.emacs.d/git/emacs-company-jedi"
  :config (progn
	    (defun my/python-mode-hook ()
	      (add-to-list 'company-backends 'company-jedi))
	    (add-hook 'python-mode-hook 'my/python-mode-hook)
	    (add-hook 'python-mode-hook 'jedi:setup)
	    (setq jedi:complete-on-dot t)))

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
    (ox-reveal reveal-ox ivy-bibtex irony-eldoc cmake-mode magit gruvbox-theme docker-tramp scad-mode plantuml-mode haskell-mode ein writeroom-mode matlab-mode cython-mode ggtags all-the-icons-dired dired-sidebar ob-blockdiag ob-ipython my-misc-config company-anaconda anaconda-mode overall_configurations overall-configurations org-element smartparens cdlatex hydra ess lua-mode yasnippet-snippets company-mode company-jedi markdown-mode yaml-mode paredit evil-magit company-irony irony traad pymacs projectile rebecca-theme solarized-theme org-pdfview highlight-symbol smex avy use-package)))
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
