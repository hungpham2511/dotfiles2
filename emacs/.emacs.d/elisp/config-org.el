(use-package org-habit
  :after org
  :config
  (setq org-habit-following-days 3)
  (setq org-habit-preceding-days 7)
  (setq org-habit-graph-column 70)
  )

(use-package org-pdfview
  :after org
  :ensure t)


(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c x" . org-capture)
	 ("C-c C-x C-j" . org-clock-goto)
	 :map org-mode-map
	 ("C-c c" . org-export-latex-custom)
	 ("C-c C-p" . org-previous-visible-heading)
	 :map org-agenda-mode-map
	 ("j" . org-agenda-next-item)
	 ("k" . org-agenda-previous-item)
	 )
  :config
  (progn
    (unbind-key "C-c C-p" org-mode-map)
    (unbind-key "C-c C-p" org-agenda-mode-map)

    (add-hook 'org-agenda-mode-hook (lambda ()
				      (message "org-mode hook")
				      ))

    ;; start org-agenda in another window, i.e. do not destroy my window layout
    ;; https://stackoverflow.com/questions/10635989/emacs-org-agenda-list-destroy-my-windows-splits
    (setq org-agenda-window-setup 'other-window)

    ;; setup image
    (setq org-image-actual-width 600)
    (setq org-agenda-search-view-always-boolean t)
   
    ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")
    (setcdr (assoc "\\.x?html?\\'" org-file-apps) "google-chrome %s")
    (setcdr (assoc "\\.pdf\\'" org-file-apps) (lambda (file link) (org-pdfview-open link)))

    (setq org-modules (cons 'org-habit org-modules))
    (setq org-tags-exclude-from-inheritance '("project"))

    ;; org-agenda [important]
    (setq org-agenda-custom-commands
	  '(
	    ("j" "(new) work agenda"
	     ((tags "+project/-DONE-someday-probnever")
	      (stuck "")
	      (agenda "" )
	      (tags "+inprogress/TODO")
	      (tags "-backlog-habit-inprogress/TODO"
			 ((org-agenda-sorting-strategy
			   '((tags priority-down category-up )))))
	      (tags "+backlog+TODO=\"TODO\"|+refile")
	      (todo "WaitingFor")
	      )

	     ;; Custom setting for this command
	     ((org-agenda-span 'day)
	      (org-agenda-files '("~/org/research.org"
				  "~/org/refile.org"
				  "~/org/study.org"
				  "~/org/papers.org"
				  "~/org/refile-phone.org"
				  "~/org/people.org"
				  "~/org/test.org"))
	      (org-agenda-sorting-strategy
	      '(
		(agenda habit-down time-up priority-down category-down)
	     	(todo habit-up category-up priority-down tag-down)
	     	(tags effort-up priority-down category-up)
	     	;; (search category-keep)
		)))
	     ("~/Dropbox/agenda.html"))
	    ("g" "test agenda"
	     ((tags "+refile|+backlog+TODO=\"TODO\""))
	     )
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
	      (org-agenda-files '("~/org/research.org"
				  "~/org/refile.org"
				  "~/org/study.org"
				  "~/org/people.org"
				  "~/org/test.org"))
	     (org-agenda-sorting-strategy
	      '((agenda habit-down time-up priority-down category-down)
		(todo category-up priority-down tag-down)
		;; (tags priority-down todo-state-up category-down)
		(tags category-keep priority-down todo-state-up )
		(search category-keep))))
	     )
	    ("p" "paper view" ;; Description
	     ;; Commands
	     ((todo "TOREAD|READING")
	      (todo "someday"))
	     ;; Custom setting
	     ((org-agenda-span 'day)
	      (org-agenda-files '("~/org/papers.org"))
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
		   "* TODO %? :backlog:\n%U\n")
		  ("n" "note" entry (file "~/org/refile.org")
		   "* %? :NOTE:\n%U\n%a\n")
		  ("j" "Journal" entry (file+datetree "~/org/journal.org")
		   "* %?\n%U\n")
		  ("p" "New paper" plain (file "~/Dropbox/BookandPaper/biblio/library.bib") "@comment Entry added with org-capture\n\n%?")

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

    (setq org-agenda-files (quote ("~/org/papers.org"
				   "~/org/research.org"
				   "~/org/study.org"
				   "~/org/test.org"
				   "~/org/refile.org"
				   "~/org/refile-phone.org")))

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

(provide 'config-org)
