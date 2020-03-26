(use-package org-habit
  :after org
  :config
  (setq org-habit-following-days 3)
  (setq org-habit-preceding-days 7)
  (setq org-habit-graph-column 70))


(use-package org-pdfview
  :after org
  :straight t)


(use-package org
  :straight org-plus-contrib
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
    (unbind-key "C-j" org-mode-map)
    (unbind-key "C-," org-mode-map)

    
    (setq org-startup-folded nil)
    (setq org-startup-indented t)

    ;; Configure org-babel to generate the correct stuffs.
    (setq org-structure-template-alist `(("s" . "src")))

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
	      (org-agenda-files '("~/org"))
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
    (setq org-agenda-files '("~/org/"))

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
    (setq org-src-fontify-natively t))

  ;; This line is mysteriously needed to get rid of this error:
  ;; Error running timer ‘org-indent-initialize-agent’: (void-function org-time-add)
  (org-reload))

(use-package org-roam
      :after org
      :hook 
      ((org-mode . org-roam-mode)
       (after-init . org-roam--build-cache-async) ;; optional!
       )
      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
      :custom
      (org-roam-directory "/home/hung/org/")
      :bind
      ("C-c n l" . org-roam)
      ("C-c n t" . org-roam-today)
      ("C-c n f" . org-roam-find-file)
      ("C-c n i" . org-roam-insert)
      ("C-c n g" . org-roam-show-graph))


(use-package deft
  :after org
  :straight t
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org")
  :config
  (setq deft-file-naming-rules
	'((noslash . "-")
	  (nospace . "-")
	  (case-fn . downcase)))
  )

(provide 'config-org)
