;;; my-org-configs -- Summary
;;; Commentary:
; Simple snippets to use with init.el
(require 'org)
(require 'org-agenda)
(require 'org-bibtex)
(require 'counsel)

;;; Code:
(defun org-random-entry-TOREAD ()
  "Randomly select a paper to read."
  (interactive)
  (org-random-entry-match "+paper/TOREAD"))

(defun org-random-entry-TODO ()
  "Randomly select a TODO task."
  (interactive)
  (org-random-entry-match "-study/TODO|workingon|WaitingFor"))

(defun org-random-entry-STUDY ()
  "Randomly select something to study."
  (interactive)
  (org-random-entry-match "+study/TODO"))

(defun counsel-org-tag-2 ()
  "Add or remove tags in `org-mode'."
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags
                    (split-string (org-get-tags-string) ":" t)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (split-string (org-get-tags-string) ":" t)))
    (let ((org-setting-tags t)
	   (org-last-tags-completion-table org-current-tag-alist))
      (ivy-read (counsel-org-tag-prompt)
                (lambda (str &rest _unused)
                  (delete-dups
                   (all-completions str 'org-tags-completion-function)))
                :history 'org-tags-history
                :action 'counsel-org-tag-action
                :caller 'counsel-org-tag))))

(defun org-sort-by-year ()
  "Sort entries by year."
  (interactive)
  (org-sort-entries t ?r nil nil "YEAR")
  (outline-hide-leaves))

(defun org-bibtex-generate-org-entry ()
  "Generate an org entry from the bibtex entry at point."
  (interactive)
  (org-bibtex-read)
  (with-current-buffer (get-buffer-create "*BIB-ORG-output.org*")
    (org-mode)
    (set-auto-mode)
    (switch-to-buffer-other-window (current-buffer))
    (org-bibtex-write)
    (goto-char (point-max))
    (let ((heading (org-get-heading)))
      (org-edit-headline (bib-org-delimit-curly-bracket heading)))))


(defun bib-org-delimit-curly-bracket (string)
  "Simple function to remove curly brackets surround STRING."
  (mapconcat 'identity (delete "" (split-string string "[{} ]")) " "))

(defun org-export-latex-custom (&optional arg)
  "Export the sub-tree ARG levels up.

A custom routine to quickly export org subtree to pdf via latex.

By default, ARG is set to zero (export the current sub-tree).  If
ARG is n, export the n-th upper sub-tree counting the current
one."
  (interactive "P")
  (save-excursion
    (print arg)
    (if (not arg) (setq arg 0))
    (if (not (eq arg 0))
	(outline-up-heading arg))
    (let
	;; Set internal variable locally to disable confirmation, which
	;; is really cumbersome.
	((org-confirm-babel-evaluate nil))
      (org-latex-export-to-pdf nil t)
      (let*
	  ((org-buffer (buffer-name (current-buffer)))
	   (pdf-buffer (concat (car (split-string org-buffer "\\.")) ".pdf")))
	(if (get-buffer pdf-buffer)
	    (progn
	      (set-buffer pdf-buffer)
	      (pdf-view-revert-buffer t t)
	      (message (concat "PDF buffer: " pdf-buffer " reverted!")))
	  (progn
	    (set-buffer (find-file-noselect (concat "~/org/" pdf-buffer)))
	    (pdf-view-revert-buffer t t)))
	(switch-to-buffer-other-window pdf-buffer)
	(switch-to-buffer-other-window org-buffer)))))

(defun org-random-collect-entry-props ()
  "Collect all entries' properties."
  (interactive)
  (let* ((props (org-entry-properties))
	 (max-score (cond
		 ((string-equal (cdr (assoc "PRIORITY" props)) "A") 110)
		 ((string-equal (cdr (assoc "PRIORITY" props)) "B") 100)
		 ((string-equal (cdr (assoc "PRIORITY" props)) "C") 90)))
	 (score (random max-score)))
    (message (concat "New item: " (cdr (assoc "ITEM" props))))
    (push (cons "SCORE"  score) props)))


(defun org-random-entry-match (match)
  "Jump randomly to an entry based on MATCH specification.

The MATCH specification consists of two parts:

TAGS and PROPERTY / TODO.

Example:

+refile/TODO          =>  Match all headings tagged with :refile: and has
                          state TODO state TODO.
+refile-paper/TODO     => Match all headings tagged with :refile:, not :paper: and has
		          state TODO state TODO."
  (interactive "sEnter MATCH string: ")
  (let* (
	 (org-agenda-files-custom '("/home/hung/org/research.org"
				    "/home/hung/org/refile.org"
				    "/home/hung/org/study.org"
				    "/home/hung/org/papers.org"
				    "/home/hung/org/people.org"
				    "/home/hung/org/test.org"))
	 (collected-props
	  (org-map-entries 'org-random-collect-entry-props match org-agenda-files-custom))
	 (i 0)
	 (i-max 0)
	 (score-max -10))

    (while (< i (safe-length collected-props))
      (if (>  (cdr (assoc "SCORE" (nth i collected-props))) score-max)
	  (progn
	    (setq i-max i)
	    (setq score-max (cdr (assoc "SCORE" (nth i collected-props))))))
      (setq i (+ i 1)))
    (print (concat "Found " (number-to-string (safe-length collected-props)) " items!"))
    (let ((selected-entry (nth i-max collected-props)))
      (progn
	(print (concat "Select => " (cdr (assoc "ITEM" selected-entry))))
	(find-file (cdr (assoc "FILE" selected-entry)))
	(goto-char (point-min))
	(goto-char (re-search-forward
		    (concat "^[\*].*" (cdr (assoc "ITEM" selected-entry)))))
	;; (print selected-entry)
	(beginning-of-line)
	(org-reveal)))))


(provide 'my-org-configs)
