;; pdf-tools for viewing pdf in emacs
(require 'general)

(use-package pdf-tools
  :straight t
  :bind (:map pdf-view-mode-map
	      ("k" . pdf-view-scroll-down-or-previous-page)
	      ("j" . pdf-view-scroll-up-or-next-page)
	      ("h" . image-backward-hscroll)
	      ("l" . image-forward-hscroll)
	      ("C-s" . isearch-forward))
  :config (pdf-tools-install))

(use-package tex
  :straight auctex
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
  :straight t)

;; Use for managing references. very useful
(use-package ivy-bibtex
  :straight t
  :commands 'ivy-bibtex
  :general ("C-c n p" 'ivy-bibtex)
  :config
  (progn
    (setq ivy-re-builders-alist
	  '((ivy-bibtex . ivy--regex-ignore-order)
	    (t . ivy--regex-plus)))
    (setq bibtex-completion-bibliography
	  '(
	    "~/Dropbox/BookandPaper/biblio/library.bib"
	    "~/Dropbox/BookandPaper/biblio/library_misc.bib"))
    (setq bibtex-completion-notes-path "~/org/readingnotes")
    (setq bibtex-completion-notes-template-multiple-files
	  "#+TITLE: (${year}): ${title} 
#+AUTHOR: ${author}
#+YEAR: ${year}
"
	  )
    ;; this allows bibtex-search to find tags in addition to other field
    (setq bibtex-completion-additional-search-fields '(tags journal))
    (setq bibtex-completion-library-path '("~/Dropbox/BookandPaper/Papers"))

    (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
      (let ((bibtex-completion-pdf-open-function
             (lambda (fpath) (start-process "xournalpp" "*ivy-bibtex-xournalpp*" "/usr/bin/xournalpp" fpath))))
        (bibtex-completion-open-pdf keys fallback-action)))

    (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)

    (ivy-add-actions
     'ivy-bibtex
     '(("p" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))

    )
  )

(provide 'config-latex)

