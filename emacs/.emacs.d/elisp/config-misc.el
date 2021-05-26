;;; Misc functions and snippets

;;; Code:
(defun hello()
  "Hello World function"
  (interactive)
  (message "Hello World! I am Hung's first elisp function!"))

(defun find-dired-dropbox-iname (args)
  "Var ARGS."
  (interactive "sEnter Search Entry: ")
  (find-dired "~/Dropbox/" (concat "-iname " "'*" args "*'")))

(defun h/show-backends ()
  (interactive)
  (prin1 company-backends))

(defun h/company-set-python-backends ()
  (interactive)
  (setq-local company-backends '(company-dabbrev-code)))

(defun h/set-writing-completion-backends ()
  (interactive)
  (setq company-backends '(company-ispell  (company-dabbrev company-yasnippet company-files))))

(defun occur-definitions-use-package ()
  "Display an occur buffer of all definitions in the current buffer.

  Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^(\\(general\\|defun\\|use-package\\|require\\)\\|^;;;"))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
	(select-window window)
      (switch-to-buffer "*Occur*"))))

;; Keymaps
(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :state '(motion visual normal insert)
 "C-c C-o" 'occur-definitions-use-package)

(provide 'config-misc)
;;; Module ends
