;;; python-configs -- Summary
;;; Commentary:
; Simple snippets to use with init.el
(require 'jedi-core)

;;; Code:

;; For some reasons I need to have an active python environment,
;; otherwise emacs will complain. Strange, need to investigate this
;; problem.


;; emacs company jedi work with ros
(use-package company-jedi
  :after company
  :load-path "~/.emacs.d/git/emacs-company-jedi"
  :config (progn
	    (defun my/python-mode-hook ()
	      (add-to-list 'company-backends 'company-jedi))
	    (add-hook 'python-mode-hook 'my/python-mode-hook)
	    (add-hook 'python-mode-hook 'jedi:setup)
	    (setq jedi:complete-on-dot t)))

(condition-case nil
    (progn
      (setq venv-location "~/Envs/")
      (venv-workon "ros"))
  (error nil))

(use-package cython-mode
  :ensure t)

;; jedi configuration, main use for python document lookup. work quite
;; well in general
(defvar jedi:server-args-with-ros)
(setq jedi:server-args-with-ros
      '(;; ros packages
	;; "--sys-path" "/home/hung/catkin_ws/install/lib/python2.7/dist-packages"
	"--sys-path" "/opt/ros/kinetic/lib/python2.7/dist-packages"
	))

(defun jedi:add-sys-path (dir)
  "Add DIR to jedi:server-args.
This is needed for auto-completion of packages in DIR."
  (interactive "fDirectory: ")
  (setq jedi:server-args-with-ros
	(append jedi:server-args-with-ros
		(list "--sys-path" dir))))

(defun list-virtualenvs ()
  "List all virtual env in ~/Envs/."
  (let ((all-file (directory-files "~/Envs/")))
    (cl-remove-if-not
     (lambda (file) ""
       ;; Check if FILE is a virtual environment.
       (file-exists-p (concat "~/Envs/" file "/bin/python")))
     all-file)))


(defun jedi:workon (venv-name)
  "Select virtual environment VENV-NAME and load it.

Append VENV-NAME to jedi:server args, stop the server
then (restart when needed).  In interactive mode ask for
VENV-NAME then select it."
  (interactive
   (list (completing-read "Select env: " (list-virtualenvs)))
   )
  (message (concat "Setting jedi with " venv-name))
  (setq jedi:server-args
	(append jedi:server-args-with-ros
		(list "--virtual-env" (concat "~/Envs/" venv-name))))
  (jedi:stop-server))


;; very useful function.
(defun python-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.

Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^ *\\(def\\|class\\|cdef\\|cpdef\\) "))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

(defun my/python-arg-numpy-docstring (arg)
  "Given an argument ARG.  Output a formatted docstring."
  (let ((arg-splitted-w/= (split-string arg "=")))
    (if (nth 1 arg-splitted-w/=)
	(concat "    " (nth 0 arg-splitted-w/=) " : , optional\n        FIXME\n")
      (concat "    " (nth 0 arg-splitted-w/=) " : \n        FIXME\n"))))

(defun my/python-insert-function-docstring-numpy ()
  "Insert a function docstring according to Numpy style."
  (interactive)
  (let* ((my-string (thing-at-point 'line t))
	 (my-regex "def.*(\\(.*\\)):")
	 (sep-args (and (string-match my-regex my-string)
		       (let ((all-args (match-string 1 my-string)))
			 (split-string all-args "[, ]+" )))))
    (forward-line 1)
    (insert "    \"\"\"\n    FIXME\n\n")
    (insert "    Parameters\n    ----------\n")
    (insert (string-join
	     (mapcar 'my/python-arg-numpy-docstring
		     sep-args)))
    (insert "\n    Returns\n    -------\n")
    (insert "    out :\n        FIXME\n")
    (insert "    \"\"\"\n")))


(defun my/python-arg-google-docstring (arg)
  "Given an argument ARG.  Output a formatted docstring."
  (let ((arg-splitted-w/= (split-string arg "=")))
    (if (nth 1 arg-splitted-w/=)
	(concat "        " (nth 0 arg-splitted-w/=) " (FIXME, optional): FIXME\n")
      (concat "        " (nth 0 arg-splitted-w/=) " (FIXME): FIXME\n"))))

(defun my/python-insert-function-docstring-google ()
  "Insert a function docstring according to Google style."
  (interactive)
  (let* ((my-string (thing-at-point 'line t))
	 (my-regex "def.*(\\(.*\\)):")
	 (sep-args (and (string-match my-regex my-string)
		       (let ((all-args (match-string 1 my-string)))
			 (split-string all-args "[, ]+" )))))
    (forward-line 1)
    (insert "    \"\"\" FIXME\n\n")
    (insert "    Args:\n")
    (insert (string-join
	     (mapcar 'my/python-arg-google-docstring
		     sep-args)))
    (insert "\n    Returns:\n")
    (insert "        out (FIXME): FIXME\n")
    (insert "    \"\"\"\n")))

(add-hook 'python-mode-hook
	      (lambda ()
		(define-key python-mode-map (kbd "C-c C-o") 'python-occur-definitions)))

(provide 'python-configs)
;;; python-configs.el ends here
