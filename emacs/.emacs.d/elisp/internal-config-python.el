;;; python-configs -- Summary
;;; Commentary:
; Simple snippets to use with init.el
;;; Code:

;; (use-package lsp-pyright
;;   :demand
;;   :straight t
;;   ;; :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;   )

(use-package lsp-python-ms
  :straight t
  :demand
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred)
                         )))

(use-package python-utils
  :after projectile
  :load-path "python-utils.el"
  :demand)

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
	    (setq jedi:complete-on-dot t))
  :defer)

;; (condition-case nil
;;     (progn
;;       (setq venv-location "~/Envs/")
;;       (venv-workon "ros"))
;;   (error nil))

(use-package cython-mode
  :straight t)

;; jedi configuration, main use for python document lookup. work quite
;; well in general
(defvar jedi:server-args-with-ros)
(setq jedi:server-args-with-ros
      '(;; ros packages
	;; "--sys-path" "/home/hung/catkin_ws/install/lib/python2.7/dist-packages"
	"--sys-path" "/opt/ros/kinetic/lib/python2.7/dist-packages"))

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
   (list (completing-read "Select env: " (list-virtualenvs))))
  (message (concat "Setting jedi with " venv-name))
  (setq jedi:server-args
	(append jedi:server-args-with-ros
		(list "--virtual-env" (concat "~/Envs/" venv-name))))
  (jedi:stop-server))


;; very useful function.
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
   unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
	(set-buffer (get-buffer "*Occur*"))
	(goto-char (point-min))
	(toggle-read-only 0)
	(if (looking-at "^[0-9]+ lines matching \"")
	    (kill-line 1))
	(while (re-search-forward "^[ \t]*[0-9]+:" (point-max) t)
	  (replace-match "")
	  (forward-line 1))
	(goto-char (point-min))
	(while (re-search-forward "^# " (point-max) t)
	  (replace-match "")
	  (forward-line 1))
	(toggle-read-only 1))

    (message "There is no buffer named \"*Occur*\".")))

(defun python-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.

  Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^ *\\(def\\|class\\|cdef\\|cpdef\\) \\|# #"))
  (occur-mode-clean-buffer)
  (let ((window (get-buffer-window "*Occur*")))
    (if window
	(select-window window)
      (switch-to-buffer "*Occur*"))))


(defun python--list-defs-in-buffer ()
  "List definitions found in current buffer."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "def" nil t)  ;; Search forward for some regular expression
      (message (thing-at-point 'line))
    (message "do not found any def")))

(defun python-ivy-definitions ()
  ""
  (interactive)
  (ivy-read "python-ivy: "
	    (list "1" "2" "3")
            :keymap python-mode-map
            :preselect (ivy-thing-at-point)
            :history 'counsel-describe-symbol-history
            :require-match t
            :sort t
            :caller 'python-ivy-definitions))


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

(provide 'internal-config-python)
;;; python-configs.el ends here
