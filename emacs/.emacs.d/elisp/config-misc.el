;;; Commentary:
; Simple snippets to use with init.el

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
  (setq-local company-backends '(company-dabbrev-code))
  )

(provide 'config-misc)
;;; hello.el ends here
