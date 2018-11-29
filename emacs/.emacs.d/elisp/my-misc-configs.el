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

(provide 'my-misc-configs)
;;; hello.el ends here
