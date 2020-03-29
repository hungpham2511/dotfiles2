(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)



(defvar test/hung-variable
  "Hung"
  "This is a fake variable."
  )


(defun test/a-function ()
  (message "test/a-function")
  50
  )

(defun test/b-function ()
  (message test/hung-variable)
  test/hung-variable
  )

(provide 'test-mod)
