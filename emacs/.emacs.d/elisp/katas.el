(require 's)

(defun kata/count-lines ()
  "Count the number of lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let '(no-lines 0)
      (while (not (eobp))
	(next-line)
	(setq no-lines (+ no-lines 1))
	)
      (message "%d" no-lines)
      no-lines
      )
    )
  )

(defun kata/open-other-windows ()
  "Open the current file in the other windows"
  (interactive)
  ;; (other-window 1)
  (select-window
   (next-window (selected-window)))
   (find-file "/home/hung/org/20200323231325_the_slipbox_and_getting_thing_done_gtd.org"))


(defun kata/zap-to-char (p c)
  "Kata the zap-to-char."
  (interactive "P\ncEnter a char: ")
  (kill-region (point)
	       (search-forward (char-to-string c))
	       )
  )


(defvar kata/black-executable
  "/home/hung/Envs/p3/bin/black"
  "The black executable link."
  )

(defvar kata/black-buffer-out
  "*black-output*"
  )


(defun kata/p-str (s)
  "Simple formatter."
  (message (format "%s" s)))


(defun kata/erase-output-buffer ()
  "Check and clear buffer."
  (if (get-buffer-create kata/black-buffer-out)
      (with-current-buffer kata/black-buffer-out
	(erase-buffer))))


(defun kata/blacken-run-proc ()
  "Run blacken, return error code."
  (equal
   (let ((process (make-process
		   :name "blaken"
		   :buffer kata/black-buffer-out
		   :stderr "*black-err*"
		   :command (list kata/black-executable "-")
		   :sentinel (lambda (p status) (message "Black terminates with status: %s" status))
		   )))
     (process-send-region process (point-min) (point-max))
     (process-send-eof process)
     (while (process-live-p process)
       (accept-process-output process nil nil t))
     (process-exit-status process))
   0))


(defun kata/copy-output-back-to-buffer ()
  "Copy output from output buffer back."
  (let ((p (point)))
    (erase-buffer)
    (insert-buffer-substring kata/black-buffer-out)
    (goto-char p)
    (save-buffer))
  )


(defun kata/blacken ()
  "Call black to work on the present buffer.

This function tries to format the current buffer using the black code formatter."
  (interactive)
  (kata/erase-output-buffer)
  (if (s-equals? major-mode "python-mode")
      (if (kata/blacken-run-proc)
	  (kata/copy-output-back-to-buffer)
	(error "Err: black fails"))
    (error "Err: Not a python buffer")))


(general-define-key
 :states 'motion
 ;; "M-t" 'kata/count-lines
 "M-t" 'kata/blacken)


(provide 'katas)
