(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'lsp-mode)
(require 'org-roam)

(defvar test/hung-variable
  "Hung"
  "This is a fake variable.")

(defun test/a-function ()
  (message "test/a-function")
  50
  )

(defun test/b-function ()
  (message test/hung-variable)
  test/hung-variable)

; lsp mode based function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun h/clear-lsp-session ()
  "Clear all workspace folders from the current session."
  (interactive)
  (while (> (length (lsp-session-folders (lsp-session))) 1)
    (progn
      (lsp-workspace-folders-remove (car (lsp-session-folders (lsp-session))))
      (message(car (lsp-session-folders (lsp-session)))))))

(defun h/lsp-print-workspace-folders ()
  "Print to terminal the list of workspace folders."
  (interactive)
  (let ((x 0)
        (session-folders (lsp-session-folders (lsp-session))))
    (while (< x (length session-folders))
      (progn
        (message (format "WorkspaceFolder %d: %s" x (nth x session-folders)))
        (setq x (+ x 1))))))

(defun h/lsp-current-workspace ()
  (interactive)
  (let ((folders (lsp-session-server-id->folders (lsp-session))))
    (print (lsp-session))))

(defcustom h/jekyll-org-files nil
  "Set a list of org files that will be translated to jekyll blog post.")

;; TODO put this somewhere else
(setq h/jekyll-org-files
      '("~/org/20200403080059-transforming_note_to_blog_posts.org"
        "~/org/notebook-styled-pythonic.org"
        "~/org/markov-decision-process.org"
        ;; "~/org/projectile-for-uber-productive-project-wrangling.org"
        "~/org/discrete-time-control.org"
        "~/org/20200403075218-how_to_install_kernel_with_preemptrt.org"
        "~/org/moving-fast-to-contact.org"
        "~/org/docker-arm-support-with-buildx-and-simulator.org"
        ))

(defun h/jekyll-find-org-files ()
  "Find and open an org file registered to exported to jekyll."
  (interactive)
  (if (not h/jekyll-org-files)
      (message "Error: No org files defined")
    (prin1 h/jekyll-org-files))
  (let ((file (ivy-read "Open org/jekyll: " h/jekyll-org-files :require-match t)))
    (find-file file)))

(defun h/jekyll-build-all-posts()
  "Build all registered posts."
  (interactive)

  ;; https://emacs.stackexchange.com/questions/33915/problem-with-save-current-buffer-and-find-file
  (dolist (org-file h/jekyll-org-files)
    (with-current-buffer
        (find-file-noselect org-file)
      (org-hugo-export-to-md))))

(defun h/visit-init-el ()
  (interactive)
  (find-file "/home/hung/.emacs.d/init.el"))

(provide 'test-mod)
