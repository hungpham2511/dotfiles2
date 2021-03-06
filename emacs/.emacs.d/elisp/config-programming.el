; This file contains my setup for the programming part.
(require 'general)
(require 'google-c-style)

(defvar h/python-virtual-env "/home/hung/Envs/p37/bin/python"
  "a var")


;; finish setting up rust for emacs
(use-package flycheck
  :straight t
  :demand
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq flycheck-python-flake8-executable h/python-virtual-env)
  (setq flycheck-flake8rc "/home/hung/dotfiles2/.flake8rc"))

(use-package yasnippet-snippets
  :straight t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
	'("/home/hung/.emacs.d/etc/yasnippet/snippets/"
	  yasnippet-snippets-dir
	  "/home/hung/.emacs.d/snippets"))
  (yas-reload-all))

(use-package  highlight-symbol
  :straight t
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)))

;; Configure company-mode for auto completion.
;;
(use-package company
  :straight t
  :demand
  :commands global-company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)

  ;; This variable, if set to t or "all", generate alots of completion
  ;; candidates. This uses up alots of memory.
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-time-limit 0.05)

  (general-define-key
   :keymaps 'company-active-map
   "C-o" 'company-show-location
   "C-s" 'company-search-candidates
   "M-n" 'company-select-next
   "M-p" 'company-select-previous))

;; Configure company-quickhelp to view the documentation of current
;; completion candidates. After installing a doc-buffer will show up
;; for the current highlighted candidate.  Homepage
;; https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
  :straight t
  :ensure
  :config
  (company-quickhelp-mode))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode t))

(use-package smartparens
  :straight t
  :after general
  :commands smartparens-global-mode
  
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "$" "$")
  (eval-after-load 'org-mode     '(require 'smartparens-org))
  (eval-after-load 'python-mode   '(require 'smartparens-python))
  (show-smartparens-global-mode)
  (smartparens-global-mode)
  (set-face-foreground 'sp-show-pair-match-face "#8ec07c")

  ;; Most important commands. Move within the sexp.
  (general-define-key
   :states '(motion visual insert)
   
   "C-M-a" 'sp-beginning-of-sexp
   "C-M-e" 'sp-end-of-sexp
   "C-M-f" 'sp-forward-sexp
   "C-M-b" 'sp-backward-sexp

   ;; Move between sexp of the same level then move between sexp
   ;; levels. These are very useful shortcuts.
   "M-l" 'sp-next-sexp
   "M-h" 'sp-backward-sexp
   "M-j" 'sp-down-sexp
   "M-k" 'sp-backward-up-sexp
   
   ;; move between symbols
   "M-f" 'sp-forward-symbol
   "M-b" 'sp-backward-symbol
   
   "C-c f s" 'sp-forward-slurp-sexp
   "C-c f b" 'sp-forward-barf-sexp
   ))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode t))

(use-package lsp-mode
  :hook (c++-mode . lsp)
  :straight t
  :commands lsp
  :config
  (setq gc-cons-threshold 500000000) ;; 500mb
  (setq read-process-output-max (* 8 1024 1024)) ;; 8mb
  (setq lsp-completion-provider nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-ui-sideline-enable t)

  (add-hook 'go-mode-hook #'lsp-deferred)
  
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; ;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t)


(message "Configuring c++ code-style")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Setup company mode integration fd
(defun company-emacs-lisp ()
  "Setup completion for emacs lisp "
  (message "Setup company mode for emacs-lisp")
  (setq-local company-backends '((company-capf company-files) company-ispell)))
(add-hook 'emacs-lisp-mode-hook 'company-emacs-lisp)

(use-package protobuf-mode
  :ensure t
  :straight t)

(use-package plantuml-mode
  :ensure t
  :straight t
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package feature-mode
  :straight t
  :ensure t)

(use-package jenkinsfile-mode
  :straight t)

(use-package cmake-mode
  :straight t
  :config
  (defun -company-cmake-mode ()
    (setq-local company-backends '(company-cmake)))
  (add-hook 'cmake-mode-hook '-company-cmake-mode))

;; Configuration layer for python
(use-package config-programming-python
  :load-path "."
  :demand)

(use-package go-mode
  :straight t)

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t
  :defer t
  :hook (clojure-mode))

(use-package typescript-mode
  :straight t
  :demand
  :config
  (setq typescript-indent-level 2)
  )

;; Editting Javascript
(use-package rjsx-mode
  :straight t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package prettier-js
  :straight t
  :hook (rjsx-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--single-quote" "--jsx-single-quote")))


;; optionally
;; Add keybindings for interacting with Cargo
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t)
  )

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (defun -company-writing-hook ()
    "Setup completion for emacs lisp "
    (setq company-backends '((company-ispell company-dabbrev company-yasnippet company-files))))
  (add-hook 'markdown-mode-hook '-company-writing-hook))

(use-package yaml-mode
  :straight t)

(use-package slime
  :straight t
  :config
  ;; Set your lisp system and, optionally, some contribs
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))


(provide 'config-programming)
