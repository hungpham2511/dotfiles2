;; setup c++ programming
(require 'google-c-style)
(message "Configuring c++ code-style")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook 'google-set-c-style)

(use-package cmake-mode
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;     Code completion with cqueery   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :hook (c++-mode . lsp)
  :straight t
  :commands lsp
  :config
  (setq gc-cons-threshold (* 1000 1024 1024))
  (setq read-process-output-max (* 8 1048 1024)) ;; 1mb
  (setq lsp-completion-provider :capf)
  )

;; ;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t)

(use-package internal-config-python
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

;; finish setting up rust for emacs
(use-package flycheck
  :straight t
  :demand
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq flycheck-python-flake8-executable "/home/hung/Envs/p3/bin/python")
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

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :straight t)

(use-package slime
  :straight t
  :config
  ;; Set your lisp system and, optionally, some contribs
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package  highlight-symbol
  :straight t
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)))

;; Completion package
(use-package company
  :straight t
  :demand
  :commands global-company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  ;; Disable company in org-mode because it's super slow.
  (setq company-global-modes '(not org-mode))
  )


(use-package smartparens
  :straight t
  :commands smartparens-global-mode
  :bind (:map evil-normal-state-map
	      ;; Most important commands. Move within the sexp.
	      ("C-M-a" . sp-beginning-of-sexp)
	      ("C-M-e" . sp-end-of-sexp)
	      ("C-M-f" . sp-forward-sexp)
	      ("C-M-b" . sp-backward-sexp)

	      ;; move between sexp of the same level
	      ("M-l" . sp-next-sexp)
	      ("M-h" . sp-backward-sexp)

	      ;; move between symbols
	      ("M-f" . sp-forward-symbol)
	      ("M-b" . sp-backward-symbol)

	      ("M-j" . sp-down-sexp)
	      ("M-k" . sp-backward-up-sexp)
	 )
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'org-mode "$" "$")
    (eval-after-load 'org-mode     '(require 'smartparens-org))
    (eval-after-load 'python-mode   '(require 'smartparens-python))
    (show-smartparens-global-mode)
    (smartparens-global-mode)
    (set-face-foreground 'sp-show-pair-match-face "#8ec07c")))

(provide 'config-programming)
