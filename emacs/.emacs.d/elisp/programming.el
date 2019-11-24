
(use-package go-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  )

(use-package cider
  :ensure t)

;; Editting Javascript
(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  )

(use-package prettier-js
  :ensure t
  :hook (rjsx-mode . prettier-js-mode))

;; reference for setup rust mode
;; https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/
(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :hook (python-mode . lsp)
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/home/hung/Envs/p3/bin/pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls))
  )


;; optionally
(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(use-package company-lsp :commands company-lsp :ensure t)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  )

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; finish setting up rust for emacs

;; (use-package lsp-python-ms
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

(use-package flycheck
  :ensure t
  :demand
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  (setq flycheck-python-flake8-executable "/home/hung/Envs/p3/bin/python")
  (setq flycheck-flake8rc "/home/hung/dotfiles2/.flake8rc")
  
  )

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-global-mode 1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t)

(use-package slime
  :ensure t
  :config
  ;; Set your lisp system and, optionally, some contribs
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )


(use-package  highlight-symbol
  :ensure t
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)))

;; Completion package
(use-package company
  :ensure t
  :demand
  :bind (
	 ("C-M-i" . company-complete)
	 :map python-mode-map
	 ("C-M-i" . company-complete)
	 :map emacs-lisp-mode-map
	 ("C-M-i" . company-complete)
	 )
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package py-autopep8
  :ensure t)


(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'org-mode "$" "$")
    (eval-after-load 'org-mode     '(require 'smartparens-org))
    (eval-after-load 'python-mode   '(require 'smartparens-python))
    (show-smartparens-global-mode)
    (smartparens-global-mode)
    (set-face-foreground 'sp-show-pair-match-face "#8ec07c")))


(provide 'programming)
