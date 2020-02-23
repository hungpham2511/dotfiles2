
(use-package go-mode
  :straight t)

(use-package clojure-mode
  :straight t
  )

(use-package cider
  :straight t)

;; Editting Javascript
(use-package rjsx-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  )

(use-package prettier-js
  :straight t
  :hook (rjsx-mode . prettier-js-mode))

(use-package counsel-etags
  :straight t
  :bind (
	 :map evil-normal-state-map
	 ("C-]" . counsel-etags-find-tag-at-point)
	 )
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  ;; Auto tag regeneration
  (setq counsel-etags-update-interval 600)

  (push "build" counsel-etags-ignore-directories)
  (push ".mypy_cache" counsel-etags-ignore-directories)
  (push "TAGS" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames)
  (push "*.html" counsel-etags-ignore-filenames)
  )


;; reference for setup rust mode
;; https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/
(use-package lsp-mode
  :straight t
  :load-path "/home/hung/git/lsp-mode"
  :hook (rust-mode . lsp)
  ;; :hook (python-mode . lsp)
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-render-all nil)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/home/hung/Envs/p3/bin/pyls")
  		    :major-modes '(python-mode)
  		    :server-id 'pyls))

  )


;; optionally
(use-package lsp-ui :commands lsp-ui-mode :straight t)
(use-package company-lsp :commands company-lsp :straight t)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t)
  )

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; finish setting up rust for emacs

;; (use-package lsp-python-ms
;;   :straight t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

(use-package flycheck
  :straight t
  :demand
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  (setq flycheck-python-flake8-executable "/home/hung/Envs/p3/bin/python")
  (setq flycheck-flake8rc "/home/hung/dotfiles2/.flake8rc")
  
  )

(use-package yasnippet-snippets
  :straight t
  :config
  (yas-global-mode 1))

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
  (setq slime-contribs '(slime-fancy))
  )


(use-package  highlight-symbol
  :straight t
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)))

;; Completion package
(use-package company
  :straight t
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
  :straight t
  )


(use-package smartparens
  :straight
  :commands smartparens-global-mode
  :config
  (progn
    (smartparens-global-mode 1)
    (require 'smartparens-config)
    (sp-local-pair 'org-mode "$" "$")
    (eval-after-load 'org-mode     '(require 'smartparens-org))
    (eval-after-load 'python-mode   '(require 'smartparens-python))
    (show-smartparens-global-mode)
    (smartparens-global-mode)
    (set-face-foreground 'sp-show-pair-match-face "#8ec07c")))


(provide 'programming)
