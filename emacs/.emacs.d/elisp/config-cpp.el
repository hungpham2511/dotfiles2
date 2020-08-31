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
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 9048 1024)) ;; 1mb
  )

;; ;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t)

(use-package company-lsp
  :straight t
  :commands company-lsp)

(use-package lsp-python-ms
  :straight t
  :demand
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(provide 'config-cpp)
