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
  :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'cpp-configs)