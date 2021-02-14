;;; python-configs -- Summary
;;; Commentary:
; Simple snippets to use with init.el
;;; Code:
(require 'lsp-python-ms)
(require 'projectile)
(require 'json)

(defun pyu-load-lsp-python-ms-configurations ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (config-file (concat project-root ".config.json")))
    (message config-file)))

(defun pyu-set-extra-paths ()
  "Set extra path by searching from projectile root."
  (interactive)
  (setq-local lsp-python-ms-extra-paths
        ["/home/hung/eureka/eureka/packages/optics-handling/optics_handling_perception/src"
         "/home/hung/eureka/eureka/packages/optics-handling/optics_handling_control/src"
         "/home/hung/eureka/eureka/packages/optics-handling/optics_handling_calibration/src"
         "/home/hung/eureka/eureka/packages/denso_common/denso_control/src"
         "/home/hung/eureka/eureka/packages/denso_common/turin_control/src"
         "/home/hung/eureka/eureka/packages/eureka-controller/"]))

(provide 'python-utils)
