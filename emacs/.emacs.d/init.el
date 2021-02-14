;;; Commentary: The entrypoint to my emacs config.
(require 'package)
;;; Code:

;; Configure straight: A package for managing package installation.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package to install, manage configurations and manage key
;; bindings for different packages.
(straight-use-package 'use-package)

; Sub modules loadpath
(add-to-list 'load-path "~/.emacs.d/elisp/")
(eval-when-compile
  (require 'use-package))
(straight-use-package 'diminish)
(require 'bind-key)

(use-package general
  :ensure t
  :straight t
  :config
  (message "Configuring key bindings below"))

(use-package config-org
  :load-path "~/.emacs.d/elisp"
  :demand)

;; Core packages: evil, magit
(use-package config-general
  :load-path "~/.emacs.d/elisp"
  :demand
  ;; :after evil org
  :bind (("<f5>" . org-random-entry-TODO)
	 ("<f6>" . org-random-entry-TOREAD)
	 ("<f7>" . org-random-entry-STUDY)
	 ("<f1>" . find-dired-dropbox-iname)
	 :map org-mode-map
	 ("C-c C-q" . counsel-org-tag-2)
	 ("C-c C-p" . org-previous-visible-heading))
  :config
  (setq browse-url-browser-function 'browse-url-chrome))

;; setup stage 4: configure org setup
;; My personal library, some snippets thats it
(use-package config-misc
  :load-path "~/.emacs.d/elisp"
  :demand
  :after org)


;; Programming/modes installation
(use-package config-programming
  :load-path "~/.emacs.d/elisp"
  :demand
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-buffer)))

(use-package config-latex
  :load-path "~/.emacs.d/elisp"
  :demand)

;; Appearance is the last thing to configure. This ensures every other
;; elements are loaded before making them look nice.
(use-package config-theme
  :load-path "~/.emacs.d/elisp"
  :config
  ;; Uncomment the theme you want
  (load-theme 'my-solarized-dark)
  ;; (set-face-font 'default "Monospace-12") ; default font
  (set-face-font 'default "RobotoMono-11") ; Có thể hiển thị được tiếng Việt đầy đủ
  ;; (set-face-font 'default "Fira Code Retina-9:style=Retina,Monospaced")
  (message "Configuring font"))

;; Load a collection of useful functions
(use-package test-mod
  :load-path "~/.emacs.d/elisp"
  :demand
  :general
  ("C-c h i" 'h/visit-init-el))

(general-define-key
 :states 'motion
 "<f1>" 'general-describe-keybindings
 
 ;; search-related command
 "C-c r" 'rg-project
 
 ;; movements
 "f" 'ace-jump-mode
 "C-f" 'ace-link
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "35a90245620bd848ec3aff5474eeaf6fad2caf18a702dd4747fcc42ec90b648d" "bfb3279da01ba13806e6d9ad838289164e216f47b6bcc98c48cc5abdc9490787" "b54e3febae8bb7c2c37ef30e0427f775ebb6475c53ec4947b925fde29d66072f" "210c29893de2eca72f992e223ba4cf6bc5d21a15129f50bfba2cffa03b4fde1c" "fcd838b7c592b6cdc33004988c7944e284b19ba06233de38d22ad9f79b8f1e4b" "49d54111ff0f3bc2ee07f23ee793808fbb6a116db08b986c444259698ee620c7" "20e95062101a0e48aca481ea0e3eed32c3b29bcd7474ae590947511f27f1b4d4" "a17ac67dfae6c9667bea31b42fc9a96c18c878ad8e4560583e8f5a27b8755e6a" "9bd39739368f64815f226b474a13dd2e41df47dc1fe5d2c05c84d3bb917cd7e6" "847b538c1868ea4b8a316acaa4da76813d7fdbbd2d598ea865dcc9ab72ffffdb" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "cceea05d19dc03690d677e360a5b1a2ebb0e9d8a65d1ec1cd7b49fefcef3ea3e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538" "3018b767e4d5b0ddf37c909b2ea2a29c249d5a88645b4243d5b97fe2d67c71ab" "36c2b7efdc064944eb067e56c7ec65808a6cee0f63ce068b693fb30b110e57e5" "8ba0a9fc75f2e3b4c254183e814b8b7b8bcb1ad6ca049fde50e338e1c61a12a0" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "3dd98b7c43041526b35a4466c165d55400207ea9a82fd765c5c4bffffe1a7bd9" "2f524d307a2df470825718e27b8e3b81c0112dad112ad126805c043d7c1305c6" "3b2d3c38bac1160e32b8fb79ebc95e5464df4f6866fdb17c188b0adf4d02342a" "5c85b6f7f76fe0e0979da4c650dee525ae5185b134cb0fdfb12eeb580ea4dafb" "dc9a8d70c4f94a28aafc7833f8d05667601968e6c9bf998791c39fcb3e4679c9" "5a970147df34752ed45bfdf0729233abfc085d9673ae7e40210c5e2d8f624b08" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" default))
 '(default-input-method "vietnamese-telex")
 '(deft-auto-save-interval 120 t)
 '(deft-default-extension "org" t)
 '(deft-directory "~/org" t)
 '(deft-extensions '("txt" "text" "org"))
 '(deft-recursive t t)
 '(deft-use-filter-string-for-filename t t)
 '(diff-hl-margin-mode t)
 '(fci-rule-color "#eee8d5")
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(indent-tabs-mode nil)
 '(ivy-mode t)
 '(lsp-python-ms-extra-paths
   ["/home/hung/eureka/eureka/packages/optics-handling/optics_handling_perception/src" "/home/hung/eureka/eureka/packages/eureka-controller/"])
 '(lsp-python-ms-python-executable-cmd "/home/hung/Envs/p3/bin/python")
 '(lsp-treemacs-sync-mode t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-bullets-bullet-list '("λ" "π" "α" "β"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hugo-front-matter-format "yaml")
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-roam-completion-system 'ivy)
 '(org-roam-directory "/home/hung/org/")
 '(org-roam-graph-viewer "/usr/bin/google-chrome")
 '(org-roam-link-title-format "%s")
 '(org-startup-indented nil)
 '(package-selected-packages
   '(prettier-js rjsx-mode web-mode smart-mode-line lsp-python-ms lsp-ui cargo company-lsp lsp-mode flycheck-rust ejson-mode wgrep dracula-theme typescript bash-completion neotree darktooth-theme rust-mode cpp-configs json-mode jsx-mode arduino-mode go-mode cider clojure-mode evil-nerd-commenter js2-mode slime py-autopep8 dockerfile-mode docker all-the-icons ox-reveal reveal-ox ivy-bibtex irony-eldoc cmake-mode magit gruvbox-theme docker-tramp scad-mode plantuml-mode haskell-mode ein writeroom-mode matlab-mode cython-mode all-the-icons-dired dired-sidebar ob-blockdiag ob-ipython my-misc-config company-anaconda anaconda-mode overall_configurations overall-configurations org-element smartparens cdlatex hydra ess lua-mode yasnippet-snippets company-mode company-jedi markdown-mode yaml-mode paredit evil-magit company-irony irony traad pymacs projectile rebecca-theme solarized-theme org-pdfview highlight-symbol smex avy use-package))
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path "/opt/plantuml/plantuml.jar")
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values
   '((lsp-python-ms-extra-paths .
                                ["/home/hung/eureka/eureka/packages/optics-handling/optics_handling_perception/src" "/home/hung/eureka/eureka/packages/optics-handling/optics_handling_control/src" "/home/hung/eureka/eureka/packages/denso_common/denso_control/src" "/home/hung/eureka/eureka/packages/denso_common/turin_control/src" "/home/hung/eureka/eureka/packages/eureka-controller/"])
     (checkdoc-package-keywords-flag)
     (eval require 'org-roam-dev)
     (lsp--override-calculate-lisp-indent\? . t)
     (flycheck-disabled-checkers quote
                                 (emacs-lisp-checkdoc))
     (eval progn
           (let
               ((dirloc-lsp-defun-regexp
                 (concat
                  (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
                  (or
                   (bound-and-true-p lisp-mode-symbol-regexp)
                   "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                  "\\)")))
             (add-to-list 'imenu-generic-expression
                          (list "Functions" dirloc-lsp-defun-regexp 1)))
           (defvar lsp--override-calculate-lisp-indent\? nil "Whether to override `lisp-indent-function' with
              the updated `calculate-lisp-indent' definition from
              Emacs 28.")
           (defun wrap-calculate-lisp-indent
               (func &optional parse-start)
             "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
             (if
                 (not lsp--override-calculate-lisp-indent\?)
                 (funcall func parse-start)
               (save-excursion
                 (beginning-of-line)
                 (let
                     ((indent-point
                       (point))
                      state
                      (desired-indent nil)
                      (retry t)
                      whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
                   (cond
                    ((or
                      (markerp parse-start)
                      (integerp parse-start))
                     (goto-char parse-start))
                    ((null parse-start)
                     (beginning-of-defun))
                    (t
                     (setq state parse-start)))
                   (unless state
                     (while
                         (<
                          (point)
                          indent-point)
                       (setq state
                             (parse-partial-sexp
                              (point)
                              indent-point 0))))
                   (while
                       (and retry state
                            (>
                             (elt state 0)
                             0))
                     (setq retry nil)
                     (setq calculate-lisp-indent-last-sexp
                           (elt state 2))
                     (setq containing-sexp
                           (elt state 1))
                     (goto-char
                      (1+ containing-sexp))
                     (if
                         (and calculate-lisp-indent-last-sexp
                              (> calculate-lisp-indent-last-sexp
                                 (point)))
                         (let
                             ((peek
                               (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                           (if
                               (setq retry
                                     (car
                                      (cdr peek)))
                               (setq state peek)))))
                   (if retry nil
                     (goto-char
                      (1+ containing-sexp))
                     (setq whitespace-after-open-paren
                           (looking-at
                            (rx whitespace)))
                     (if
                         (not calculate-lisp-indent-last-sexp)
                         (setq desired-indent
                               (current-column))
                       (parse-partial-sexp
                        (point)
                        calculate-lisp-indent-last-sexp 0 t)
                       (cond
                        ((looking-at "\\s("))
                        ((>
                          (save-excursion
                            (forward-line 1)
                            (point))
                          calculate-lisp-indent-last-sexp)
                         (if
                             (or
                              (=
                               (point)
                               calculate-lisp-indent-last-sexp)
                              whitespace-after-open-paren)
                             nil
                           (progn
                             (forward-sexp 1)
                             (parse-partial-sexp
                              (point)
                              calculate-lisp-indent-last-sexp 0 t)))
                         (backward-prefix-chars))
                        (t
                         (goto-char calculate-lisp-indent-last-sexp)
                         (beginning-of-line)
                         (parse-partial-sexp
                          (point)
                          calculate-lisp-indent-last-sexp 0 t)
                         (backward-prefix-chars)))))
                   (let
                       ((normal-indent
                         (current-column)))
                     (cond
                      ((elt state 3)
                       nil)
                      ((and
                        (integerp lisp-indent-offset)
                        containing-sexp)
                       (goto-char containing-sexp)
                       (+
                        (current-column)
                        lisp-indent-offset))
                      (calculate-lisp-indent-last-sexp
                       (or
                        (and lisp-indent-function
                             (not retry)
                             (funcall lisp-indent-function indent-point state))
                        (and
                         (save-excursion
                           (goto-char indent-point)
                           (skip-chars-forward " 	")
                           (looking-at ":"))
                         (save-excursion
                           (goto-char calculate-lisp-indent-last-sexp)
                           (backward-prefix-chars)
                           (while
                               (not
                                (or
                                 (looking-back "^[ 	]*\\|([ 	]+"
                                               (line-beginning-position))
                                 (and containing-sexp
                                      (>=
                                       (1+ containing-sexp)
                                       (point)))))
                             (forward-sexp -1)
                             (backward-prefix-chars))
                           (setq calculate-lisp-indent-last-sexp
                                 (point)))
                         (> calculate-lisp-indent-last-sexp
                            (save-excursion
                              (goto-char
                               (1+ containing-sexp))
                              (parse-partial-sexp
                               (point)
                               calculate-lisp-indent-last-sexp 0 t)
                              (point)))
                         (let
                             ((parse-sexp-ignore-comments t)
                              indent)
                           (goto-char calculate-lisp-indent-last-sexp)
                           (or
                            (and
                             (looking-at ":")
                             (setq indent
                                   (current-column)))
                            (and
                             (<
                              (line-beginning-position)
                              (prog2
                                  (backward-sexp)
                                  (point)))
                             (looking-at ":")
                             (setq indent
                                   (current-column))))
                           indent))
                        normal-indent))
                      (desired-indent)
                      (t normal-indent)))))))
           (when
               (< emacs-major-version 28)
             (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent)))
     (project-vc-merge-submodules)))
 '(smartparens-global-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-code ((t (:width expanded))))
;;  '(org-date ((t (:foreground "#8be9fd" :underline nil))))
;;  '(org-level-1 ((t (:inherit bold :foreground "#ff79c6" :weight bold :height 1.1))))
;;  '(org-level-2 ((t (:inherit bold :foreground "#50fa7b" :height 1.1))))
;;  '(org-level-3 ((t (:foreground "#bd93f9" :weight normal :height 1.0))))
;;  '(org-link ((t (:foreground "#8be9fd" :overline nil :underline nil :width normal)))))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
