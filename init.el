;; 
;;         ##    .#              
;; /     ###############          Luis E. Galindo Amaya
;; ######     ,#     /####        https://galindosoft.neocities.org
;; (##       #          #######   https://github.com/Galindo-lab
;;         #        ##     #    
;;       (      ## ####         
;;      #,  ,##      ##         
;;     #####         ##         
;;                   / 
;;

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Inicializar 'use-package' para plataformas no unix
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq-default cursor-type 'bar)         ;Tipo del cursor
(setq-default tab-width 4)              ;Tamaño del tab
(setq-default indent-tabs-mode nil)     ;Desactivar tabs

(setq scroll-step 1)                    ;Smooth scrolling
(setq inhibit-startup-screen t)         ;Hide startup screen
(setq use-dialog-box nil)               ;Disable the use of dialog boxes

(global-hl-line-mode 1)                 ;Cambiar el color de la line actual
(global-auto-revert-mode 1)             ;Reload file changes on disk

(set-fringe-mode 10)                    ;Espaciado
(tool-bar-mode -1)                      ;Barra de herramientas visisles
(menu-bar-mode -1)                      ;Menu de herramientas visible
(column-number-mode t)                  ;Numero de columna en el modeline
(line-number-mode t)                    ;Numero de fila en el modeline
(scroll-bar-mode -1)                    ;Scroll bars visibles
(display-time-mode -1)                  ;Mostrar la hora
(display-battery-mode -1)               ;Mostrar batteria
(delete-selection-mode 1)               ;Typed text replaces the selection

(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-parameter                    ;frame visible
 (selected-frame) 'undecorated t) 

(set-frame-parameter                    ;fondo trasparente
 (selected-frame) 'alpha '(95 95))

(add-to-list                            ;transparencia del borde
 'default-frame-alist '(alpha 85 85))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable 'tramp-backup-directory-alist
                        backup-directory-alist)

(use-package gcmh                       ;Using garbage magic hack.
  :config
  (gcmh-mode 1)
  )

(setq gc-cons-threshold 402653184       ;Setting garbage collection threshold
      gc-cons-percentage 0.6)

(use-package recentf
  :config
  (recentf-mode 1)
  )

(use-package ivy
  :init
  (ivy-mode 1)

  :bind
  ("C-x <" . ido-switch-buffer)
  )

(use-package magit
)

(use-package projectile
  :config
  (projectile-mode +1)

  :bind
  (:map projectile-mode-map
        ("s-p"   . projectile-command-map)
        ("C-c p" . projectile-command-map))
  )

(use-package neotree
  :bind
  ("C-x j" . neotree-toggle)

  :config
  (setq neo-theme 'ascii
        neo-smart-open t
        neo-window-width 25
        neo-window-fixed-size -1)
  )

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package crux
  :bind
  ("C-c f"   . crux-recentf-find-file)
  ("C-,"     . crux-find-user-init-file)
  ("C-x C-u" . crux-upcase-region)
  ("C-x C-l" . crux-downcase-region)
  ("C-x M-c" . crux-capitalize-region)
  ("C-c k"   . crux-kill-other-buffers)
  ("C-c t"   . crux-visit-term-buffer)
  )

(use-package dashboard
  :config
  (setq dashboard-startup-banner "~/.emacs.d/res/nu_35.txt")
  (setq dashboard-center-content t
        dashboard-items '((recents  . 10)
                          (bookmarks . 10)))

  (dashboard-setup-startup-hook)
  )

(use-package doom-themes
  :config
  (load-theme 'doom-opera t)
  )

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (global-company-mode)
  )

(use-package company-quickhelp
  :config
  (company-quickhelp-mode)
  )

(use-package git-gutter
  :ensure t

  ;; :config
  ;; (global-git-gutter-mode +1)
  )

(use-package centered-window
  )

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))

  :hook
  ((prog-mode . hl-todo-mode)
   (yaml-mode . hl-todo-mode)
   (org-mode . hl-todo-mode))

  :config
  (setq hl-todo-mode 1)
  )

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  )

(use-package format-all
  )

(use-package emmet-mode
  :hook
  ((sgml-mode . emmet-mode))
  )

(use-package gnuplot
  )

(use-package org
  :hook
  (org-mode . (lambda ()
                (org-indent-mode t)
                (org-content 2)))

  :config
  (setq org-support-shift-select t
        org-preview-latex-default-process 'dvisvgm
        org-html-htmlize-output-type `nil
        org-src-tab-acts-natively t
        org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (python . t)
                               (latex . t)
                               (ditaa . t)
                               (maxima . t)
                               (octave . t)
                               (plantuml . t)))

  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment))

  )

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")

  :mode
  ("README\\.md\\'" . gfm-mode)
  )

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  )

(use-package company-anaconda
  :init 
  (require 'rx)

  :after 
  (company)

  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

(use-package company-web
  :init
  (require 'rx)

  :after
  (company)

  :config
  (add-to-list 'company-backends 'company-web-html)
  )

;; CUSTOM -----------------------------------------------------

(setq org-babel-python-command "python3"

      default-frame-alist 
      '((font . "Source Code Pro-10"))

      org-plantuml-jar-path 
      (expand-file-name "~/Programas/platinuml/plantuml-1.2022.2.jar")

      )
