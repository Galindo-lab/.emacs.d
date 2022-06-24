;; test

;; Fuetes
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

(require 'recentf)
(recentf-mode 1)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package crux
  :ensure t)

(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  (hl-todo-mode 1))

(use-package htmlize
  :ensure t)

;; explorador de archivos
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'ascii
        neo-smart-open t
        neo-window-width 25
        neo-window-fixed-size -1)
  )

;; desactivar los numeros y el warp de texto
(add-hook 'neo-after-create-hook
          (lambda (&rest _)
            (display-line-numbers-mode -1)
            (visual-line-mode -1))
          )

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  ;; (global-company-mode t)
  )

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package csv-mode
  :ensure t)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t)
  ;; (setq dashboard-banner-logo-title "your custom text")
  ;; (setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook))

(use-package muban
  :ensure t
  :bind
  ("C-c i" . muban-apply))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package emmet-mode
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package gnuplot
  :ensure t)

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1)
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  )

(use-package olivetti
  :ensure t)

(use-package format-all
  :ensure t
  )

(use-package lorem-ipsum
  :ensure t
  )

(global-set-key (kbd "C-c C-l s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c C-l p") 'lorem-ipsum-insert-paragraphs)
(global-set-key (kbd "C-c C-l l") 'lorem-ipsum-insert-list)

;; para editar codigo html, css y js en el mismo archivo
(use-package web-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

(use-package racket-mode
  :ensure t)

(use-package octave
  :ensure t
  :init
  :bind (:map octave-mode-map
              ("C-c C-c" . octave-send-buffer)
              ))

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(use-package org
  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment))
  :config
  (setq org-babel-python-command "python3")
  (setq org-support-shift-select t)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-html-htmlize-output-type `nil)
  (setf org-html-mathjax-template "<script src='https://polyfill.io/v3/polyfill.min.js?features=es6'></script><script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>")
                                        ;preview tikz
  (setq org-src-tab-acts-natively t)	;indentar src_blocks
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))) ;tamaño de preview

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (org-content 2)
            ;; (display-line-numbers-mode -1)
            ))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (python . t)
                             (latex . t)
                             (ditaa . t)
                             (maxima . t)
                             (octave . t)
                             (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/Programas/platinuml/plantuml-1.2022.2.jar"))

(setq org-latex-pdf-process
      (let
          ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
                        " --synctex=1"
                        " -output-directory %o %f")))
        (list cmd
              "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
              "cd %o; bibtex %b"
              cmd
              cmd)))

(set-face-attribute 'default nil
                    :font "Fira Code"
                    :height 98 )


(setq default-frame-alist '((font . "Source Code Pro-10")))

(setq inhibit-startup-message t)        ;Pantalla de inicio de emacs
;; (global-display-line-numbers-mode t)    ;numeros de linea
(set-fringe-mode 10)                    ;espacio entre el frame y el buffer
;; (global-visual-line-mode 1)             ;separar lineas
(setq-default cursor-type 'bar)         ;tipo del cursor
(setq-default tab-width 4)              ;tamaño del tab
(tool-bar-mode -1)                      ;barra de herramientas visisles
(menu-bar-mode -1)                      ;menu de herramientas visible
(setq column-number-mode t)             ;numero de columna en el modeline
(line-number-mode t)                    ;numero de fila en el modeline
(scroll-bar-mode -1)                    ;scroll bars visibles

;; (use-package doom-themes
;;   :ensure t
;;   :init (load-theme 'doom-one t))


(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-opera t))

;; eliminar elemento seleccionado
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-hook 'eshell-mode-hook
          (lambda (&rest _)
            (display-line-numbers-mode -1)
            (visual-line-mode -1)))

;; incluidas
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x j") 'neotree-toggle)
(global-set-key (kbd "C-x <") 'ido-switch-buffer)
(global-set-key (kbd "C-M-z") 'toggle-80-editting-columns-balanced)

;; Crux
(global-set-key (kbd "C-c f") 'crux-recentf-find-file)
(global-set-key (kbd "C-,") 'crux-find-user-init-file)
(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key (kbd "C-x C-l") 'crux-downcase-region)
(global-set-key (kbd "C-x M-c") 'crux-capitalize-region)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)

;; muban
(global-set-key (kbd "C-c i") 'muban-apply)

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)
(setq org-confirm-babel-evaluate nil)

(setq scroll-step            1
      scroll-conservatively  10000)
