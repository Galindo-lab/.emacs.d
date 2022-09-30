;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ----------------------------------------------------------------------------
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
;; ----------------------------------------------------------------------------

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

(setq-default 
 cursor-type 'bar                  ;Tipo del cursor
 tab-width 4                       ;Tamaño del tab
 indent-tabs-mode nil              ;Desactivar tabs
 scroll-step 1                     ;Smooth scrolling
 inhibit-startup-screen t          ;Hide startup screen
 use-dialog-box nil                ;Disable the use of dialog boxes
 )

(set-fringe-mode 10)               ;Espaciado
(column-number-mode t)             ;Numero de columna en el modeline
(line-number-mode t)               ;Numero de fila en el modeline
(scroll-bar-mode -1)               ;Scroll bars visibles
(display-time-mode -1)             ;Mostrar la hora
(display-battery-mode -1)          ;Mostrar batteria
(delete-selection-mode 1)          ;Typed text replaces the selection
(tool-bar-mode -1)                 ;Barra de herramientas visisles
(menu-bar-mode -1)                 ;Menu de herramientas visible
(global-hl-line-mode 1)            ;Cambiar el color de la line actual
(global-auto-revert-mode 1)        ;Reload file changes on disk

;;Usar solo y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable 'tramp-backup-directory-alist
                        backup-directory-alist)

;; Using garbage magic hack.
(use-package gcmh
  :config
  ;; Setting garbage collection threshold
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)
  (gcmh-mode 1)
  )

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
        neo-window-fixed-size -1
        neo-autorefresh t
        ;; neo-window-position 'right
        )
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
                          ;; (bookmarks . 10)
                          ))

  (dashboard-setup-startup-hook)
  )

(use-package doom-themes
  :config
  (load-theme 'doom-opera t)
  )

(use-package company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (add-to-list 'company-backends 'company-clang)
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

(use-package rainbow-mode
  )

(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat
        telephone-line-mode 1)
  )



(use-package eshell
  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (format-time-string "[%H:%M]" (current-time))
           (if (magit-get-current-branch)
               (concat "[git:" (magit-get-current-branch) "]" )
             "")
           " "
           (abbreviate-file-name (eshell/pwd))
           "\n"
           " > "
           )))

  (setq eshell-prompt-regexp " > ")   ; or "└─> "
  (setq eshell-prompt-string " > ")   ; or "└─> "
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

;; CUSTOM ---------------------------------------------------------------------



(setq org-babel-python-command "python3"

      ;; default-frame-alist 
      ;; '(
      ;;   ;; (font . "Source Code Pro-10")
      ;;   ;; (font . "Fira Code Regular-10")
      ;;   (font . "Monoid Regular-10")
      ;;   )

      org-plantuml-jar-path 
      (expand-file-name "~/Programas/platinuml/plantuml-1.2022.2.jar")

      )

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet/"))
  (yas-global-mode 1)
  )


(use-package cc-mode
   :config
   (add-hook 'c-mode-common-hook
             (lambda ()
               (message "aaaaaaaaaaaa")
               )
             )
  )

;; (use-package vterm
;;   :init
;;   (defvar vterm-install t))


;; (use-package tron-legacy-theme
;;   :config
;;   (setq tron-legacy-theme-vivid-cursor t)
;;   (load-theme 'tron-legacy t))


;; (use-package sublime-themes
;;   :config
;;    (load-theme 'dorsey)
;;   )

;; ---------------------------------------
;; load elscreen
;; ---------------------------------------

;; (add-to-list 'load-path (expand-file-name "~/elisp"))

;; (use-package tabbar
;;   :config
;;   (tabbar-mode 1)
;;   (setq 'tabbar-use-images nil))


;; (use-package awesome-tab
;;   :load-path "./elisp/awesome-tab"
;;   :config
;;   (awesome-tab-mode t))

;; (use-package elscreen
;;   :config
;;   (setq elscreen-tab-display-kill-screen nil)
;;   (setq elscreen-tab-display-control nil)
;;   (elscreen-start))

(use-package imenu-list)

(use-package lua-mode)

(use-package ess)

(use-package nasm-mode
  :mode "\\.asm\\'"
  :init
  )

;; (use-package js2-mode
;;   :mode
;;   (("\\.js\\'" . js2-mode))
;;   )

(use-package org
  :hook
  (org-mode . (lambda ()
                (org-indent-mode t)
                (org-content 2)
                (toggle-truncate-lines)
                ))

  :config
  (setq org-confirm-babel-evaluate nil)
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
                               (plantuml . t)
                               (shell . t)))

  (setq org-html-htmlize-output-type 'nil)

  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment))

  )



