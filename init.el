(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
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

(setq create-lockfiles nil)

(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq-default cursor-type 'bar)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default scroll-step 1)
  (setq-default inhibit-startup-screen t)
  (setq-default use-dialog-box nil)
  (setq-default display-fill-column-indicator-column 80)

  (setq-default cursor-type 'bar)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  ;; (setq-default scroll-step 1)
  (setq-default inhibit-startup-screen t)

  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (add-to-list
   'backup-directory-alist (cons "." "~/.emacs.d/backups/"))

  (customize-set-variable
   'tramp-backup-directory-alist backup-directory-alist)

  (set-default 'truncate-lines -1)
  (set-fringe-mode 10)
  (delete-selection-mode 1)
  (column-number-mode t)
  (line-number-mode t)
  (global-auto-revert-mode 1)
  (global-hl-line-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  :bind
  ("<f5>" . recompile)

  :hook
  (text-mode-hook . auto-fill-mode))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-frame-parameter
 (selected-frame) 'undecorated t)

(tab-bar-mode 1)

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

(use-package magit)

(use-package recentf
  :config 
  (recentf-mode 1))

(use-package ivy
  :init   
  (ivy-mode 1)

  :bind   
  ("C-x <" . ido-switch-buffer))

(use-package neotree
  :bind    
  ("C-x j" . neotree-toggle)

  :config  
  (setq neo-window-width 32
        neo-theme 'ascii
        neo-smart-open t
        neo-window-fixed-size -1
        neo-autorefresh t
        neo-window-position 'right))

(use-package which-key
  :config
  (which-key-mode))

(use-package crux
  :bind
  ("C-c f"   . crux-recentf-find-file)
  ("C-,"     . crux-find-user-init-file)
  ("C-x C-u" . crux-upcase-region)
  ("C-x C-l" . crux-downcase-region)
  ("C-x M-c" . crux-capitalize-region)
  ("C-c k"   . crux-kill-other-buffers)
  ("C-c t"   . crux-visit-term-buffer)
  ("C-c r"   . crux-rename-file-and-buffer))

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)

  ;; (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-capf)

  (global-company-mode))

(use-package lsp-mode
  :hook
  (python-mode . lsp)
  (prolog-mode . lsp)

  :commands lsp)

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package git-gutter)

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))

  :hook
  (prog-mode . hl-todo-mode)
  (yaml-mode . hl-todo-mode)
  (org-mode . hl-todo-mode)

  :config
  (setq hl-todo-mode 1))

(use-package format-all)

(use-package emmet-mode
  :hook
  (sgml-mode . emmet-mode))

(use-package gnuplot)

(use-package rainbow-mode)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet/"))
  (yas-global-mode 1))

(use-package imenu-list)

(use-package lorem-ipsum)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "png"))

(use-package zen-mode)

(use-package centered-window
  :hook
  (org-mode . centered-window-mode)
  (prog-mode . centered-window-mode))

(use-package prog-mode
  :hook 
  (prog-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode))

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")

  :mode
  ("README\\.md\\'" . gfm-mode))

(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :init 
  (require 'rx)

  :after 
  (company)

  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-web
  :init
  (require 'rx)

  :after
  (company)

  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package lua-mode)

(use-package ess)

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package racket-mode)

(use-package haskell-mode)

(use-package prolog-mode
  :mode "\\.pl\\'"
  :hook 
  (prolog-mode . company-mode))

(use-package org
  :hook
  (org-mode . (lambda ()
                ;; (visual-line-mode)
                ;; (org-indent-mode t)
                (org-content 2)))

  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select t)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-html-htmlize-output-type `nil)
  (setq org-src-tab-acts-natively t)
  (setq org-html-htmlize-output-type 'nil)
  (setq org-latex-caption-above nil)
  (setq org-babel-python-command "python3")

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))

  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml-1.2023.10.jar"))

  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (latex . t)
     (ditaa . t)
     (maxima . t)
     (octave . t)
     (plantuml . t)
     (shell . t)))

  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment)))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/.emacs.d/roam/"))

  :bind 
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)

  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package vterm)

(use-package doom-themes
  :config
  (load-theme 'doom-opera t))

(defun doom-toggle-theme ()
  "Alterna entre dos temas personalizados en Emacs."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-opera-light)
      (progn
        (disable-theme 'doom-opera-light)
        (load-theme 'doom-opera t))
    (progn
      (disable-theme 'doom-opera)
      (load-theme 'doom-opera-light t))))

(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat
        telephone-line-mode 1))

(use-package fireplace)

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun reverse-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))
