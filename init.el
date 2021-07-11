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

;; Asegurarse de que los paquetes simepre esten instalados
(setq use-package-always-ensure t)

(require 'recentf)
(recentf-mode 1)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package rainbow-mode)

(use-package magit)

(use-package crux)

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  (hl-todo-mode 1))

(use-package htmlize)

;; explorador de archivos 
(use-package neotree
  :config
  (setq neo-theme 'ascii)
  (setq neo-smart-open t)
  (setq neo-window-width 25)
  (setq neo-window-fixed-size -1))

;; desactivar los numeros y el warp de texto 
(add-hook 'neo-after-create-hook
          (lambda (&rest _) 
            (display-line-numbers-mode -1)
            (visual-line-mode -1)))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

;; para editar codigo html, css y js en el mismo archivo
(use-package web-mode)

;; para hacer html más rapidos
(use-package emmet-mode)

(use-package lua-mode)

;; Tipografia
;; (set-face-attribute 'default nil
;;                     :font "Fira Code"
;;                     :height 98 )

;; Ajustes 
;; Pantalla de inicio de emacs
(setq inhibit-startup-message t)
;; numeros de linea
(global-display-line-numbers-mode t)
;; scroll bars visibles
(scroll-bar-mode -1)
;; barra de herramientas visisles
(tool-bar-mode -1)
;; menu de herramientas visible
(menu-bar-mode -1)
;; espacio entre el frame y el buffer	   
(set-fringe-mode 10)
;; separar lineas 
(global-visual-line-mode 1)
;; tipo del cursor
(setq-default cursor-type 'bar)
;; tamaño del tab
(setq-default tab-width 4)

;; Mode line
;; numero de columna
(setq column-number-mode t)
;; numero de fila
(line-number-mode t)
;; mostrar la hora             
(display-time-mode -1)
;; mostrar batteria
(display-battery-mode -1)

;; Frame
;; frame visible
;;(set-frame-parameter (selected-frame) 'undecorated t) 
;; fondo trasparente
;;(set-frame-parameter (selected-frame) 'alpha '(95 95))
;; transparencia del borde
;;(add-to-list 'default-frame-alist '(alpha 85 85)) 

;; Tema
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

;; guardar escritorio
;;(desktop-save-mode 1)                 
;; abrir archivo al iniciar
(find-file "~/notes.org")             
;; eliminar elemento seleccionado   
(delete-selection-mode 1)

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

(use-package org
  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment))
  :config
  (setq org-support-shift-select t)
  (setq org-preview-latex-default-process 'dvisvgm)	;preview tikz
  (setq org-src-tab-acts-natively t)	;indentar src_blocks
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))) ;tamaño de preview


(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (org-content 2)
            (display-line-numbers-mode -1)))

;; Babel
(org-babel-do-load-languages 'org-babel-load-languages '( (python . t) ) )
(setq org-babel-python-command "python3")

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

;; (defun kill-other-buffers ()
;;   "Kill all other buffers."
;;   (interactive)
;;   (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; (defun insert-current-date () (interactive)
;;   (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun toggle-80-editting-columns ()
  "Set the right window margin so the edittable space is only 80 columns."
  (interactive)
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (set-window-margins nil 0 (max (- (window-width) 80) 0)))))

(defun toggle-80-editting-columns-balanced ()
  "Set both window margins so the edittable space is only 80 columns."
  (interactive)
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (let* ((change (max (- (window-width) 80) 0))
             (left (/ change 2))
             (right (- change left)))
        (set-window-margins nil left right)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lua-mode company emmet-mode web-mode use-package rainbow-mode neotree magit ivy htmlize hl-todo exec-path-from-shell crux base16-theme ample-theme)))
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backups/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo ((t (:inherit hl-todo :italic t)))))

;; -------------------- Pruebas --------------------

;; https://www.reddit.com/r/emacs/comments/98prqr/how_would_i_make_a_keybinding_run_a_shell_command/

(defun run-buffer ()
  (interactive)
  (shell-command (concat "./eigenmath " buffer-file-name)))
(global-set-key (kbd "<f9>") 'run-buffer)
