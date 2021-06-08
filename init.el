(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

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

;; Mantiene una lista con los archivos abiertos recientemente es necesario que este activado.
(require 'recentf)
(recentf-mode 1)

;; Autocompletado en el minibufer
(use-package ivy
  :config
  (ivy-mode 1))                      ;Activar ivy en todos los buffers

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

;; Colorear los valores RGB
(use-package rainbow-mode)

;; Getor de git para emacs
(use-package magit)

;; Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux)

;; eshell hooks
(add-hook 'eshell-mode-hook
          (lambda (&rest _) 
            (display-line-numbers-mode -1)
              (visual-line-mode -1)))

;; highlight todo
(use-package hl-todo
     :custom-face
     (hl-todo ((t (:inherit hl-todo :italic t))))
     :hook ((prog-mode . hl-todo-mode)
            (yaml-mode . hl-todo-mode)
            (org-mode . hl-todo-mode))
     :config
      (hl-todo-mode 1))

(use-package org
  :bind
  (:map org-mode-map
        ("<M-return>" . org-toggle-latex-fragment))
  :config
  (setq org-support-shift-select t)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (setq org-content 2)
            (display-line-numbers-mode -1)))

;; Tipografia
(set-face-attribute 'default nil
                    :font "Fira Code"
                    :height 98 )

;; Tema
(load-file "~/.emacs.d/themes/sea.el")

;; otros
(setq inhibit-startup-message t)     ;Pantalla de inicio de emacs 
(global-display-line-numbers-mode t) ;numeros de linea
(scroll-bar-mode -1)                 ;scroll bars visibles
(tool-bar-mode -1)                   ;barra de herramientas visisles
(menu-bar-mode -1)                   ;menu de herramientas visible
(set-fringe-mode 10)                 ;espacio entre el frame y el buffer
(global-visual-line-mode 1)          ;separar lineas 
(setq-default cursor-type 'bar)      ;tipo del cursor

;; Mode line
(setq column-number-mode t)          ;numero de columna 
(line-number-mode t)                 ;numero de fila
(display-time-mode -1)		 ;mostrar la hora
(display-battery-mode -1)            ;mostrar batteria

;; Frame
;;(set-frame-parameter (selected-frame) 'undecorated t) ;frame visible
;;(set-frame-parameter (selected-frame) 'alpha '(95 95)) ;fondo trasparente
;;(add-to-list 'default-frame-alist '(alpha 85 85)) ;transparencia del borde

;; Varios
;;(desktop-save-mode 1)                   ;guardar escritorio
(find-file "~/notes.org")               ;abrir archivo al iniciar

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
