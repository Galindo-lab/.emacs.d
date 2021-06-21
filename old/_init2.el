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

(setq org-preview-latex-default-process 'dvisvgm)
(setq org-src-tab-acts-natively t)

;; explorador de archivos 
(use-package neotree
  :config
  (setq neo-theme 'ascii)
  (setq neo-smart-open t)
  (setq neo-window-width 35)
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
            (org-content)	
            (display-line-numbers-mode -1)))

;; Tipografia
;; (set-face-attribute 'default nil
;; ;;                    :font "Source Code Pro"
;; 		    :font "Ubuntu Mono Regular"		    
;;                     :height 98 )

;; Tema
;;(load-file "~/.emacs.d/themes/sea.el")
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

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
(display-time-mode -1)		     ;mostrar la hora
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
(global-set-key (kbd "C-c C-f") 'crux-recentf-find-file)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" default)))
 '(package-selected-packages
   (quote
    (htmlize exec-path-from-shell hl-todo crux magit rainbow-mode neotree ivy use-package)))
 '(safe-local-variable-values
   (quote
    ((org-html-postamble . "%t %a %d")
     (org-html-postamble . "%t %a %d %D")
     (org-html-postamble . "test"))))
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backups/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo ((t (:inherit hl-todo :italic t)))))

;; ---------------------------------------------------



;(setq frame-title-format "")

;(load-file "~/.emacs.d/themes/mate-flat-theme.el")

;; (setq frame-title-format
;;       '(buffer-file-name "%f"
;; 			 (dired-directory dired-directory "%b")))


;;https://www.reddit.com/r/emacs/comments/98prqr/how_would_i_make_a_keybinding_run_a_shell_command/
(defun run-buffer ()
  (interactive)
  (shell-command (concat "./eigenmath " buffer-file-name)))
(global-set-key (kbd "<f9>") 'run-buffer)

(setq eshell-prompt-function '(lambda () (concat (car (last (split-string (eshell/pwd) "/"))) " $ ")))

(use-package htmlize)

(setq org-html-postamble (concat
			  "Autor: %a <br>"
			  "Modificado: " (shell-command-to-string "echo -n $(date +%Y-%m-%d)") ))	 

(defun publish-dir-org ()
  "Publish all org files in a directory"
  (interactive)
  (save-excursion
    (mapc
     (lambda (file)
       (with-current-buffer
       (find-file-noselect file)
     (org-export-as-html-batch)))
       (file-expand-wildcards  "*.org"))))
