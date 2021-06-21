;; Tipografia
(set-face-attribute 'default nil
		    :font "Fira Code"
		    :height 98 )

;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'sea-love t)

;;(load-theme 'wombat)
;;(load-theme sea)
(load-file "~/.emacs.d/themes/sea.el")
;; (load-theme 'soft-charcoal t)

;; (custom-set-faces
;;  '(neo-root-dir-face ((t (:foreground "#8D8D84"))))
;;  '(neo-dir-link-face ((t (:foreground "#0000FF"))))
;;  '(neo-file-link-face ((t (:foreground "#BA36A5")))))

;; otros
;;(set-face-background 'fringe "#ff0000")
(setq inhibit-startup-message t);Pantalla de inicio de emacs 
(global-display-line-numbers-mode t);numeros de linea
(scroll-bar-mode -1);scroll bars visibles
(tool-bar-mode -1);barra de herramientas visisles
(menu-bar-mode -1);menu de herramientas visible
(set-fringe-mode 10) ;espacio entre el frame y el buffer
(global-visual-line-mode 1);separar lineas 
(setq-default cursor-type 'bar);tipo del cursor
(setq-default indent-tabs-mode nil)
(setq column-number-mode t);numero de columna 
(line-number-mode t);numero de fila
(display-time-mode 1);mostrar la hora
(display-battery-mode -1);mostrar batteria


;;(set-frame-parameter (selected-frame) 'undecorated t) ;frame visible
;;(set-frame-parameter (selected-frame) 'alpha '(95 95)) ;fondo trasparente
;;(add-to-list 'default-frame-alist '(alpha 85 85)) ;transparencia del borde
                                                                          
(global-set-key (kbd "C-x t") 'eshell)                                    
(global-set-key (kbd "C-x j") 'neotree-toggle)                            
(global-set-key (kbd "C-x <") 'ido-switch-buffer)                         
(global-set-key (kbd "C-M-z") 'toggle-80-editting-columns-balanced)       
                                                                          
;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(require 'recentf)
(recentf-mode 1)

(use-package ivy
  :config
  (ivy-mode 1))			     ;Activar ivy en todos los buffers

(use-package neotree
  :config
  (setq neo-theme 'ascii)
  (setq neo-smart-open t)
  (setq neo-window-width 25)
  (setq neo-window-fixed-size -1))

(add-hook 'neo-after-create-hook
	  (lambda (&rest _) 
	    (display-line-numbers-mode -1)
	    (visual-line-mode -1)))

(use-package rainbow-mode)

(use-package magit)

(use-package crux)
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
  ;; :hook
  ;; (org-mode-hook . (lambda ()
  ;; 		     (display-line-numbers-mode -1)
  ;; 		     (org-indent-mode t)
  ;; 		     (org-content 2)))
  :config
  (setq org-support-shift-select t)
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 1.5)))

;; (require 'org)
;; (define-key org-mode-map (kbd "<M-return>") 'org-toggle-latex-fragment)

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; (setq org-support-shift-select t) 
	    ;; (setq org-format-latex-options
	    ;; 	  (plist-put org-format-latex-options :scale 1.5)) 

	    (org-indent-mode t)
	    (setq org-content 2)
	    ;; (org-reload)
	    (display-line-numbers-mode -1)

	    )
	  )

(add-hook 'eshell-mode-hook
	  (lambda (&rest _) 
	    (display-line-numbers-mode -1)
	    (visual-line-mode -1)))

;; (defun kill-other-buffers ()
;;   "kill all other buffers."
;;   (interactive)
;;   (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; (defun insert-current-date () (interactive)
;;        (insert (shell-command-to-string "echo -n $(date +%y-%m-%d)")))

(defun toggle-80-editting-columns ()
  "set the right window margin so the edittable space is only 80 columns."
  (interactive)
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (set-window-margins nil 0 (max (- (window-width) 80) 0)))))

(defun toggle-80-editting-columns-balanced ()
  "set both window margins so the edittable space is only 80 columns."
  (interactive)
  (let ((margins (window-margins)))
    (if (or (car margins) (cdr margins))
        (set-window-margins nil 0 0)
      (let* ((change (max (- (window-width) 80) 0))
             (left (/ change 2))
             (right (- change left)))
        (set-window-margins nil left right)))))

;;(desktop-save-mode 1)			;guardar escritorio
;;(find-file "~/notes.org")	                ;abrir archivo al iniciar

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

(custom-set-variables
 ;; custom-set-variables was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 '(package-selected-packages (quote (crux use-package rainbow-mode neotree magit ivy))))
(custom-set-faces
 ;; custom-set-faces was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
