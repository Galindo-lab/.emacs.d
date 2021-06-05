;; Tipografia 
  (set-face-attribute 'default nil
    :font "Source Code Pro" 	;nombre de la tipografia
    :height 98			;tamaño
  ) 	

  ;; Tema
  ;; TODO: crear una funcion para automatizar esto
  ;(load-theme 'wombat)
;  (load-file "~/.emacs.d/aaaaa/sea.el")
  (load-file "./themes/sea.el")

  ;; otros
  (setq inhibit-startup-message t)  ;Pantalla de inicio de emacs 
  (global-display-line-numbers-mode t)	;numeros de linea
  (scroll-bar-mode -1)		;scroll bars visibles
  (tool-bar-mode -1)		;barra de herramientas visisles
  (menu-bar-mode -1)		;menu de herramientas visible
  (set-fringe-mode 10)		;espacio entre el frame y el buffer
  ;(set-face-background 'fringe "#ff0000")
  (global-visual-line-mode 1)	;separar lineas 
  (setq-default cursor-type 'bar)

(setq column-number-mode t)	;numero de columna 
(line-number-mode t)		;numero de fila
(display-time-mode 1)		;mostrar la hora
(display-battery-mode -1)	        ;mostrar batteria

;(set-frame-parameter (selected-frame) 'undecorated t) ;frame visible
(set-frame-parameter (selected-frame) 'alpha '(95 95)) ;fondo trasparente
;(add-to-list 'default-frame-alist '(alpha 85 85)) ;transparencia del borde

(global-set-key (kbd "C-x e") 'eshell)
(global-set-key (kbd "C-x j") 'neotree-toggle)
(global-set-key (kbd "C-x <") 'ido-switch-buffer)
(global-set-key (kbd "C-M-z") 'toggle-80-editting-columns-balanced)

;; Make ESC quit prompts
;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;(add-to-list 'load-path "~/.emacs.d/aaaaa/")
(add-to-list 'load-path "./themes/")

(ivy-mode 1)			;Activar ivy en todos los buffers
(use-package ivy
  :diminish
  :bind (
  :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
  :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
  :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill)
  )
  :config)

(add-hook 'neo-after-create-hook
  (lambda (&rest _) 
    (display-line-numbers-mode -1)
    (visual-line-mode -1)
  ))

(use-package neotree
 :ensure t)
(setq neo-theme 'ascii)
(setq neo-smart-open t)
(setq neo-window-width 25)
(setq neo-window-fixed-size -1)

;; (custom-set-faces
;;  '(neo-root-dir-face ((t (:foreground "#8D8D84"))))
;;  '(neo-dir-link-face ((t (:foreground "#0000FF"))))
;;  '(neo-file-link-face ((t (:foreground "#BA36A5")))))

(use-package rainbow-mode
:ensure t)

(use-package magit
 :ensure t)

;; (doom-modeline 1)
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

;(require 'zen-mode)
;(global-set-key (kbd "C-M-z") 'zen-mode)

(add-hook 'org-mode-hook
  (lambda ()
    (setq org-startup-indented t)
    (setq org-support-shift-select t)
    (setq org-content 2)
    (setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5)) ;latex-preview size

    (define-key org-mode-map (kbd "<C-return>") ;preview latex
      'org-preview-latex-fragment))

    (org-reload)
  )

(add-hook 'eshell-mode-hook
  (lambda (&rest _) 
    (display-line-numbers-mode -1)
    (visual-line-mode -1)
))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

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

;(desktop-save-mode 1)			;guardar escritorio
;(find-file "~/notes.org")	                ;abrir archivo al iniciar
