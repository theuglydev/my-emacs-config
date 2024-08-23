;;;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;

;;;; NO BACK UP FILES
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;; FORCE FULL SCREEN
;; Define a function to toggle fullscreen mode
(defun toggle-fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))

;; Bind a key to toggle fullscreen mode (optional)
(global-set-key (kbd "M-RET") 'toggle-fullscreen)

;; Automatically enter fullscreen mode on startup (optional)
(add-hook 'after-init-hook 'toggle-fullscreen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; OVERRIDING DEFAULT WELCOME SCREEN
(setq initial-buffer-choice
      (lambda ()
	(if (buffer-file-name)
	    (current-buffer) ;; leave as-is
	  (find-file "~/.emacs"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; remove all bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; SWITCH FOCUS ON NEW SPLITS
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; Switch windows positions
(defun transpose-windows ()
  "Transpose the positions of two windows."
  (interactive)
  (let ((this-buffer (window-buffer))
        (this-point (window-point)))
    (other-window 1)
    (let ((other-buffer (window-buffer))
          (other-point (window-point)))
      (switch-to-buffer this-buffer)
      (set-window-point (selected-window) other-point)
      (other-window -1)
      (switch-to-buffer other-buffer)
      (set-window-point (selected-window) this-point))))

(global-set-key (kbd "C-x 4 t") 'transpose-windows)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Change Vertical Splits to Horizontal Splits
(defun my-toggle-window-split ()
  "Toggle between horizontal and vertical window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                          (car next-win-edges))
                                      (<= (cadr this-win-edges)
                                          (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "Only works with exactly 2 windows")))

(global-set-key (kbd "C-x 5 t") 'my-toggle-window-split)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;; evil mode
(evil-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; PROJECTILE, IVY, COUNSEL
;;;;;;; IVY and COUNSEL
(require 'ivy)
(require 'projectile)

;; Enable Ivy globally
(ivy-mode 1)

;; Enable Projectile globally
(projectile-mode 1)

;; Set Ivy as the completion system for Projectile
(setq projectile-completion-system 'ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; CODE COMPLETION USING COMPANY
;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Customize company mode appearance
(setq company-tooltip-align-annotations t)   
(setq company-tooltip-flip-when-above t)     
(setq company-minimum-prefix-length 1)       
(setq company-idle-delay 0.0)                

;; Optional: use company-box for a more visually appealing completion interface 
(when (package-installed-p 'company-box)
  (add-hook 'company-mode-hook 'company-box-mode))

(unless (package-installed-p 'company-box)
  (package-refresh-contents)
  (package-install 'company-box))

(add-hook 'company-mode-hook 'company-box-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; MULTIPLE CURSORS
(require 'multiple-cursors)

(global-set-key (kbd "C-L") 'mc/mark-all-like-this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; PROGRAMMING STUFF
;; (use-package lsp-mode
;;   :ensure t)
;; (use-package lsp-ui
;;   :ensure t)
;; (use-package flycheck
;;   :ensure t)
;; (setq global-flycheck-mode 1)

(require 'yafolding)

;; Rust Mode
(use-package rust-mode
  :ensure t)

;; LSP Mode for Rust
(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp))

;; Optional: LSP UI for better integration
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Optional: Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Optional: Flycheck integration with Rust
(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;;;;;;;; RELATIVE LINE NUMBERS
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (setq display-line-numbers-type 'relative)

(global-hl-line-mode 1) ;; Enable global-hl-line-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
