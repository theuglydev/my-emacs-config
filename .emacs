;;;;;;;;;;;;;;;;;;;;;APPLESSCRIPT
(use-package apples-mode
  :ensure t
  :mode (("\\.applescript\\'" . apples-mode)
         ("\\.scpt\\'" . apples-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;; AUTO SAVE FILES
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; DISABLE BACKUPS
;; Disable creation of backup files
(setq make-backup-files nil)


;; Set backup directory to nil
(setq backup-directory-alist nil)
;;;;;;;;;;;;;;

;;;;; REMOVE BACKUP FILES IN CASE CREATED
(defun remove-backup-file ()
  "Remove backup file immediately after it is created."
  (when (and buffer-file-name
             (file-exists-p (concat buffer-file-name "~")))
    (delete-file (concat buffer-file-name "~"))))

(add-hook 'after-save-hook 'remove-backup-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;

;;;;;;;;; VETERM
(require 'vterm)
;; Set the default directory for vterm buffers
(setq vterm-shell "zsh")  ;; Set your preferred shell
;; Optional keybinding to toggle vterm
(global-set-key (kbd "C-`") 'vterm)
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;;;;;;;;;

;;;;;;;;; TREEMACS
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    ;; Other Treemacs configuration settings can go here if needed
    ))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
;;;;;;;;;;;;;;;;;;

;;;;;;; IVY and COUNSEL
(require 'ivy)
(require 'projectile)

;; Enable Ivy globally
(ivy-mode 1)

;; Enable Projectile globally
(projectile-mode 1)

;; Set Ivy as the completion system for Projectile
(setq projectile-completion-system 'ivy)
;; Define a JavaScript project type
(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test"
                                  :run "npm start"
                                  :test-suffix ".spec")

(require 'counsel)

;; Hook Treemacs with Projectile
(with-eval-after-load 'treemacs
  (defun my/treemacs-hook ()
    (require 'treemacs-projectile)
    (setq treemacs-space-between-root-nodes nil))
  (add-hook 'treemacs-mode-hook #'my/treemacs-hook))
;; Configure key bindings
(global-set-key (kbd "C-s") 'swiper) ;; Use `swiper` for buffer search
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Use `counsel-M-x` for command execution
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; Use `counsel-find-file` for file opening
;;;;;;;;;;;

;; ;;;;;;;;;;;;;;; CUSTOM THEMES SECTION
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; (load-theme 'masked-theme t)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kaolin-bubblegum))
 '(custom-safe-themes
   '("6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "abccbf10aee9804f2e5fa8fc3480b271d1653b871dcda0259804ece106c11686" "f0b62710362b87292ebf855e2719fd5c6950cedf0a01334a63fb97532b4e519b" "df4acee173ac16f40b47f769cfe97333e5cd2d97779688bf13043a03c2bec2bc" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "bf7f4fb05a45eae1a6bc1a009b7731b09260d945ec4c3c4ed7f5da06647a7946" "d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "18624b2da7749af193a4eeaa7be1dc2abe94a97a8562ba69f5ee0f06d6dd156e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "ce784eb8135893a19e5553ed94cc694320b05228ce712a64b2fb69c0c54161b9" "a1c18db2838b593fba371cb2623abd8f7644a7811ac53c6530eebdf8b9a25a8d" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "1bad38d6e4e7b2e6a59aef82e27639e7a1d8e8b06bbeac6730f3e492d4f5ba46" "8d412c0ed46b865312d6df5c1dfd1821d349dd3cba00049cf88c4ad34403597e" "841649342fc8ba89e9b50beb55829513afa742b4838cd52ce0ae852ad857b534" "12953be69a341359b933c552cc2c0127410c755349096018868d5c109283bfd8" "7403e5668f855dc751fe4360cb40f6b3dcd6b535d88db1c9027d2de6e178d6a0" "448d7e6f9639189b0196dd43047f3d8e018a28a9d3318e64eea35699d93a535d" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "ed5afe11def738a452af6d1070faaa98a3e32e5a22c179e4e7c4c40ffff93478" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "1711947b59ea934e396f616b81f8be8ab98e7d57ecab649a97632339db3a3d19" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(package-selected-packages
   '(apples-mode applescript-mode yafolding flymake-eslint treemacs-projectile flucui-themes flutter dart-server lsp-dart kaolin-themes brutalist-theme slime-company paredit rainbow-delimiters slime sly horizon-theme spacegray-theme lsp-ui hc-zenburn-theme planet-theme afternoon-theme counsel-spotify subatomic-theme cyberpunk-2019-theme gruber-darker-theme nodejs-repl auto-save-buffers-enhanced tern-auto-complete tern challenger-deep-theme restclient magit flycheck-rust projectile flycheck rust-mode move-text w3m counsel ivy vterm lsp-mode solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;;;;;;;;;;;;;; CUSTOM KEYBINDINGS
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-x p f") 'projectile-find-file)

;;;;;;; MAGIT SATATUS
;; Set keybinding for Magit status
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; PROGRAMMING STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Code Folding
(add-hook 'prog-mode-hook #'yafolding-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; RELATIVE LINE NUMBERS
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(global-hl-line-mode 1) ;; Enable global-hl-line-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; RUST

;;;;;;;;;;;;FIND RSL in my DIR
(setq exec-path(append exec-path '("/Users/khaledtahboub/.rustup/toolchains/stable-aarch64-apple-darwin")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install lsp-mode and rust-mode if not already installed
(unless (package-installed-p 'lsp-mode)
  (package-refresh-contents)
  (package-install 'lsp-mode))

(unless (package-installed-p 'rust-mode)
  (package-refresh-contents)
  (package-install 'rust-mode))

;; Configure lsp-mode for Rust
(require 'lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(add-hook 'rust-mode-hook #'lsp-deferred)

;;;;;;; Adding LSP WITH JS AS WELL HERE
(add-hook 'js-mode-hook #'lsp-deferred)
(defun my-setup-flymake-eslint ()
  "Configure flymake-eslint for JavaScript files."
  (when (executable-find "eslint")
    (flymake-eslint-enable)))

(add-hook 'js-mode-hook #'my-setup-flymake-eslint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tern
  :ensure t)

(use-package tern-auto-complete
  :ensure t
  :config
  (tern-ac-setup))

;; Enable Tern in js-mode
(add-hook 'js-mode-hook (lambda () (if (executable-find "tern") (tern-mode t))))

;; Optionally, enable auto-complete-mode in js-mode
(add-hook 'js-mode-hook (lambda () (if (functionp 'auto-complete-mode) (auto-complete-mode t))))

;; nodejs debugger
(require 'nodejs-repl)
(add-hook 'js-mode-hook #'nodejs-repl-minor-mode)
;;;;;;;;;;;;;;;;;;

;; Set up rust-mode
(require 'rust-mode)
(setq rust-format-on-save t) ;; Format Rust code on save

;; Configure flycheck for Rust
(require 'flycheck)
(require 'flycheck-rust)

;; Enable Flycheck globally
(global-flycheck-mode)

;; Configure Rust support for Flycheck
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; PROGRAMMING STUFF FOR LISP
(defun my-sly-lsp-setup ()
  "Enable LSP mode when entering SLY mode."
  (lsp))

(add-hook 'sly-mode-hook 'my-sly-lsp-setup)

(use-package slime
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  (slime-setup))

(add-hook 'slime-mode-hook #'slime-company)

(use-package rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode))

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

(add-hook 'lisp-mode-hook #'lsp-ui-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; FLUTTER AREA

;; Enable lsp-dart
(setq lsp-dart-sdk-dir "/opt/homebrew/Cellar/dart-sdk/3.3.3/libexec")
;; (setq lsp-dart-analysis-server-experimental-protocol-enabled "2.9")
;; (setq exec-path (cons "/Users/khaledtahboub/fvm/default/bin/" exec-path))
;; (setq flutter-sdk-path "/Users/khaledtahboub/fvm/default/bin/flutter")

(use-package dart-mode
  ;; Optional
  :hook (dart-mode . flutter-run))

(add-hook 'dart-mode-hook #'lsp)

;; (use-package flutter
;;   :after dart-mode
;;   :bind (:map dart-mode-map
;;               ("C-M-x" . #'flutter-run-or-hot-reload))
;;   :custom
;;   (flutter-sdk-path "/Users/khaledtahboub/fvm/default/bin"))


(defun run-ios-simulator ()
  "Launch iOS simulator."
  (interactive)
  (shell-command "open -a Simulator"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
