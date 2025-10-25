;;;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;

;;;;;; FONT SIZE
(set-face-attribute 'default nil :height 150)

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

;;;;;;;;;; YAFOLDING STUFF
(global-set-key (kbd "C-c z c") 'yafolding-hide-element)
(global-set-key (kbd "C-c z o") 'yafolding-show-element)
;;;;;;;;;;;;;;;;;;;;;;;;;

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

(require 'slime)

;; Rust Mode
(use-package rust-mode
  :ensure t)

;; Enable rustfmt on save
(add-hook 'rust-mode-hook
          (lambda () (add-hook 'before-save-hook 'rust-format-buffer nil 'local)))

;; LSP Mode for Rust and TS
(use-package lsp-mode
  :ensure t
  :hook ((rust-mode . lsp))
  :commands lsp)

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

;;;;;;;;;;;;;;;; TYPESCRIPT
(setq lsp-javascript-typescript-server-command '("typescript-language-server" "--stdio"))
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package prettier
  :hook ((typescript-mode . prettier-mode)
         (tsx-mode . prettier-mode)))
(add-hook 'before-save-hook 'lsp-format-buffer) ;; Format using LSP before saving
(add-hook 'before-save-hook 'lsp-organize-imports) ;; Organize imports before saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;; GOLANG
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(use-package go-mode
  :ensure t)

;; optional: only needed if Emacs can't find gopls automatically
;; (setq lsp-gopls-server-path "gopls")

(add-hook 'go-mode-hook #'lsp-deferred)

;; Turn on lsp-ui when lsp starts (you already do this globally, but safe)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; Format + organize imports on save (buffer-local so it only affects Go buffers)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;; CLOJURE
;; Clojure mode + CIDER
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;; must have clojure and clojure lsp
;; brew install clojure
;; brew install clojure-lsp/brew/clojure-lsp-native
(with-eval-after-load 'clojure-mode
  (require 'lsp-mode)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)
  (setq lsp-clojure-server-command '("clojure-lsp")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; RELATIVE LINE NUMBERS
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(global-hl-line-mode 1) ;; Enable global-hl-line-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-night))
 '(custom-safe-themes
   '("adfd86d3c3f4a1659e11d70106a55d1ed0aed6c0d5b382febf0aeceea88abc54" "144aa208033b570b4c31e054b77afa01b9e2349cdba14bb17c3e484c82effa30" "f019002925408f081e767c515e4fb4b1d7f1462228d6cd32ff66f06a43671527" "87b82caf3ade09282779733fb6de999d683caf4a67a1abbee8b8c8018a8d9a6b" "0013cec68d42e640266e700c09ea4eb55e18668f72da7a5b92f0c22b80581204" "0a953c81f5798aa99cafbc4aa8a56d16827442400028f6c1eab0c43061ea331c" "587ce9a1a961792114991fd488ef9c3fc37f165f6fea8b89d155640e81d165a3" "29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232" "9fba87dbc0f14d5650006893ed53088be71f16d57b749394d9c485ef2326e85f" "daf189a2af425e9f376ddb9e99627e9d8f2ebdd5cc795065da81633f88389b4b" "97283a649cf1ffd7be84dde08b45a41faa2a77c34a4832d3884c7f7bba53f3f5" "5e41864cbdd81b18d1fa62f09971a55a121a939238ca4c66faafcfcafb976c3e" "f8108bbb81e9bae9e4ed27b95e4a1507aa18ecc50193bff08e9b2cc2dcadbfbd" "a087e01778a85f8381b2aa2b7b0832951aea078621b38844b6c8c8d638d73e3b" "8a3d04fd24afde8333c1437a3ecaa616f121554041a4e7e48f21b28f13b50246" "0279c1b81b569e46a4ee8e87e5e381568bf931257287191fd091f2948f7e2e8e" "c3e62e14eb625e02e5aeb03d315180d5bb6627785e48f23ba35eb7b974a940af" "1fefcf9915617538b409d8aba3c6bbefddfcf2a80db09741aeef1457e1809c2b" "4f03e70554a58349740973c69e73aefd8ce761a77b22a9dc52a19e708532084a" "b93039071f490613499b76c237c2624ae67a9aafbc717da9b4d81f456344e56e" "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79" "dc96af3e6aaa9c96aa83d1a73a28a6d1dab58e376df1e51980b4fa9b256e9d7f" "aa04c854054e8d43245bd67ca619a7bede9171e2a2efb1b2c26caf1d031497eb" "01cad03be8c042a9941fda5a484280629ee2cc83fe084af6d19376c83141c91b" "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3" "40352d95bc42c2e3acb7fc75afb3029d81a76897e14e9438857729cc87630980" "2b36236a9d02c9041efa7749d49cfbf6f55bb0a6674fae43f78a81367490e535" "fb7595c9571f2bd41635745d12551f35322296b70330056ddd0020ab2374671c" "68126b97815568009c26e380545eec4fc7fc164d09c274c4dfbc4bccd865cb3e" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "1d2e7f3afdd436cf7b1f7d009111e9890328da1f68380c71ad8041ebd62a0a95" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "d1b46cf4414713c0901c3d77b640d857614b220e56c23f00c2fcfe5a2406b05a" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "063c278e83aa631e230535f1be093fa57d0df4a2f5b7e781c6952e6145532976" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" default))
 '(package-selected-packages
   '(ef-themes github-dark-vscode-theme gruber-darker-theme js3-mode typescript-mode vterm sly-quicklisp slime color-theme-sanityinc-tomorrow rustic yafolding flycheck-rust rust-mode atom-one-dark-theme flycheck lsp-ui lsp-mode multiple-cursors company-box company projectile counsel ivy evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
