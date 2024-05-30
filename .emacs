;;;;;;;;;;;;;;; CODE
;;;;;;;;;; for common lisp and repl, make life easier by syncing the repl with the code, C-c tilda

;;;;;;;;;;;;;;;;;;;;; DISABLE SSL/TLS VALIDATION IN EMACS
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; SET THE SSL CERT PATH
(setenv "SSL_CERT_FILE" "/System/Library/Keychains/SystemRootCertificates.keychain")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;; AUTO SAVE FILES
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
;;;;;;;;;;;;;;;;;;;;;;26

;; Go back to previous position
(global-set-key (kbd "C-S--") 'pop-global-mark)
;;;;;;;;;;;;;

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
;; (global-set-key (kbd "C-`") 'vterm)
;; (setq kill-buffer-query-functions
;;       (delq 'process-kill-buffer-query-function
;;             kill-buffer-query-functions))
(defun my/open-vterm-in-horizontal-window ()
  "Open a new horizontal window and launch vterm."
  (interactive)
  (let ((new-window (split-window-below)))
    (select-window new-window)
    (vterm)))

(global-set-key (kbd "C-`") 'my/open-vterm-in-horizontal-window)
;;;;;;;;;

;;;;;;;;; TREEMACS
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     ;; Other Treemacs configuration settings can go here if needed
;;     ))

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)
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

(projectile-register-project-type 'dart-pubspec '("pubspec.yaml")
                                  :compile "flutter pub get"
                                  :test "flutter test"
                                  :run "flutter run"
                                  :test-suffix "_test")

(projectile-register-project-type 'common-lisp '("*.asd")
                                  :compile "asdf:load-system"
                                  :test "asdf:test-system"
                                  :run "asdf:run"
                                  :test-suffix ".test")

;; (projectile-register-project-type 'python
;; 				  :directory-predicates (file-exists-p "pyproject.toml")
;; 				  :compile "pip install -r requirements.txt"
;; 				  :test "pytest"
;; 				  :run "python main.py"
;; 				  :test-suffix ".spec")

(require 'counsel)

;; Hook Treemacs with Projectile
;; (with-eval-after-load 'treemacs
;;   (defun my/treemacs-hook ()
;;     (require 'treemacs-projectile)
;;     (setq treemacs-space-between-root-nodes nil))
;;   (add-hook 'treemacs-mode-hook #'my/treemacs-hook))
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
 '(custom-enabled-themes '(autumn-light))
 '(custom-safe-themes
   '("77f1e155387d355fbbb3b382a28da41cc709b2a1cc71e7ede03ee5c1859468d2" "4ba6aa8a2776688ef7fbf3eb2b5addfd86d6e8516a701e69720b705d0fbe7f08" "f5a7e07642decb17b03483af7c44e93353d2b128de403bf301651954c628c0ab" "5fa49528e5d7369c4ef8f55c02fb5aa54048bfa4f9b9795e9b3df0ec35221ac6" "821c37a78c8ddf7d0e70f0a7ca44d96255da54e613aa82ff861fe5942d3f1efc" "6198e96f1fd7de3889a1b6ab8be1fc9b7c734cc9db0b0f16b635a2974601f977" "774218d0781ca9aad07888de412eac35b7920bafc10ecc014ecf493d7a74b310" "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827" "b5b6396361db4bee9b0c0d7ea678b96b3b55e4217c610038c8d289eb05c426ef" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a" "98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "3ee898efcd3fa5b63c4f15e225f3616497010f2347a514490be8b563edbd39d9" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "f13fa8e962b2ad938955650c449a8447769fc617f5d914552bff6b2ea7fec0bd" "6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1" "04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216" "2d70bca08b194d0becf19a1df2c54fcb78daeeebc880042de47c735a5c837af0" "1a6d120936f9df3f44953124dbf9e56b399e021702ca7d1844e6c5e1658b692b" "551629d1e63bb66423dd80b3ec2d1a67611d1fa570e7238201e65b25a3b3834f" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "9c22d1654fbfed0285a519541b5ea76462d9b03d778d1dc34779fd38613bf7c8" "f3781be0be23cc71c89b317489e07a4ad3e885f84c0a618692b53bbd69e60843" "0cf95236abcf59e05b1ea69b4edd53d293a5baec4fe4c3484543fee99bfd2204" "3db9833382bb7a23932e65a6e768a9be72998967aef95d31cee3226e609544c2" "09fcace336d09b96ef82209890c4503708c65f9d8ce020617327182ec559e6cd" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "c7737b9fc3471779c8e51ea0a37834d24aa80a0d6a79b215e7501227ada39855" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "062e6ec918ed89d5d9a342dbbefd99e8690c5514c6698a78fc25f259972e9242" "46e9b34ca8971629e5ad94694d7a3894b587d8a8fd7c6703fa2fd51d4317ac91" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "abccbf10aee9804f2e5fa8fc3480b271d1653b871dcda0259804ece106c11686" "f0b62710362b87292ebf855e2719fd5c6950cedf0a01334a63fb97532b4e519b" "df4acee173ac16f40b47f769cfe97333e5cd2d97779688bf13043a03c2bec2bc" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "bf7f4fb05a45eae1a6bc1a009b7731b09260d945ec4c3c4ed7f5da06647a7946" "d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "18624b2da7749af193a4eeaa7be1dc2abe94a97a8562ba69f5ee0f06d6dd156e" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "ce784eb8135893a19e5553ed94cc694320b05228ce712a64b2fb69c0c54161b9" "a1c18db2838b593fba371cb2623abd8f7644a7811ac53c6530eebdf8b9a25a8d" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "1bad38d6e4e7b2e6a59aef82e27639e7a1d8e8b06bbeac6730f3e492d4f5ba46" "8d412c0ed46b865312d6df5c1dfd1821d349dd3cba00049cf88c4ad34403597e" "841649342fc8ba89e9b50beb55829513afa742b4838cd52ce0ae852ad857b534" "12953be69a341359b933c552cc2c0127410c755349096018868d5c109283bfd8" "7403e5668f855dc751fe4360cb40f6b3dcd6b535d88db1c9027d2de6e178d6a0" "448d7e6f9639189b0196dd43047f3d8e018a28a9d3318e64eea35699d93a535d" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "ed5afe11def738a452af6d1070faaa98a3e32e5a22c179e4e7c4c40ffff93478" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "1711947b59ea934e396f616b81f8be8ab98e7d57ecab649a97632339db3a3d19" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(package-selected-packages
   '(kuronami-theme autumn-light-theme orangey-bits-theme naga-theme timu-caribbean-theme tron-legacy-theme nordic-night-theme company-jedi jedi elpy python-mode company-box cyberpunk-theme cape corfu-doc embark-consult consult embark marginalia vertico orderless corfu company company-go cider clojure-mode gruvbox-theme elixir-ts-mode flycheck-elixir elixir-mode distinguished-theme cybercafe-theme color-theme-sanityinc-tomorrow emacsql-sqlite espresso-theme parchment-theme leuven-theme moe-theme anti-zenburn-theme minimal-theme syntax-subword govet go-errcheck flymake-go-staticcheck flycheck-eglot eglot flymake-go go-mode jbeans-theme rg ripgrep yaml-mode apples-mode applescript-mode yafolding flymake-eslint flucui-themes flutter dart-server lsp-dart brutalist-theme slime-company paredit rainbow-delimiters slime sly horizon-theme spacegray-theme lsp-ui hc-zenburn-theme planet-theme afternoon-theme counsel-spotify subatomic-theme cyberpunk-2019-theme gruber-darker-theme nodejs-repl auto-save-buffers-enhanced tern-auto-complete tern challenger-deep-theme restclient magit flycheck-rust projectile flycheck rust-mode move-text w3m counsel ivy vterm lsp-mode solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
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


;; ;;;;;;;;;HIPPIE EXPAND
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; (yas-global-mode +1)
;; (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)

;; (use-package hippie-exp
;;   :bind ([remap dabbrev-expand] . hippie-expand)
;;   :commands (hippie-expand)
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
;;   (dabbrev-upcase-means-case-search t)
;;   :config
;;   (setopt hippie-expand-try-functions-list
;;           '(try-expand-all-abbrevs
;;             try-expand-dabbrev
;;             try-expand-dabbrev-all-buffers
;;             try-expand-dabbrev-from-kill
;;             try-complete-lisp-symbol-partially
;;             try-complete-lisp-symbol
;;             try-complete-file-name-partially
;;             try-complete-file-name)))

;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;; CODE COMPLETION USING CORFU

;; ;; Install Corfu and related packages

;; Add the Cape package
;; (use-package cape
;;   :ensure t
;;   :init
;;   ;; Add useful defaults completion sources
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   (add-to-list 'completion-at-point-functions #'cape-line)
;;   :config
;;   ;; Customize cape completion sources
;;   (setq cape-dabbrev-min-length 3) ; Minimum length for dabbrev completion
;;   (setq cape-keyword-min-length 3)) ; Minimum length for keyword completion


;; (use-package corfu
;;   :ensure t
;;   :init
;;   (setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
;;         corfu-auto t                 ; Enable auto completion
;;         corfu-auto-delay 0.2         ; Delay in seconds before auto completion starts
;;         corfu-auto-prefix 2          ; Number of characters before auto completion starts
;;         corfu-echo-documentation 0.25) ; Delay in seconds before documentation appears
;;   :config
;;   (global-corfu-mode))

;; ;; ;; ;; Enable Orderless for better completion style
;; (use-package orderless
;;   :ensure t
;;   :init
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

;; ;; ;; ;; Enable Vertico for enhanced minibuffer completion
;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode))

;; ;; ;; ;; Enable Marginalia for richer annotations in completion
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode))

;; ;; ;; ;; Enable Embark for actionable completions
;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command))

;; ;; ;; ;; Use Consult for advanced completion commands
;; (use-package consult
;;   :ensure t)


;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1))

;; ;; Ensure M-TAB is bound to completion-at-point
;; (global-set-key (kbd "M-TAB") #'completion-at-point)



;;;;;;;;;;;; Code Folding
(add-hook 'prog-mode-hook #'yafolding-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; MULTIPLE CURSORS
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;;;;;; non continuos lines
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq rust-format-on-save t)
;; Format Rust code on save

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
  :hook (lisp-mode . paredit-mode))
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

(require 'yaml-mode)

(defun start-dart-server ()
  "Start the Dart server."
  (interactive)
  (start-process "dart-server" nil "dart" "server"))

(add-hook 'dart-mode-hook 'start-dart-server)


;;;;;;;;; restart lsp after flutter pub get
;; (defun my-flutter-pub-get-and-refresh ()
;;   "Run `flutter pub get` and refresh the Dart project in Emacs."
;;   (interactive)
;;   (async-shell-command "flutter pub get" "*Flutter Pub Get*")
;;   (when (derived-mode-p 'dart-mode)
;;     ;; Trigger project refresh or reload in Dart mode
;;     ;; Replace this with appropriate command based on your setup
;;     (lsp-workspace-restart 'dart-server)))

;; ;; Bind the function to a keybinding
;; (global-set-key (kbd "C-c C-f") 'my-flutter-pub-get-and-refresh)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; GOLANG STUFF
;; Configure Go-specific settings for lsp-mode
(add-to-list 'lsp-language-id-configuration '(go-mode . "go"))
(setq lsp-go-server "gopls") ; Use gopls as the LSP server for Go
(setq lsp-gopls-staticcheck t) ; Enable staticcheck in gopls
(setq lsp-gopls-complete-unimported t) ; Automatically complete unimported packages

;; Configure Go-specific settings for projectile
(add-to-list 'projectile-project-root-files "go.mod") ; Recognize go.mod files as project roots

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (go-mode . lsp-ui-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; SQLITE
(require 'emacsql-sqlite)
;;;;;;;;;;;;;;;;;;;;;

;;;;;; STUFF FOR ELIXIR
;; Load Elixir mode
(require 'elixir-mode)

;; Enable Flycheck and flycheck-elixir in Elixir mode
(add-hook 'elixir-mode-hook
          (lambda ()
            (flycheck-mode)
            (flycheck-elixir-setup)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (elixir-mode . lsp-ui-mode))
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; CLOJURE
(add-to-list 'auto-mode-alist '("\.clj$" . clojure-mode))
(defun my-clojure-mode-hook ()
  (cider-mode 1))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
;;;;;;;;;;;;;;;;

;;;;;;;;;;;; PYTHON STUFF
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(elpy-enable)

(setq python-executable-path "/opt/homebrew/bin/python3")

(add-hook 'after-init-hook'
(lambda ()
(company-mode t)
(add-to-list 'company-backends 'company-jedi)))
;;;;;;;;;;;;;;;;;;;;;;;;
