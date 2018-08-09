;; ------------------------------------------------------------------------
;; @ load-path 

;; load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;; load environment value//
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))
;;-------------------------------------------------------------------------
;; @ フレーム
  (setq default-frame-alist
        (append '(
;;                  (top                  . 28     ) ;; 配置上位置
;;                  (left                 . 70     ) ;; 配置左位置
;;                  (width                . 94     ) ;; フレーム幅
;;                  (height               . 27     ) ;; フレーム高
;;                  (line-spacing         . 0      ) ;; 文字間隔
                  (left-fringe          . 1      ) ;; 左フリンジ幅
                  (right-fringe         . 0      ) ;; 右フリンジ幅
;;                  (menu-bar-lines       . nil    ) ;; メニューバー
;;                  (tool-bar-lines       . nil    ) ;; ツールバー
;;                  (vertical-scroll-bars . nil    ) ;; スクロールバー
                  (cursor-type          . box    ) ;; カーソル種別
;;                  (foreground-color     . "white") ;; 文字色
;;                  (background-color     . "black") ;; 背景色
;;                  (cursor-color         . "red"  ) ;; カーソル色
                  ) default-frame-alist) )
  (setq initial-frame-alist default-frame-alist)
;; ------------------------------------------------------------------------
;; @ general

;; M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; package install
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; common lisp
(require 'cl)

(setq inhibit-startup-screen t)

(setq initial-scratch-message "")

(tool-bar-mode -1)

(global-linum-mode t)

;;左の行数
(set-face-attribute 'linum nil
                    :foreground "#008"
                    :background "LightSteelBlue1"
                    :height 0.9)
(setq linum-format "%4d")

(set-frame-parameter (selected-frame) 'alpha '(100))

(line-number-mode t)

(column-number-mode t)

;;(set-face-background 'show-paren-match-face "#500")

;;(set-face-background 'region "#555")

;;行のハイライト hi-light line
;(global-hl-line-mode t);;
;(custom-set-faces
;'(hl-line ((t (:background "#fff0f5")))) ;;
;)

;;
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression);; highlight entire bracket expression
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#0000ff");; :weight 'extra-bold)

;; 
(setq-default indent-tabs-mode nil)

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;
(setq scroll-step 1)

;; 
(defun scroll-down-with-lines ()
  ""
  (interactive)
  (scroll-down 2)
  )
(defun scroll-up-with-lines ()
  ""
  (interactive)
  (scroll-up 2)
  )
;;
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)

;;
(global-set-key "\C-z" 'undo)

;(setq-default tab-width 4)
(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4)
  )
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)

;;window move
(windmove-default-keybindings 'super) ; Mac

;; ------------------------------------------------------------------------
;; @ initial frame maximize

;; http://www.emacswiki.org/emacs/FullScreen#toc12
(defun jbr-init ()
  "Called from term-setup-hook after the default
   terminal setup is
   done or directly from startup if term-setup-hook not
   used.  The value
   0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar))

(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBook Air(Late2010) 11inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 200 60))))

;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

;; font setting
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 120)    ;; font size

(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
 (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))

;; scroll line to 1
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

(electric-pair-mode 1) ;;complete parens

;;背景色の変更 change background clor
(custom-set-faces
 '(default ((t
             (:background "white" :foreground "black");;fffdfd
             )));;背景色の変更
 '(mode-line ((t
               (:background "CornflowerBlue" :foreground "black");;gray75 black
               )));;1モードラインの文字, 背景の設定
 '(mode-line-inactive ((t
                        (:background "LightBlue3" :foreground "Blue");;gray75 black
                        )));;1アクティブでないバッファのモードラインの設定
 '(fringe ((t
            (:background "LightSteelBlue1" :foreground "red");;gray75 black
            )));;(フレーム左右の余白部分,行番号との境界)の色
 )

;; 全角スペースやタブ文字、行末のスペースを色を付けて表示する
(global-font-lock-mode t)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode 
   '(("¥t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ ¥t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;--------------------------dired---------------------------------
;;; フォルダを開く時, 新しいバッファを作成しない
;; バッファを作成したい時にはoやC-u ^を利用する
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
  (before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-advertised-find-file
  (after kill-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

(defadvice dired-up-directory
  (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
  (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

;;dired の RET を同じバッファでディレクトリを開くように変更
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;;------------------------PowerLine---------------------------
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/powerline-20180322.248"))
(require 'powerline)

(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq powerline-current-separator 'utf-8)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                          (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-id nil )
                                     (powerline-raw " [ ")
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-raw "%*")
                                     (powerline-raw " |")
                                     (powerline-process nil)
                                     (powerline-vc)
                                     (powerline-raw " ]")
                                     ))
                          (rhs (list (powerline-raw "%4l")
                                     (powerline-raw ":")
                                     (powerline-raw "%2c")
                                     (powerline-raw " | ")
                                     (powerline-raw "%6p")
                                     (powerline-raw " ")
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill nil (powerline-width rhs))
                             (powerline-render rhs)))))))

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))
(make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
(make/set-face 'mode-line-2-fg "red" "LightSalmon1" 'bold)
(make/set-face 'mode-line-1-arrow  "black" "CornflowerBlue" 'bold)
(make/set-face 'mode-line-2-arrow  "black" "CornflowerBlue" 'bold)

(powerline-my-theme)

;;-------------------------C-Ret-------------------------------
;; kbd：http://dev.ariel-networks.com/articles/emacs/part5/ 矩形選択の設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; -----------------------autocomplete-------------------------
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20170124.1845"))
(require 'auto-complete)
(require 'auto-complete-config)

;;-------------------------flycheck----------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/flycheck/flycheck-20180422.2106")
(add-to-list 'load-path "~/.emacs.d/elpa/flycheck/dash-20180413.30")
(add-to-list 'load-path "~/.emacs.d/elpa/flycheck/seq-2.20")

(require 'flycheck)

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;---------------------yasnipetto----------------------------
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/yasnippet-20180412.1548"))
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        ;;yas-installed-snippets-dir 
        "~/.emacs.d/yasnippets"
        ))
(yas-global-mode 1)

;; ----------------------tabber------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/tabbar-20160524.1401")

(require 'tabbar)
(tabbar-mode 1)

(tabbar-mwheel-mode nil)                  ;; mouse wheel to nil
(setq tabbar-buffer-groups-function nil)  ;; gourp nil
(setq tabbar-use-images nil)              ;; image nil

;; kbd for tab changing
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
(global-set-key (kbd "<M-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<M-left>") 'tabbar-backward-tab)

;;modeに上書きされないkbd設定、複数設定できない
;(define-minor-mode overriding-minor-mode
;  "強制的にM-rightを割り当てる"             ;説明文字列
;  t                                     ;デフォルトで有効にする
;  ""                                    ;モードラインに表示しない
;  `((,(kbd "<M-right>") . tabbar-forwardward-tab)))

; hide left botton
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
(set btn (cons (cons "" nil)
                 (cons "" nil))))

; length of tab separator
(setq tabbar-separator '(2.0))

;; tab color
(set-face-attribute
 'tabbar-default nil;; default
 :family "Menlo"
 :background "SteelBlue"
 :foreground "white"
 :height 1)
(set-face-attribute;; sellected
 'tabbar-selected nil
 :background "white"
 :foreground "SteelBlue"
 ;:weight 'bold
 :box nil)
(set-face-attribute
 'tabbar-modified nil;; modified
 :background "IndianRed"
 :foreground "white"
 :box nil)
(set-face-attribute ;; unsellected
 'tabbar-unselected nil
 :background "SteelBlue"
 :foreground "white"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)

(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
		     ((equal "*scratch*" (buffer-name b)) b) 
                                        ; display *scratch* buffer
		     ((equal "*shell*" (buffer-name b)) b) 
                                        ; display *shell* buffer
		     ((equal "*eshell*" (buffer-name b)) b) 
                                        ; display *eshell* buffer
		     ((char-equal ?* (aref (buffer-name b) 0)) nil) 
                                        ; not display other buffer start with *
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;;-----------------------neotree----------------------------

(add-to-list'load-path "~/.emacs.d/elpa/neotree-20170522.758")
(require 'neotree)
(setq neo-show-hidden-files t)
(setq neo-persist-show t);;not close neotree by delete-other-window
(setq neo-create-file-auto-open t)


;; ----------------------Haskell-----------------------------

(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.8.0")

(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;#!/usr/bin/env runghc 
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;#!/usr/bin/env runhaskell

;; ---------------------------go----------------------------------

;; go-mode
(add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20180327.830")

;;
(add-to-list 'exec-path (expand-file-name "/usr/local/bin/go"))
;; 
(add-to-list 'exec-path (expand-file-name "/Users/hamakei/.go/bin/gocode"))
;;
(add-to-list 'exec-path (expand-file-name "/Users/hamakei/dev/go/bin/"))

;; go-autocomplete
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/go-autocomplete-20170907"))
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; go-eldoc
(add-to-list 'load-path "~/.emacs.d/elpa/go-eldoc-20170305.627")
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)

; go-mode
(autoload 'go-mode "go-mode" nil t)
(eval-after-load "go-mode" '(progn (require 'go-autocomplete)))
(add-hook 'go-mode-hook (lambda()
       (add-hook 'before-save-hook 'gofmt-before-save) 
       (local-set-key (kbd "M-.") 'godef-jump)
       (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
       (local-set-key (kbd "C-c i") 'go-goto-imports)
       (local-set-key (kbd "C-c d") 'godoc)
       (setq indent-tabs-mode nil)  
       (setq c-basic-offset 4) 
       (setq tab-width 4)
       (setq gofmt-command "goimports")
       (setq indent-tabs-mode t)))
       ; (define-key ac-mode-map (kbd "M-tab") 'auto-complete)))

;; go-mode
;; In advance, install godef, gocode, and goimports in $GOPATH.
;;   go get code.google.com/p/rog-go/exp/cmd/godef
;;   go get -u github.com/nsf/gocode
;;   go get code.google.com/p/go.tools/cmd/goimports
;; exec-path should include a path to .go/bin.
;; So DO NOT launch an emacs from App icon, DO launch an emacs from terminal (shell).

;; files path by go get
(add-to-list 'exec-path (expand-file-name "~/dev/go-workspace/bin"))

;; files path by homebrew
(add-to-list 'exec-path (expand-file-name "~/homebrew/bin"))

;; ---------------------------python mode-------------------------------

;;(add-to-list 'load-path "/Users/hamakei/.emacs.d/elpa/python-mode-20180319.344/")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(put 'dired-find-alternate-file 'disabled nil)


;; ----------------------------org mode----------------------------------

;; org-modeの初期化
(require 'org-install)
;; キーバインドの設定
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
;;(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/prog/ORG/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; アジェンダの表示用
(setq org-agenda-files '("~/prog/ORG/TODO/"))

;;
;; Disabling key bindings
;; M-right/left が tabber.el の設定と被ため
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map (kbd "<M-left>") nil)
     ))

;; ---------------------------- helm -----------------------------------

(add-to-list'load-path "~/emacs.d/elpa/helm-20180616.208")
(add-to-list'load-path "~/emacs.d/elpa/helm-core-20180616.208")

(require 'helm)
(require 'helm-config)
;;(require 'helm-migemo)
(require 'helm-mode)

;; ;;; 既存のコマンドを Helm インターフェイスに置き換える
(helm-mode 1)

;;(require 'helm-descbinds)

;;; キー設定
(global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "M-j") 'helm-select-3rd-action)
(define-key helm-map (kbd "C-;") 'anything-keyboard-quit)

;;; 自動補完を無効
(custom-set-variables '(helm-ff-auto-update-initial-value nil))
;;; helm-mode で無効にしたいコマンド
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil))

;;; 一度に表示する最大候補数を増やす
(setq helm-candidate-number-limit 99999)

