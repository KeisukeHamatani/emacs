;; ------------------------------------------------------------------------
;; @ load-path 

;; load-path¤ÎÄÉ²Ã´Ø¿ô
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-path¤ËÄÉ²Ã¤¹¤ë¥Õ¥©¥ë¥À
;; 2¤Ä°Ê¾å¥Õ¥©¥ë¥À¤ò»ØÄê¤¹¤ë¾ì¹ç¤Î°ú¿ô => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;; load environment value//
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; ------------------------------------------------------------------------
;; @ general

;; 透明度を変更するコマンド M-x set-alpha
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

(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)

(set-frame-parameter (selected-frame) 'alpha '(1.00))

;; ¥â¡¼¥É¥é¥¤¥ó¤Ë¹ÔÈÖ¹æÉ½¼¨
(line-number-mode t)

;; ¥â¡¼¥É¥é¥¤¥ó¤ËÎóÈÖ¹æÉ½¼¨
(column-number-mode t)

;; ³ç¸Ì¤ÎÈÏ°Ï¿§
;;(set-face-background 'show-paren-match-face "#500")

;; ÁªÂòÎÎ°è¤Î¿§
;;(set-face-background 'region "#555")

(global-hl-line-mode t);; ¸½ºß¹Ô¤ò¥Ï¥¤¥é¥¤¥È
(custom-set-faces
'(hl-line ((t (:background "#f5f5f5")))) ;;
)

;; ³ç¸Ì¤ÎÈÏ°ÏÆâ¤ò¶¯Ä´É½¼¨
(show-paren-mode t);; ÂÐ±þ¤¹¤ë³ç¸Ì¤ò¥Ï¥¤¥é¥¤¥È
(setq show-paren-delay 0)
(setq show-paren-style 'expression);; highlight entire bracket expression
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#708090" :weight 'extra-bold)



;; ¥¿¥Ö¤ò¥¹¥Ú¡¼¥¹¤Ç°·¤¦
(setq-default indent-tabs-mode nil)

;; yes or no¤òy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 1 ¹Ô¤º¤Ä¥¹¥à¡¼¥º¤Ë¥¹¥¯¥í¡¼¥ë¤¹¤ë
(setq scroll-step 1)

;; ;; ¥Þ¥¦¥¹¥Û¥¤¡¼¥ë¤Ç¥¹¥¯¥í¡¼¥ë
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
;;¥­¡¼¥Ð¥¤¥ó¥É¤ÏÅ¬µ¹ÊÑ¹¹
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)

;;¥­¡¼¥Ð¥¤¥ó¥É
(global-set-key "\C-z" 'undo)

;(setq-default tab-width 4)
(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4)
  )
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)

;;window move
(windmove-default-keybindings 'super) ; Macの人はこちらをオススメ

;; ------------------------------------------------------------------------
;; @ initial frame maximize

;; µ¯Æ°»þ¤Ë¥¦¥£¥ó¥É¥¦ºÇÂç²½
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
         (set-frame-size (selected-frame) 95 60))))


;; MacÍÑ¥Õ¥©¥ó¥ÈÀßÄê
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

 ;; ±Ñ¸ì
 (set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 120)    ;; font size

;; ÆüËÜ¸ì
(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; È¾³Ñ¤ÈÁ´³Ñ¤ÎÈæ¤ò1:2¤Ë¤·¤¿¤±¤ì¤Ð
(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; MacÍÑ¥Õ¥©¥ó¥ÈÀßÄê

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)
;; ----------------------------------------------------------

;; autocomplete
(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20170124.1845"))
(require 'auto-complete)
(require 'auto-complete-config)

;; flycheck
;;(add-to-list 'exec-path (expand-file-name "~/.emacs.d/elpa/flycheck-20180422.2106"))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
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

(tabbar-mwheel-mode nil)                  ;; マウスホイール無効
(setq tabbar-buffer-groups-function nil)  ;; グループ無効
(setq tabbar-use-images nil)              ;; 画像を使わない


;;----- キーに割り当てる
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
(global-set-key (kbd "<M-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<M-left>") 'tabbar-backward-tab)


;----- 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
 (set btn (cons (cons "" nil)
                 (cons "" nil))))


;;----- タブのセパレーターの長さ
(setq tabbar-separator '(2.0))

;;----- tab color
(set-face-attribute
 'tabbar-default nil;; default
 :background "#E0E0E0"
 :foreground "#ffffff"
 :height 1.0)
(set-face-attribute;; sellected
 'tabbar-selected nil
 :background "#ffffff"
 :foreground "#000000"
 ;:weight 'bold
 :box nil)
(set-face-attribute
 'tabbar-modified nil;; modified
 :background "#D0D0D0" 
 :foreground "darkred"  
 :box nil)
(set-face-attribute ;; unsellected
 'tabbar-unselected nil
 :background "#D0D0D0"
 :foreground "black"
 :box nil)
;(set-face-attribute
; 'tabbar-button nil
; :box nil)

(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
		     ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
		     ((equal "*shell*" (buffer-name b)) b) ; *shell*バッファは表示する
		     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; ----------------------Haskell-----------------------------


(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.8.0")

(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;#!/usr/bin/env runghc ÍÑ
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;#!/usr/bin/env runhaskell ÍÑ


;; ----------------------MATLAB------------------------------

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/matlab-emacs-src/")) ;;¥Ñ¥¹ÀßÄê
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)

(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(add-hook 'matlab-mode-hook
         '(lambda ()
            (set-buffer-file-coding-system 'sjis-dos))) ;;M-¥Õ¥¡¥¤¥ë¤Ï¥·¥Õ¥ÈJIS¤Ç³«¤¯

(setq matlab-shell-command "/Applications/MATLAB_R2016a_Student.app/bin/matlab"  ;;bash¤Ë¥Ñ¥¹ÄÌ¤·¤Æ¤â¾å¼ê¤¯¤¤¤«¤Ê¤«¤Ã¤¿¤Î¤Ç
	matlab-indent-level 2  ;;¤³¤³¤éÊÕ¤Ï¹¥¤ß¤Ç¤É¤¦¤¾
	matlab-indent-function-body nil  
	matlab-highlight-cross-function-variables t
	matlab-return-add-semicolon t
	matlab-show-mlint-warnings t 
	mlint-programs '("/Applications/MATLAB_R2016a_Student.app/bin/maci64/mlint")  
	matlab-mode-install-path (list (expand-file-name "~/.emacs.d/elisp/matlab/"))
	)
		
(autoload 'mlint-minor-mode "mlint" nil t)  
(add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))
(add-hook 'matlab-shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'matlab-shell-mode-hook
		(lambda () (setenv "LANG" "C")))
(eval-after-load "shell"  
	  '(define-key shell-mode-map [down] 'comint-next-matching-input-from-input))
(eval-after-load "shell"
	  '(define-key shell-mode-map [up] 'comint-previous-matching-input-from-input))

;; ---------------------------go----------------------------------

;; go-mode
(add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20180327.830")

;; go¤Î¥Ñ¥¹¤òÄÌ¤¹
(add-to-list 'exec-path (expand-file-name "/usr/local/bin/go"))
;; git hub¾å¤Ç¸ø³«¤µ¤ì¤Æ¤¤¤ëgo¤Ëautocompleteµ¡Ç½¤ò¼Â¸½¤¹¤ë°Ù¤Î¥³¥Þ¥ó¥É¥é¥¤¥ó¥Ä¡¼¥ë
(add-to-list 'exec-path (expand-file-name "/Users/hamakei/.go/bin/gocode"))
;; go get ¤ÇÆþ¤ì¤¿¥Ä¡¼¥ë¤Î¥Ñ¥¹¤òÄÌ¤¹
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


;flycheck-mode¤òÍ­¸ú²½¤·¤Æ¥·¥ó¥¿¥Ã¥¯¥¹¥¨¥é¡¼¤ò¸¡ÃÎ
;(add-hook 'go-mode-hook 'flycheck-mode)

; go-mode¤ÈÊä´°µ¡Ç½¤òÀßÄê
(autoload 'go-mode "go-mode" nil t)
(eval-after-load "go-mode" '(progn (require 'go-autocomplete)))
(add-hook 'go-mode-hook (lambda()
       (add-hook 'before-save-hook 'gofmt-before-save) ;;ÊÝÂ¸»þ¤Ë¼«Æ°¤Çgo¡¡fmt(go¥³¡¼¥ÉÀ°·Á)
       (local-set-key (kbd "M-.") 'godef-jump);;ÄêµÁ¥¸¥ã¥ó¥×godef
       (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
       (local-set-key (kbd "C-c i") 'go-goto-imports)
       (local-set-key (kbd "C-c d") 'godoc)
       (setq indent-tabs-mode nil)    ; ¥¿¥Ö¤òÍøÍÑ
       (setq c-basic-offset 4)    ; tab¥µ¥¤¥º¤ò4¤Ë¤¹¤ë
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

;; go get ¤ÇÆþ¤ì¤¿¥Ä¡¼¥ë·´¤âEmacs¤«¤é»È¤¤¤¿¤¤¤Î¤Ç¡¢ $GOPATH/bin ¤âÄÉ²Ã
(add-to-list 'exec-path (expand-file-name "~/dev/go-workspace/bin"))

;; homebrew¤ÇÆþ¤ì¤¿¥Ä¡¼¥ë¤òEmacs¤«¤é»È¤¦¤¿¤á¡¢ homebrew¤Îbin¥Ç¥£¥ì¥¯¥È¥ê¤ò exec-path ¤ËÄÉ²Ã
(add-to-list 'exec-path (expand-file-name "~/homebrew/bin"))
;;¡¡-----------------------------------------------------------------

