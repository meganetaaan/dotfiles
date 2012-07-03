(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cl)
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;;
;;twittering-mode
;;___________________

(add-to-list 'load-path "~/.eamcs.d/twittering-mode-2.0.0")
(require 'twittering-mode)
(setq twittering-use-master-password t) ;;use master password
;; キーを設定
(add-hook 'twittering-mode-hook
          '(lambda ()
             (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
             (define-key twittering-mode-map (kbd "R") 'twittering-reply-to-user)
             (define-key twittering-mode-map (kbd "Q") 'twittering-organic-retweet)
             (define-key twittering-mode-map (kbd "T") 'twittering-native-retweet)
             (define-key twittering-mode-map (kbd "M") 'twittering-direct-message)
             (define-key twittering-mode-map (kbd "N") 'twittering-update-status-interactive)
             (define-key twittering-mode-map (kbd "C-c C-f") 'twittering-home-timeline)))

;;
;; PATH
;;______________________________________________________________________

(setq exec-path (cons "/usr/local/bin" exec-path))

;;
;; mail address
;;______________________________________________________________________

(setq user-mail-address "ishikawa.s.ab@m.titech.ac.jp")

;;
;; Window settings
;;______________________________________________________________________

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 85)  ; 透明度
      (tool-bar-mode 0)                  ; ツールバー非表示
      (set-scroll-bar-mode nil)            ; スクロールバー非表示
      (setq line-spacing 0.2)              ; 行間
      (setq ns-pop-up-frames nil)))
;;
;; Color
;;______________________________________________________________________

(set-foreground-color                                  "#FFFFFF") ; 文字色
(set-background-color                                  "#000000") ; 背景色
;(set-cursor-color                                      "#FF0000") ; カーソル色
;(set-face-background 'region                           "#222244") ; リージョン
;(set-face-foreground 'modeline                         "#CCCCCC") ; モードライン文字
;(set-face-background 'modeline                         "#333333") ; モードライン背景
;(set-face-foreground 'mode-line-inactive               "#333333") ; モードライン文字(非アクティブ)
;(set-face-background 'mode-line-inactive               "#CCCCCC") ; モードライン背景(非アクティブ)
;(set-face-foreground 'font-lock-comment-delimiter-face "#888888") ; コメントデリミタ
;(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
;(set-face-foreground 'font-lock-string-face            "#7FFF7F") ; 文字列
;(set-face-foreground 'font-lock-function-name-face     "#BF7FFF") ; 関数名
;(set-face-foreground 'font-lock-keyword-face           "#FF7F7F") ; キーワード
;(set-face-foreground 'font-lock-constant-face          "#FFBF7F") ; 定数(this, selfなども)
;(set-face-foreground 'font-lock-variable-name-face     "#7F7FFF") ; 変数
;(set-face-foreground 'font-lock-type-face              "#FFFF7F") ; クラス
;(set-face-foreground 'fringe                           "#666666") ; fringe(折り返し記号なでが出る部分)
;(set-face-background 'fringe                           "#282828") ; fringe

;;
;; encoding
;;______________________________________________________________________

(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;;
;; general key bind
;;______________________________________________________________________

(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-c d")   'delete-indentation)
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "C-S-i")   'indent-region)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "C-t")     'next-multiframe-window)
(global-set-key (kbd "C-S-t")   'previous-multiframe-window)
(global-set-key (kbd "C-M-r")   'replace-regexp)
(global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "C-/")     'undo)

;;
;; backup file
;;______________________________________________________________________

;; 残さない
(setq make-backup-files nil)

;;
;; miscellaneous
;;______________________________________________________________________

;; Bell
(setq ring-bell-function 'ignore)
;; Scroll
(setq scroll-conservatively 35
       scroll-margin 0
       scroll-step 1)
;; Column number
(column-number-mode t)
;; Fill column
(setq default-fill-column 72)
;; Truncate
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
;; Indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)
;; Narrowing
(put 'narrow-to-region 'disabled nil)
;; 行番号表示
;(global-linum-mode t)
;(setq linum-format "%4d")
;; スタートページ非表示
(setq inhibit-startup-message t)

;;
;; highlight
;;______________________________________________________________________

;; highlight current line
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#000000")
;; hilight paren
(show-paren-mode 1)
;; highlight reagion
(setq transient-mark-mode t)
;; highlight edit characters
(require 'jaspace)
(setq jaspace-highlight-tabs t)
(add-hook 'mmm-mode-hook 'jaspace-mmm-mode-hook)

;;
;; yasnippet
;;______________________________________________________________________

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/site-lisp/snippets")
(yas/load-directory yas/root-directory)
(set-face-background 'yas/field-highlight-face "#404040")

;;
;; flymake-mode
;;______________________________________________________________________
(require 'popup)
(require 'flymake)

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

;; 全てのファイルで flymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; エラーメッセージをポップアップ表示
(defun flymake-popup-err-message ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no            (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data          (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
      (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                            (nth 1 menu-data)
                            "\n")))
    ))
;; ;; カーソルをエラー行に載せるとエラーメッセージをポップアップ表示
;; ;; anythingと干渉するようなのでコメントアウト 
;; (defadvice flymake-mode (before post-command-stuff activate compile)
;;   "エラー行にカーソルが当ったら自動的にエラーが minibuffer に表示されるように
;; post command hook に機能追加"
;;(set (make-local-variable 'post-command-hook)
;;      (add-hook 'post-command-hook 'flymake-popup-err-message))

;; キーバインド
(global-set-key "\M-p" 'flymake-goto-prev-error)
(global-set-key "\M-n" 'flymake-goto-next-error)
(global-set-key "\C-cd" 'flymake-popup-err-message)


;;
;; html-helper-mode
;;______________________________________________________________________

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-basic-offset 0)
(setq html-helper-item-continue-indent 0)
(defvar html-helper-new-buffer-template
  '("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
    "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ja\" lang=\"ja\">\n"
    "\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n"
    "<title></title>\n"
    "</head>\n"
    "\n"
    "<body>\n"
    "\n"
    "\n"
    "\n"
    "</body>\n"
    "</html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if html-helper-build-new-buffer is set to t")
(require 'sgml-mode)
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'sgml-close-tag)))
;;
;; misc
;;____________________

;find file(or url) at point
(ffap-bindings)

;multi-terminal
(require 'multi-term)
(setq multi-term-program shell-file-name)
(global-set-key (kbd "C-c t") '(lambda ()
                                (interactive)
                                (if (get-buffer "*terminal<1>*")
                                    (switch-to-buffer "*terminal<1>*")
                                (multi-term))))

;;
;; Fullscreen
;;_________________________________

(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width 82)
           (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

(my-toggle-fullscreen)
(global-set-key [f11] 'my-toggle-fullscreen)

;;
;; for linux
;;__________________________
(add-to-list 'load-path "/usr/share/emacs23/site-lisp")

;;
;; emacs mozc
;;__________________________
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")


;;
;; evernote-mode
;;__________________________
(setq evernote-ruby-command "/usr/bin/ruby")
(require 'evernote-mode)
(setq evernote-username "bakamegane") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;;
;; yatex-mode 設定
;;______________________________

;; yatex-mode をロードする
;; (require 'yatex-mode) ではエラーが発生
(add-to-list 'load-path "/usr/share/emacs23/site-lisp/yatex")
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist (append
  '(("\\.tex$" . yatex-mode)
    ("\\.ltx$" . yatex-mode)
    ("\\.cls$" . yatex-mode)
    ("\\.sty$" . yatex-mode)
    ("\\.clo$" . yatex-mode)
    ("\\.bbl$" . yatex-mode)) auto-mode-alist))