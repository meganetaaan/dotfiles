;;
;; PATH
;;_____________________________________________________________________
;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/texbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
 ;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

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

(setq initial-frame-alist
      (append (list
	       '(width . 50)
	       '(height . 45)
	       '(top . 0)
	       '(left . 0)
	       )
	      initial-frame-alist))
;;
;; Fonts
;;______________________________________________________________________

;;(add-to-list 'default-frame-alist '(font . "ricty-14"))
(set-face-attribute 'default nil :family "ricty" :height 140)
(set-fontset-font "fontset-default" 'japanese-jisx0208 `("ricty" . "iso10646-*"))

;;
;; evernote-mode
;;______________________________________________________________________
;;(add-to-list 'exec-path "/usr/local/bin/")
;;    (setenv "PATH" (concat (getenv "PATH") ";/usr/local/bin/"))
(setq evernote-ruby-command "/usr/local/Cellar/ruby/1.9.3-p327/bin/ruby")

(add-to-list 'load-path "~/.emacs.d/site-lisp/evernote-mode-0_41")
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
;; Color
;;______________________________________________________________________

(set-foreground-color                                  "#FFFFFF") ; 文字色
(set-background-color                                  "#000000") ; 背景

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
(global-set-key (kbd "M-<RET>") 'ns-toggle-fullscreen)
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
;; auto-complete
;;______________________________________________________________________
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(add-hook 'emacs-startup-hook
	  (function (lambda ()
		      (require 'auto-complete-config)
		      (ac-config-default))))
(require 'ac-python)
;(add-to-list 'ac-modes 'python-2-mode)

;;
;; flymake
;;______________________________________________________________________
;;; flymake for python
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pychecker"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
;(load-library "flymake-cursor")
;(global-set-key [f10] 'flymake-goto-prev-error)
;(global-set-key [f11] 'flymake-goto-next-error)

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
;; packages
;;___________________________________________________________________
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;
;; TeX
;;___________________________________________________________________
(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex")
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)

(setq tex-command "platex -synctex=1")
(setq dvi2-command "pxdvi -watchfile 1 -s 6")
(setq YaTeX-dvipdf-command "dvipdfmx")

(setq YaTeX-typeset-auto-rerun nil)

;; RefTeX with YaTeX                                                                      
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))


;;
;; markdown
;;________________________________________________
(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))
