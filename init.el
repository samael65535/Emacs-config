
(setq default-frame-alist'((height . 30) (width .40) (menu-bar-lines . 20) (tool-bar-lines . 0)))
(setq default-directory "~/Code")    ;设置打开时的默认路径
(setq inhibit-startup-message t)
;;切换到另一个窗口，快捷键为C+Tab
(global-set-key [C-tab] 'other-window)
;;启动最大化
(global-set-key [f11] 'my-fullscreen);快捷键最大化
(defun my-fullscreen ()
(interactive)
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
)


;;设置自动提醒
(appt-activate)   ;调用函数
;; 设置weibo模式
(add-to-list 'load-path "~/.emacs.d/weibo/")
(require 'weibo)
(setq weibo-display-image nil)
;; 新版org-mode
(setq load-path (cons "~/.emacs.d/org-7.8.11/lisp/" load-path))
;; 设置org-mode自动换行
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; xml-rpc
(add-to-list 'load-path "~/.emacs.d/xml-rpc")
(require 'xml-rpc)
;; 设置google-chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
;; 设置org2blog
(setq load-path (cons "~/.emacs.d/org2blog/" load-path))
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("my-blog"
         :url "http://samael.us/xmlrpc.php"
         :username "samael65535")))

;; 设置color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-pok-wog)
;; 备份文件目录
(setq  backup-by-copying t) ; 自动备份
;;自动备份目录~/.emacs.d/backup
(setq  backup-directory-alist  '(("." . "~/.emacs.d/backup")))
(setq  delete-old-versions t) ; 自动删除旧的备份文件
(setq  kept-new-versions 2) ; 保留最近的3个备份文件
(setq  kept-old-versions 1) ; 保留最早的2个备份文件
(setq  version-control t) ; 多次备份
;;;不要生成备份文件
;(setq-default make-backup-files nil)

;; Function to copy lines 
;; C-c C-w 复制整行, 而"C-u 5 C-c w"复制 5 行
(defun copy-lines(&optional arg) 
(interactive "p" )
(save-excursion 
(beginning-of-line) 
(set-mark (point)) 
(if arg 
(next-line (- arg 1))) 
(end-of-line) 
(kill-ring-save (mark) (point)) 
) 
) 
;;复制一行 set key 
(global-set-key (kbd "C-c w") 'copy-lines) 

;; 设置shell
(ansi-color-for-comint-mode-on)

;; 显示行号
(require 'linum)
(setq linum-format "%3d ")
;对所有文件生效
(add-hook 'find-file-hooks (lambda () (linum-mode 1)))

;; 更一个快速编译的方法
(defun quick-compile ()
"A quick compile funciton for C"
(interactive)
(compile (concat "gcc -ggdb -o " (buffer-name (current-buffer))".out " (buffer-name (current-buffer)) " -lm"))
)
;;快捷键F9
(global-set-key [(f12)] 'compile)

;;evernote-mode代码
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(add-to-list 'load-path "~/.emacs.d/evernote-mode/")  ;evernote-mode插件路径
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;;设置TODO
(global-set-key  (kbd "<f12>") 'todo-show)  ;F12设置为添加新的item
(setq todo-file-do "~/todo/do")
(setq todo-file-done "~/todo/done")
(setq todo-file-top "~/todo/top")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(display-time-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8) (todo-categories "study plan" "reading book" "personal") (todo-categories "reading book" "study plan" "personal") (todo-categories "study plan" "personal"))))
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

(put 'dired-find-alternate-file 'disabled nil)

;;设置auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

;;代码折叠
(add-hook 'c-mode-common-hook 'hs-minor-mode)
;;设置yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet-bundle)
(put 'upcase-region 'disabled nil)

(defface ac-yasnippet-candidate-face
  '((t (:backgrodund "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")
 
(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")
 
(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

;;xscheme-mode
(add-to-list 'load-path "~/.emacs.d/xscheme/xscheme.el")
(require 'xscheme)
;;设置magit
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)

;; 设置最近打开的文件
(require 'recentf)
(recentf-mode t)

;; golden-ratio分割窗口
(add-to-list 'load-path "~/.emacs.d/golden-ratio/")
(require 'golden-ratio)
(golden-ratio-enable)

