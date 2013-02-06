(setq debug-on-error t) ;报告错误
(setq backup-inhibited t) ;不产生备份
;; 高亮当前行
(require 'hl-line)
(global-hl-line-mode t)
(define-key global-map "\C-c\C-g" 'goto-line) ;设置跳转快捷键
(setq auto-save-default nil) ; 不生成名为#filename# 的临时文件
(load-file "~/.emacs.d/init-dired/init-dired.el") ; config dired
(mouse-avoidance-mode 'animate);光标靠近鼠标指针时，让鼠标指针自动让开
(setq mouse-yank-at-point t);支持中键粘贴
(global-cwarn-mode 1) ; 高亮显示C/C++中的可能的错误(CWarn mode)
(setq initial-scratch-message ";; Abandon hope all ye who enter here\n") ;设置scratch的欢迎文字
(global-set-key [(f1)] 'speedbar) ;开启speedbar
(setq default-directory "~/Code")    ;设置打开时的默认路径
(setq inhibit-startup-message t) ; 去掉欢迎界面
(global-set-key [C-tab] 'other-window) ;切换到另一个窗口，快捷键为C+Tab
(setq-default make-backup-files nil) ;不要生成备份文件
;; markdown-mode
(autoload 'markdown-mode "~/.emacs.d/markdown-mode/markdown-mode.el" "Major mode for editing Markdown files" t) 
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;; cedet
;; (add-to-list 'load-path "~/.emacs.d/jdee-2.4.0.1/lisp")
;; (add-to-list 'load-path "~/.emacs.d/cedet-1.1/common")
;; (add-to-list 'load-path "~/.emacs.d/elib-1.0")
;; (load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;; (global-semanticdb-minor-mode 1)
;; (global-ede-mode t)

;;jdee
;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde t)
(if defer-loading-jde    
    (progn    
      (autoload 'jde-mode "jde" "JDE mode." t)    
      (setq auto-mode-alist    
            (append    
             '(("\\.java\\'" . jde-mode))    
             auto-mode-alist)))
(require 'jde))
 
;; web-mode
(add-to-list 'load-path "~/.emacs.d/web-mode/")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(defun screen-width nil -1)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")

(setq default-frame-alist'((height . 30) (width .40) (menu-bar-lines . 20) (tool-bar-lines . 0)))
(setq default-directory "~/Code")    ;设置打开时的默认路径

(setq inhibit-startup-message t) ; 去掉欢迎界面

(global-set-key [C-tab] 'other-window) ;切换到另一个窗口，快捷键为C+Tab
; f12全屏
(global-set-key [f12] 'my-fullscreen)
(defun my-fullscreen ()
(interactive)
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
)

;;设置自动提醒
(appt-activate)   ;调用函数

;; wikipedia-mode.el
(autoload 'wikipedia-mode                                         
  "~/.emacs.d/wikipedia-mode/wikipedia-mode.el" 
  "Major mode for editing documents in Wikipedia markup." t)      
;; ibuffer 
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 新版org-mode
(setq load-path (cons "/org-7.8.11/lisp/" load-path))
;; 设置org-mode自动换行
(add-hook 'org-mode-hook 'toggle-truncate-lines)
;; tabbar 设置
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode 1)
(global-set-key (kbd "C--") 'tabbar-backward)  
(global-set-key (kbd "C-=") 'tabbar-forward)  
;; 设置tabbar外观  
;; 设置默认主题: 字体, 背景和前景颜色，大小  
(set-face-attribute 'tabbar-default nil  
                    :family "DejaVu Sans Mono"  
                    :background "gray80"  
                    :foreground "gray30"  
                    :height 1.0  
                    )  
;; 设置左边按钮外观：外框框边大小和颜色  
(set-face-attribute 'tabbar-button nil  
                    :inherit 'tabbar-default  
                    :box '(:line-width 1 :color "yellow70")  
                    )  
;; 设置当前tab外观：颜色，字体，外框大小和颜色  
(set-face-attribute 'tabbar-selected nil  
                    :inherit 'tabbar-default  
                    :foreground "DarkGreen"  
                    :background "LightGoldenrod"  
                    :box '(:line-width 2 :color "DarkGoldenrod")  
                    :overline "black"  
                    :underline "black"  
                    :weight 'bold  
                    )  
;; 设置非当前tab外观：外框大小和颜色  
(set-face-attribute 'tabbar-unselected nil  
                    :inherit 'tabbar-default  
                    :box '(:line-width 2 :color "#00B2BF")  
                    )  
 
;; xml-rpc
(add-to-list 'load-path "~/.emacs.d/xml-rpc")
(require 'xml-rpc)
;; 设置google-chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; 设置color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-pok-wog)
(setq  version-control t) ; 多次备份

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline bold bold-italic modeline])
 '(ansi-color-names-vector ["black" "red" "PaleGreen" "yellow" "DodgerBlue1" "magenta" "cyan" "white"])
 '(column-number-mode t)
 '(display-time-mode t)
 '(ede-project-directories (quote ("/home/samael/Code/os_diy/08test" "/home/samael/Code/os_diy/08" "/home/samael/Code/test/aa" "/home/samael/Code/my_projects/c/test_cedet")))
 '(global-hl-line-mode t)
 '(hl-line-face (quote hl-line))
 '(jde-java-environment-variables (quote ("1.6" "/usr/bin/java")))
 '(jde-jdk (quote ("1.6")))
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/bin/java"))))
 '(safe-local-variable-values (quote ((encoding . utf-8) (todo-categories "study plan" "reading book" "personal") (todo-categories "reading book" "study plan" "personal") (todo-categories "study plan" "personal"))))
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))

;; 显示行号
(require 'linum)
(setq linum-format "%3d ")
;对所有文件生效
(add-hook 'find-file-hooks (lambda () (linum-mode 1)))

;;设置TODO
(global-set-key  (kbd "<f11>") 'todo-show)  ;F11设置为添加新的item
(setq todo-file-do "~/todo/do")
(setq todo-file-done "~/todo/done")
(setq todo-file-top "~/todo/top")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(hl-line ((t (:background "gray13")))))

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

;;git-emacs
(add-to-list 'load-path "~/.emacs.d/git-emacs/")
(require 'git-emacs)
;; 设置最近打开的文件
(recentf-mode t)

;; golden-ratio分割窗口
(add-to-list 'load-path "~/.emacs.d/golden-ratio/")
(require 'golden-ratio)
(golden-ratio-enable)


;; compile 一键
(global-set-key [f9] 'compile) 

;; browse-kill-ring 功能
(add-to-list 'load-path "~/.emacs.d/browse-kill-ring/")
(require 'browse-kill-ring)
(require 'browse-kill-ring+)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

(put 'set-goal-column 'disabled nil)

;; smex
(add-to-list 'load-path "~/.emacs.d/smex/")
(require 'smex) ; Not needed if you use package.el
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; delete-regio
(global-set-key (kbd "C-c d") 'delete-region)

;; etags
(add-to-list 'load-path "~/.emacs.d/etags-mode/")
(autoload 'gtags-mode "gtags" "" t)
