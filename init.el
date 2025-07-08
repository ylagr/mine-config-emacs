;;; -*- lexical-binding: t; -*-

;; Hacks for speeding up initialization.
(defconst +file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
; (setq gc-cons-threshold most-positive-fixnum) ; gc use big memory
;; Packages should have been made available.  Disable it to speed up
;; installing packages during initialization.
(setq package-quickstart nil)

;; replace emacs paths early -- before doing anything
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	)
  )
;; -------------------------------config start
;; config use-package 
;; Set up use-package for user config
(setq use-package-always-ensure t)  ; All packages used have to be installed

;; benchmark
(use-package benchmark-init
  :demand t
  :config (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  )

;; def fun show os name
(defun print-os()
  (interactive)				
  (message "%s" system-type)
  )

;; def fun quick open config file
(defun open-init-file()			
  (interactive)
  (find-file "~/.config/emacs/init.el")
  )
(global-set-key (kbd "M-<f3>") 'open-init-file) 
(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
;; leader key
(global-set-key (kbd "M-SPC") nil) ;修改默认keybind M-SPC -> nil, 方便后续作为leader使用
(global-set-key (kbd "M-ESC") 'keyboard-quit)

(global-set-key (kbd "ESC p") 'cycle-spacing) ;原始M-SPC功能修改为
;;(global-set-key (kbd "ESC SPC") 'cycle-spacing) ;test ESC SPC leaderkey使用
(global-set-key (kbd "M-o") 'other-window)




;; config ui
;;(fido-vertical-mode +1)			;minibuffer垂直补全  和 orderless冲突
(icomplete-vertical-mode +1)	      ;minibuffer垂直补全
(global-hl-line-mode 1)		;高亮当前行
(global-tab-line-mode +1)		;显示tab line 不同的buffer编辑区
(tab-bar-mode +1)			;显示tab bar  相当于不同的工作区
(column-number-mode +1)			;显示行列在buffer区域
(delete-selection-mode +1)              ;选中区域后插入删除选中文字
(global-auto-revert-mode +1)		;实时刷新文件


(setq icomplete-in-buffer t)
(setq completion-auto-help 'always)
(setq completions-detailed t)
(global-completion-preview-mode +1)
(setq completion-preview-ignore-case t)
(setq completion-ignore-case t)
(setq completion-preview-completion-styles '(basic partial-completion initials orderless))

(repeat-mode +1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package paren
  :init
  :config (setq show-paren-when-point-in-periphery t
 		show-paren-when-point-inside-paren t
	       	show-paren-style 'parenthesis
 		)
  )

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.01)
  )



;; ------------------  补全
;; 帮助文档
(use-package helpful
  :ensure t
  :bind
  (("C-c h f" . helpful-callable)
   ("C-c h v" . helpful-variable)
   ("C-c h k" . helpful-key)
   ("C-c h s" . helpful-symbol)
   ))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("C-c '" . 'marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  )
(use-package orderless
  :init
  (setq completion-styles '(basic partial-completion orderless))
  )

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line))
  )

(use-package corfu
  :disabled
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-deply 0)
  (corfu-min-width 1)
  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )


;; ------------------  term
(use-package eat
  :bind ("ESC SPC v" . eat)
  :config
  (setq eat-kill-buffer-on-exit t)
  (setq eat-enable-blinking-text t)
  (setq eat-enable-directory-tracking t)
  )


;; custom
(load custom-file t)
