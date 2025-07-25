;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; code:
;; -----------------  Hacks for speeding up initialization.
(defconst +file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 16777216)
;(setq gc-cons-threshold most-positive-fixnum) ; gc use big memory
;; Packages should have been made available.  Disable it to speed up
;; installing packages during initialization.
(setq package-quickstart nil)

;; ------------------ move default dir   " it make win gui server mode err"
;; replace emacs paths early -- before doing anything
;; (use-package no-littering
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq auto-save-file-name-transforms
;;         `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
;; 	)
;;   )

;; -------------------------------config start
;; =============================  config use-package
;; Set up use-package for user config
(setq use-package-always-ensure t)  ; All packages used have to be installed

;; ============================  benchmark
(use-package benchmark-init
  :demand t				;立刻加载
  :config (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  )
;(desktop-save-mode +1)
(recentf-mode +1)
(setq recentf-max-menu-items 25)
(defvar autosaves-dir (expand-file-name "autosaves/" user-emacs-directory))
(defvar backups-dir (expand-file-name "backups/" user-emacs-directory))
(unless (file-exists-p autosaves-dir)
  (make-directory autosaves-dir t)
  )
(unless (file-exists-p backups-dir)
  (make-directory backups-dir t)
  )

;; change default action
(setq default-directory "~/" )
(message "set default dir = %s" (expand-file-name "~/"))
(setq backup-directory-alist
;      `((".*" . ,temporary-file-directory))
      `((".*" . ,backups-dir))

     backup-by-copying t ; 自动备份
     delete-old-versions t ; 自动删除旧的备份文件
     kept-new-versions 3 ; 保留最近的3个备份文件
     kept-old-versions 1 ; 保留最早的1个备份文件
     version-control t) ; 多次备份
(setq auto-save-file-name-transforms
;      `((".*" ,temporary-file-directory t))
      `((".*" ,autosaves-dir t))		;自动保存临时文件
      )
(setq auto-save-timeout 5)		;set default auto save time without input
;(setq create-lockfiles nil) ; 使用下方操作修改lock文件（.#*）位置
(setq lock-file-name-transforms
      `((".*" ,backups-dir t))
      )

;; def fun show os name
(defun print-os()
  "Show current os name."
  (interactive)
  (message "%s" system-type)
  )

;; def fun quick open config file
(defun open-init-file()
"Open Emacs config file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el"))
  )

;; 自定义两个函数
;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (forward-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (forward-line -10))
;; 绑定到快捷键
(global-set-key (kbd "M-n") 'next-ten-lines)            ; 光标向下移动 10 行
(global-set-key (kbd "M-p") 'previous-ten-lines)        ; 光标向上移动 10 行

;; ======================       keybind
(global-set-key (kbd "M-<f3>") 'open-init-file)
(global-set-key (kbd "C-x ,") 'open-init-file)
(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
;; leader key
(global-set-key (kbd "M-SPC") nil) ;修改默认keybind M-SPC -> nil, 作为leader使用，用于各种命令替代
(global-set-key (kbd "M-ESC") 'keyboard-quit)
(global-set-key (kbd "C-j") nil)	      ;修改默认的C-j功能，作为编辑的leader key使用
(global-set-key (kbd "C-j C-j") 'electric-newline-and-maybe-indent);原始的C-j功能修改
(global-set-key (kbd "ESC ]") 'cycle-spacing) ;原始M-SPC功能修改为
;;(global-set-key (kbd "ESC SPC") 'cycle-spacing) ;test ESC SPC leaderkey使用
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-SPC c") 'comment-line)
(global-set-key (kbd "C-j c") 'comment-line)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-,") 'completion-at-point)
(defun newline-and-indent-up ()
  "回车到上一行."
  (interactive)
  (forward-line -1)
  (move-end-of-line 1)
  (newline-and-indent)
  )
(defun newline-and-indent-down ()
  "回车到下一行."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  )
(defun duplicate-line ()
  "Duplicate line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )
(defun clear-line ()
  "clear line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  )
(global-set-key (kbd "C-j C-i") 'newline-and-indent-up)
(global-set-key (kbd "C-j C-o") 'newline-and-indent-down)
(global-set-key (kbd "C-j C-d") 'duplicate-line)
(global-set-key (kbd "C-j C-l") 'clear-line)

;; ======================      config ui
(setq cursor-type 'bar);box)       ; 终端不生效  原因不明
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")

;;(fido-vertical-mode +1)			;minibuffer垂直补全  和 orderless冲突
;(icomplete-vertical-mode +1)	      ;minibuffer垂直补全
(global-hl-line-mode 1)		;高亮当前行
(global-tab-line-mode +1)		;显示tab line 不同的buffer编辑区
(tab-bar-mode +1)			;显示tab bar  相当于不同的工作区
(column-number-mode +1)			;显示行列在buffer区域
(global-display-line-numbers-mode +1)
(electric-pair-mode +1)			;自动补全括号
(electric-quote-mode +1)
(electric-indent-mode +1)
(electric-layout-mode +1)
(show-paren-mode +1)			;
(delete-selection-mode +1)              ;选中区域后插入删除选中文字
(global-auto-revert-mode +1)		;实时刷新文件

(eval-after-load "dired"
  '(define-key dired-mode-map (kbd "b") 'dired-up-directory)
  )
(add-hook 'prog-mode-hook 'hs-minor-mode) ;折叠模式
;; 这里额外启用了 :box t 属性使得提示更加明显
;; (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
;; (defun hideshow-folded-overlay-fn (ov)
;;     (when (eq 'code (overlay-get ov 'hs))
;;       (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
;;              (info (format " ... #%d " nlines)))
;;         (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
;; (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)


;(setq icomplete-in-buffer t)
(setq completion-auto-help 'always)
;(setq completions-detailed t)
(setq completions-format 'one-column);one-column quickly vertical>slow
(setq completion-cycle-threshold 4)
(setq completion-preview-ignore-case t)
(setq completion-ignore-case t)
(setq completions-max-height 15)

(setq completion-preview-completion-styles '(orderless basic initials))
(global-completion-preview-mode +1)


(setq scroll-margin 3)			;set next page margin line
(setq scroll-conservatively 101)	;if value greater than 100, will nerver scroll
(setq resize-mini-windows t)
(setq max-mini-window-height 5)
;(completion-preview-active-mode)

(repeat-mode +1)
(setq repeat-exit-key (kbd "q"))
(defvar buffer-lunch-repeat-map ; C-x <left> 或 <right>
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "n") #'next-line)
        (define-key map (kbd "p") #'previous-line)
        (define-key map (kbd "f") #'forward-char)
	(define-key map (kbd "b") #'backward-char)
	(define-key map (kbd "e") #'previous-line)
	(define-key map (kbd "d") #'next-line)
	(define-key map (kbd "s") #'backward-char)
	(define-key map (kbd "v") #'forward-word)
	(define-key map (kbd "z") #'backward-word)
	(define-key map (kbd "a") #'back-to-indentation)
	(define-key map (kbd "g") #'move-end-of-line)
	(define-key map (kbd "N") #'next-ten-lines)
	(define-key map (kbd "P") #'previous-ten-lines)

        (dolist (it '(next-line previous-line forward-char backward-char forward-word backward-word back-to-indentation move-end-of-line left-word right-word previous-ten-lines next-ten-lines))
          (put it 'repeat-map 'buffer-lunch-repeat-map))
        map)
      "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."
      )


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package paren
  :init
  :config (setq show-paren-when-point-in-periphery t
 		show-paren-when-point-inside-paren t
	       	show-paren-style 'mixed
		show-paren-delay 0.2
 		)
  )
(use-package colorful-mode
  :defer t
  :config
  (setq colorful-use-prefix t))


;; set hl line only use in line end non-word partition
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
						(line-beginning-position 2))
				 )
	)
  )
;; see in url https://emacs-china.org/t/topic/28495/3
(use-package highlight-parentheses
;  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
					;         (prog-mode . highlight-parentheses-mode))
;         (after-init . highlight-parentheses-mode)
         (after-init . global-highlight-parentheses-mode)
	 )
  :config
  (setq highlight-parentheses-colors
;	'("firebrick1" "firebrick3" "orange1" "orange3")
	'("firebrick1" "IndianRed1" "firebrick3" "IndianRed3" "IndianRed4")
        highlight-parentheses-attributes
	'((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2
	)
  )


(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.2)
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
  (setq orderless-matching-styles '(orderless-literal
				    orderless-flex
				    orderless-regexp
				    )
	)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles basic partial-completion)))
	)
  )

(use-package diff-hl
  :hook
;  (global-diff-hl-mode . diff-hl-margin-mode)
  (diff-hl-mode . diff-hl-margin-local-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode +1)
 ; (global-diff-hl-show-hunk-mouse-mode -1)
 ; (diff-hl-flydiff-mode +1)
  (diff-hl-amend-mode +1)
  :config
  (advice-add 'svn-status-update-modeline :after #'diff-hl-update)
  )

(use-package consult
  :ensure t
  :bind
  (("C-j s" . consult-line)
   ("M-SPC s" . consult-line)
   )
  )
(use-package vertico
  :init
  (vertico-mode +1)
  )
(use-package posframe
  )
(use-package vertico-posframe
  :after (vertico posframe)
  )
;(vertico-posframe-mode +1)


(use-package company
  :disabled
  :init
  (company-mode +1)
  )

(use-package corfu
;  :disabled
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-auto-deply 0.1)
  (corfu-min-width 2)
;  (corfu-quit-at-boundary nil)
  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  (corfu-indexed-mode)
  :bind
  (
   :map corfu-map
   ("RET" . 'newline)
   ("SPC" . 'corfu-insert-separator)
   )
  :config
;  (keymap-unset corfu-map "RET");配置无效 原因不明
  (progn
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred))))
      )
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
    )
  )


;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-abbrev)
;  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )


;; key bind mode
(use-package meow
   :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
  ; '("j" . meow-next)
  ; '("k" . meow-prev)
  ; '("<escape>" . ignore)
   )
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   ;; '("b" . meow-back-word)
   ;; '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-kill)
   '("d" . meow-next)
   '("D" . meow-next-expand)
   ;; '("d" . meow-delete)
   ;; '("D" . meow-backward-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   ;; '("e" . meow-next-word)
   ;; '("E" . meow-next-symbol)
   '("f" . meow-right)
   '("F" . meow-right-expand)
   ;; '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   ;; '("q" . meow-quit)
   '("q" . meow-kill)
   '("Q" . meow-quit)
   ;; '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-left)
   '("S" . meow-left-expand)
   ;; '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-find)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-next-word)
   '("V" . meow-next-symbol)
   ;; '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   ;; '("z" . meow-pop-selection)
   '("`" . meow-pop-selection)
   '("z" . meow-back-word)
   '("Z" . meow-back-symbol)
   '("'" . repeat)
   '("<escape>" . ignore))
  )
  (meow-setup)
  (meow-global-mode 1)
 
  ;; (meow-motion-define-key '("n" . next-line))
  ;; (meow-motion-define-key '("p" . previous-line))
  ;; (meow-motion-define-key '("f" . forward-char))
  ;; (meow-motion-define-key '("b" . backward-char))
  ;; (meow-motion-define-key `("x" . ,(kbd "C-x")))
  ;; (meow-motion-define-key `("<escape>" . ,(kbd "<escape>")))
  ;; (meow-motion-define-key '("q" . meow-motion-mode))
  ;; (meow-motion-define-key '("i" . meow-motion-mode))
  ;; (global-set-key (kbd "C-c m") 'meow-motion-mode)
  ;; (defun meow-motion-enable()
  ;;   (interactive)
  ;;   (setq meow-motion-mode t)
  ;;   (message "Meow-Motion mode enable in current buffer" )
  ;;   )
  ;; (global-set-key (kbd "C-'") 'meow-motion-enable)
  (defun meow-enable ()
    "Enable meow."
    (interactive)
    (meow-global-mode +1)
    )
  (defun meow-disable ()
    "Disable meow."
    (interactive)
    (meow-global-mode -1)
    )
  (global-set-key (kbd "C-'") 'meow-enable)
  (global-set-key (kbd "C-.") 'meow-disable)
  )


(use-package flymake
  :ensure nil
  :init
  (add-hook 'prog-mode-hook 'flymake-mode)
  (setq flymake-show-diagnostics-at-end-of-line t)
  (setq flymake-no-changes-timeout 0.9)
  :bind
  (
   ("C-j C-n" . 'flymake-goto-next-error)
   ("C-j C-p" . 'flymake-goto-prev-error)
   )
  )

;; ====================     term
(use-package eat
  :bind (
	 ("ESC SPC v" . 'eat-other-window)
	 :map eat-semi-char-mode-map ("M-o" . 'other-window)
	  )
  :config
  (setq eat-kill-buffer-on-exit nil)
  (setq eat-enable-blinking-text t)
  (setq eat-enable-directory-tracking t)
  )

;; ========================   config project
(use-package projection
;  :disabled
  )
(use-package magit
  ;:disabled
  :defer t
  )
(use-package eglot
  :ensure nil
;  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format)
  )
(use-package eglot-java
  :after eglot
  ;:init
  )

;; =============== package straight
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;  (load bootstrap-file nil 'nomessage))
;; ================ straight end
;; (use-package yasnippet
;;   :init
;;   (yas-global-mode +1)
;;   )

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode))
(use-package kbd-mode
  :disabled
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest)
  )

;; custom
(when (file-exists-p custom-file)
  (load custom-file))
;; End of hacks.
(setq file-name-handler-alist +file-name-handler-alist)
(setq gc-cons-threshold 16777216) ;; 16mb
;; Re-enable package-quickstart.
(setq package-quickstart t)

(use-package server
  :if window-system
;     :commands (server-running-p)
     :init
  (progn
      (server-mode +1)
      (message "Emacs Server …DONE")
      )
  )



(provide 'init)
;;; init.el ends here
