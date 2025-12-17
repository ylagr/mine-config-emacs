; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
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
;;(setq package-quickstart nil)
;; ------------------- Common consts
(defconst l/mac (eq system-type 'darwin))
(defconst l/windows (eq system-type 'windows-nt))
(defconst l/linux (eq system-type 'gnu/linux))
(defconst l/wsl2 (string-match-p "WSL2" (shell-command-to-string "uname -r")))
(defconst l/nix (string-match-p "current-system" (shell-command-to-string "which nix")))
(defconst l/wsl1 (and
                  (eq system-type 'gnu/linux)
                  (not l/wsl2)
                  ))

;; Don't recenter to the middle of the screen
(setq recenter-positions '(top 0.3 bottom))


;; ------------------ move default dir   " it make win gui server mode err"
;; replace emacs paths early -- before doing anything
;; (use-package no-littering
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq auto-save-file-name-transforms
;;         `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
;;      )
;;   )

;; -------------------------------config/ start
;; =============================  config/ use-package
;; Set up use-package for user config/
(setq use-package-always-ensure t)  ; All packages used have to be installed

;; ============================  benchmark
(use-package benchmark-init
  :demand t                             ;立刻加载
  :config (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  )


;; config/ builtin config and kbd.
(use-package emacs
  :demand t
  :config
  (setq confirm-kill-emacs 'yes-or-no-p)
  ;; (desktop-save-mode +1)
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
	`((".*" ,autosaves-dir t))                ;自动保存临时文件
	)
  (setq auto-save-timeout 5)              ;set default auto save time without input
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
  (if t
      (progn
	(defun next-half-page-lines()
          "Move cursor to next half-page lines."
          (interactive)
          (forward-line (/ (window-body-height) 2)))

	(defun previous-half-page-lines()
          "Move cursor to previous half-page lines."
          (interactive)
          (forward-line (/ (window-body-height) -2)))
	;; 绑定到快捷键
	(global-set-key (kbd "M-N") 'next-half-page-lines)            ; 光标向下移动 屏幕一半 行
	(global-set-key (kbd "M-P") 'previous-half-page-lines)        ; 光标向上移动 屏幕一半 行

	)
    )
  (if t
      (progn
	(defun scroll-half-page-down ()
          "Scroll down half the page."
          (interactive)
          (scroll-down (/ (window-body-height) 2)))

	(defun scroll-half-page-up ()
          "Scroll up half the page."
          (interactive)
          (scroll-up (/ (window-body-height) 2)))

	(global-set-key "\M-n" 'scroll-half-page-up)
	(global-set-key "\M-p" 'scroll-half-page-down)

	)
    )

  ;; ======================       keybind
  (global-set-key (kbd "C-x 4 o") #'display-buffer)
  (global-set-key (kbd "M-<f3>") #'open-init-file)
  (global-set-key (kbd "C-x ,") #'open-init-file)
  (global-set-key (kbd "C-x ，") #'open-init-file)
  (global-set-key (kbd "C-c C-_") #'comment-or-uncomment-region)
  (global-set-key (kbd "C-c C-/") #'comment-or-uncomment-region)
  (global-set-key (kbd "C-x C-k") #'kill-current-buffer)
  ;; leader key
  (global-set-key (kbd "M-SPC") nil) ;修改默认keybind M-SPC -> nil, 作为leader使用，用于各种命令替代
  (global-set-key (kbd "M-ESC") #'keyboard-quit)
  (global-set-key (kbd "C-j") nil)              ;修改默认的C-j功能，作为编辑的leader key使用
  (global-set-key (kbd "C-j C-j") #'electric-newline-and-maybe-indent);原始的C-j功能修改
  (global-set-key (kbd "ESC ]") #'cycle-spacing) ;原始M-SPC功能修改为
  ;;(global-set-key (kbd "ESC SPC") 'cycle-spacing) ;test ESC SPC leaderkey使用
  (global-set-key (kbd "M-o") #'other-window)
  (global-set-key (kbd "M-SPC c") #'comment-line)
  (global-set-key (kbd "C-j c") #'comment-line)
  (keymap-unset lisp-interaction-mode-map "C-j")
  (keymap-set lisp-interaction-mode-map "C-j C-j" #'eval-print-last-sexp)
  (global-set-key (kbd "C-j C-k") #'kill-whole-line)
  (global-set-key (kbd "C-x C-b") #'ibuffer)
  (global-set-key (kbd "C-,") #'completion-at-point)
  (global-set-key (kbd "M-z") #'zap-up-to-char) ;old func is zap-to-char, diff is with no up del input char.
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-'") #'repeat)
  (global-set-key (kbd "M-SPC C-M-w") #'append-next-kill)
  (global-set-key (kbd "C-M-w") #'yank)
  (global-set-key (kbd "C-M-S-w") #'yank-pop)
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
  (if nil
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
    )
  (defun l/duplicate-line ()
    "Duplicate line, but cursor follow."
    (interactive)
    (let ((column (current-column))
	  (line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	  )
      (end-of-line)
      (newline)
      (insert line)
      (beginning-of-line)
      (forward-char column)
      )
    )
  (defun clear-line ()
    "Clear line."
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    )
  (global-set-key (kbd "C-j C-i") #'newline-and-indent-up)
  (global-set-key (kbd "C-j C-o") #'newline-and-indent-down)
  (global-set-key (kbd "C-j C-d") #'l/duplicate-line)
  (global-set-key (kbd "C-j C-l") #'clear-line)
  (global-set-key (kbd "M-SPC O") #'other-frame)
  (global-set-key (kbd "C-c C-n") #'scratch-buffer)
  ;; ======================      config ui
  (setq use-short-answers t)
  (defun cursor-type-default ()
    "Cursor default."
    (interactive)
    (setq cursor-type '(hbar . 6))
    )
  ;;(cursor-type-default)
					;(setq cursor-type '(hbar . 4));box)       ; 终端不生效  原因不明
  (setq isearch-lazy-count t
	lazy-count-prefix-format "%s/%s ")
  (define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

  ;;(fido-vertical-mode +1)                       ;minibuffer垂直补全  和 orderless冲突
					;(icomplete-vertical-mode +1)         ;minibuffer垂直补全
  (global-hl-line-mode 1)         ;高亮当前行
  (global-tab-line-mode +1)               ;显示tab line 不同的buffer编辑区
  (tab-bar-mode +1)                       ;显示tab bar  相当于不同的工作区
  (column-number-mode +1)                 ;显示行列在buffer区域
  (global-display-line-numbers-mode +1)
  (electric-pair-mode +1)                 ;自动补全括号
  (electric-quote-mode +1)
  (electric-indent-mode +1)
  (electric-layout-mode +1)
  (show-paren-mode +1)                    ;
  ;;(delete-selection-mode +1)              ;选中区域后插入删除选中文字
  (global-auto-revert-mode +1)            ;实时刷新文件

  (eval-after-load "dired"
    '(progn
       ;; (define-key dired-mode-map (kbd "b") #'dired-up-directory)
       (define-key dired-mode-map (kbd "F") #'dired-create-empty-file)
       (define-key dired-mode-map (kbd "b") #'(lambda () (interactive) (find-alternate-file "..")))
       )
    )

  (add-hook 'prog-mode-hook 'hs-minor-mode) ;折叠模式
  (global-set-key (kbd "C-c c c") #'hs-toggle-hiding)
  (global-set-key (kbd "C-c c a") #'hs-hide-all)
  (global-set-key (kbd "C-c c s") #'hs-show-all)
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
  ;; (setq completion-preview-ignore-case t)
  (setq completion-ignore-case t)
  (setq completions-max-height 15)

  (setq completion-preview-completion-styles '(basic initials file))
  ;; (setq completion-preview-completion-styles '(orderless basic initials))
  
  (global-completion-preview-mode +1)
  ;;(completion-preview-active-mode)
  ;; Don't let Emacs hurt my ears.
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)

  (setq scroll-step 1)
  (setq scroll-margin 2)                  ;set next page margin line
  (setq scroll-conservatively 101)        ;if value greater than 100, will nerver scroll
  (setq scroll-preserve-screen-position t)
  (setq resize-mini-windows t)
  (setq max-mini-window-height 1)
  

  ;; Disable fancy features when the file is too large
  (global-so-long-mode t)

  ;; 增加读取缓冲区
  (setq process-adaptive-read-buffering t)

  )


;; config/ builtin repeat mode
(use-package emacs
  :demand t
  :config
  (repeat-mode +1)
  (setq repeat-exit-key (kbd "i"))
  (defun l/tab-line-switch-next-tab ()
    (interactive)
    (tab-line-switch-to-next-tab)
    )
  (defun l/tab-line-switch-prev-tab ()
    (interactive)
    (tab-line-switch-to-prev-tab)
    )
  
  (defvar buffer-lunch-repeat-map ; C-x <left> 或 <right>
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (define-key map (kbd "f") #'forward-char)
      (define-key map (kbd "b") #'backward-char)
      (define-key map (kbd "e") #'previous-line)
      (define-key map (kbd "d") #'next-line)
      (define-key map (kbd "s") #'backward-char)
      (define-key map (kbd "h") #'backward-char)
      (define-key map (kbd "l") #'forward-char)
      (define-key map (kbd "v") #'forward-word)
      (define-key map (kbd "z") #'backward-word)
      (define-key map (kbd "a") #'back-to-indentation)
      (define-key map (kbd "g") #'move-end-of-line)
      (define-key map (kbd "N") #'next-half-page-lines)
      (define-key map (kbd "j") #'next-half-page-lines)
      (define-key map (kbd "P") #'previous-half-page-lines)
      (define-key map (kbd "k") #'previous-half-page-lines)
      (define-key map (kbd "4") #'scroll-up-command)
      (define-key map (kbd "2") #'scroll-down-command)
      (define-key map (kbd "w") #'scroll-half-page-down)
      (define-key map (kbd "r") #'scroll-half-page-up)
      (define-key map (kbd "i") #'repeat-exit)
      (define-key map (kbd "o") #'other-window)
      (define-key map (kbd "O") #'other-frame)
      (define-key map (kbd "m") #'newline)
      (define-key map (kbd "x") #'delete-char)
      (define-key map (kbd "t") #'kill-current-buffer)
      (define-key map (kbd "c") #'comment-line)
      (define-key map (kbd ".") #'xref-find-definitions)
      (define-key map (kbd "/") #'xref-find-references)
      (define-key map (kbd ",") #'xref-go-back)
      (define-key map (kbd ">") #'end-of-buffer)
      (define-key map (kbd "<") #'beginning-of-buffer)
      (define-key map (kbd "0") #'delete-window)
      (define-key map (kbd "[") #'l/tab-line-switch-prev-tab)
      (define-key map (kbd "]") #'l/tab-line-switch-next-tab)
      
      (dolist (it '(next-line previous-line forward-char backward-char forward-word backward-word back-to-indentation move-end-of-line left-word right-word previous-half-page-lines next-half-page-lines scroll-up-command scroll-down-command scroll-half-page-up scroll-half-page-down other-window kill-current-buffer
			      xref-find-definitions xref-find-references xref-go-back comment-line
			      embark-dwim end-of-buffer beginning-of-buffer ace-window delete-window
			      l/tab-line-switch-next-tab l/tab-line-switch-prev-tab))
	(put it 'repeat-map 'buffer-lunch-repeat-map))
      map)
    "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."
    )
  (defvar frame-lunch-repeat-map ; C-x <left> 或 <right>
    (let ((omap (make-sparse-keymap)))
      (define-key omap (kbd "O") #'other-frame)
      
      (dolist (it '(other-frame))
	(put it 'repeat-map 'buffer-lunch-repeat-map))
      omap)
    "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."
    )
  
  )

;; config/ downcase word -> downcase dwim
(use-package emacs
  :config
  ;; dwim
  (global-set-key (kbd "M-l") #'downcase-dwim)
  (global-set-key (kbd "M-u") #'upcase-dwim)
  (global-set-key (kbd "M-c") #'capitalize-dwim)
  ;; Mark
  (setq global-mark-ring-max 50)
  (setq mark-ring-max 50)
  (setq set-mark-command-repeat-pop t)
  ;; Kill
  (setq kill-do-not-save-duplicates t)  ; not save same kill ring
  (setq save-interprogram-paste-before-kill t)  ;; save system copycliboard data into kill ring
  ;; Reverse yank-pop.  By default, it is C-u M-y, but it's not as
  ;; intuitive.  The "Shift reverse" metaphor seems well established.
  (defun yank-pop-last()
    (interactive)
    (yank-pop -1)
    )
  (global-set-key (kbd "M-Y") #'yank-pop-last)
  (global-set-key (kbd "C-M-y") #'yank-pop-last)
  
  ;; (global-set-key (kbd "M-Y") #'(lambda () (interactive) (yank-pop -1)))
  ;; avoid select region cannot copy
  (if l/wsl2
      (setq select-active-regions nil)
      )
  ;; avoid kill no selection region, just kill a word, like vim
  (setq kill-region-dwim 'emacs-word)
  

  )

;; config/ builtin undo
(use-package emacs
  :demand t
  :config
  ;; config undo
  (setq undo-limit (* 100 1024 1024))
  (setq undo-strong-limit (* 200 1024 1024))
  (setq undo-outer-limit (* 50 1024 1024))

  )
;; ============================== third package


;; config/ window move
;; Use ace-window for quick window navigation
;; Sorry, `other-window', but you are too weak!
(use-package ace-window
  :bind (("C-x o" . ace-window)
	 ("C-x M-0" . ace-delete-window)
	 ("C-x M-o" . ace-delete-window)
         ;; ("C-x C-o" . ace-window)
         )  ;; was delete-blank-lines
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0 :background "yellow")))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  )


(use-package vundo
  :bind
  ("C-x u" . vundo)
  )
(use-package undohist
  :init
  (add-hook 'after-init-hook
          #'(lambda ()
              (require 'undohist)
              (undohist-initialize)

              ;; Patch to make undohist silent
              (define-advice undohist-recover-1 (:override ())
                (let* ((buffer (current-buffer))
                       (file (buffer-file-name buffer))
                       (undo-file (make-undohist-file-name file))
                       undo-list)
                  (when (and (undohist-recover-file-p file)
                             (file-exists-p undo-file)
                             (null buffer-undo-list))
                    (with-temp-buffer
                      (insert-file-contents undo-file)
                      (goto-char (point-min))
                      (let ((alist (undohist-decode (read (current-buffer)))))
                        (if (string= (md5 buffer) (assoc-default 'digest alist))
                            (setq undo-list (assoc-default 'undo-list alist))
                          (message "File digest doesn't match, so undo history will be discarded."))))
                    (when (consp undo-list)
                      (setq buffer-undo-list undo-list)))))))
  :config
;;  (setq undohist-directory (no-littering-expand-var-file-name "undohist"))
  (push "\\.git/COMMIT_EDITMSG\\'" undohist-ignored-files)
  (push "dict.yaml\\'" undohist-ignored-files)
  (push "essay.txt\\'" undohist-ignored-files)
  (push tramp-file-name-regexp undohist-ignored-files)
  
  )

(use-package undo-tree
  ;; sorry, vundo more useful.
  :disabled
  )


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode #'rainbow-delemiters-mode)
;;  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package paren
  :init
  :config (setq show-paren-when-point-in-periphery t
                show-paren-when-point-inside-paren t
                show-paren-style 'mixed
                show-paren-delay 0.2
                show-paren-context-when-offscreen 'child-frame
                ;; show-paren-context-when-offscreen 'overlay
                ;; show-paren-context-when-offscreen t
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
  ;; :disabled
  :hook (
	 ;; (minibuffer-setup . highlight-parentheses-minibuffer-setup)
         ;; (prog-mode . highlight-parentheses-mode))
;         (after-init . highlight-parentheses-mode)
         (after-init . global-highlight-parentheses-mode)
         )
  :config
  (setq highlight-parentheses-colors
;       '("firebrick1" "firebrick3" "orange1" "orange3")
        '("firebrick1" "IndianRed1" "firebrick3" "IndianRed3" "IndianRed4")
        highlight-parentheses-attributes
        '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2
        )
  )
;; use to save key statistics
(use-package keyfreq
  :disabled
  :config
  (keyfreq-mode 1)
  )
;; config/ echo key
(setq echo-keystrokes 0.01)
(use-package embark
  :bind
  (
   ("C-." . embark-dwim)
   ("M-SPC ." . embark-act)
   ;; ("C-M->" . embark-act)
   ("C-M-?" . 'embark-act)
   ;; ("C-z" . embark-act)
   ("C-c h b" . embark-bindings)
   ("C-c h B" . embark-bindings-at-point)
   ;;  ("C-h" . embark-prefix-help-command)
   ;;("M-n" . embark-next-symbol)
   ;;("M-p" . embark-previous-symbol)
   (:map embark-general-map
         ("e" . embark-export)
         )
   )
  :custom
  (embark-quit-after-action nil)
  ;;  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")

  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (delete 'embark-target-flymake-at-point embark-target-finders)
  :init
  (defun l/choose-prefix-help-command ()
    "一个交互式选择器函数。
它会提示用户从预定义的选项中选择一项，然后执行对应的动作。"
    (interactive)
    (let* (;; 定义选项列表，每个元素是一个 (DISPLAY-TEXT . FUNCTION) 的 cons cell
           (choices (list (cons "打开embark-help (embark-help-prefix-command)" 'embark-prefix-help-command)
                          (cons "打开emacs-help (describe-prefix-bindings)" 'describe-prefix-bindings)
			  ))
           ;; 使用 completing-read 弹出交互式菜单
           (selection (completing-read
                       "选择一个操作: " ; 提示信息
                       choices           ; 选择列表
                       nil               ; PREDICATE (可选的过滤函数)
                       t                 ; REQUIRE-MATCH (必须选择列表中的项)
                       nil               ; INITIAL-INPUT (初始输入)
                       'my-choices-history ; HISTORY (历史记录的变量名)
                       ))
           ;; 根据用户输入的文本，找到对应的函数
           (chosen-fn (cdr (assoc selection choices))))
      ;; 检查是否选择了有效选项并执行
      (if chosen-fn
          (progn
            (call-interactively chosen-fn)) ; 安全地调用交互式命令
	(message "未选择有效操作"))))
  (setq prefix-help-command #'l/choose-prefix-help-command)
  ;; 可选：将这个函数绑定到一个快捷键上，例如 F12
  ;; (global-set-key (kbd "<f12>") 'my-interactive-selector)

  ;; 可选：将这个函数绑定到一个快捷键上，例如 F12
  ;; (global-set-key (kbd "<f12>") 'my-interactive-selector)
  )

(defun my-interactive-selector ()
  "一个交互式选择器函数。
它会提示用户从预定义的选项中选择一项，然后执行对应的动作。"
  (interactive)
  (let* (;; 定义选项列表，每个元素是一个 (DISPLAY-TEXT . FUNCTION) 的 cons cell
         (choices (list (cons "打开文件 (find-file)" 'find-file)
                        (cons "切换缓冲区 (switch-to-buffer)" 'switch-to-buffer)
                        (cons "列出缓冲区 (list-buffers)" 'list-buffers)
                        (cons "保存文件 (save-buffer)" 'save-buffer)
                        (cons "退出 Emacs (kill-emacs)" 'kill-emacs)))
         ;; 使用 completing-read 弹出交互式菜单
         (selection (completing-read 
                     "选择一个操作: " ; 提示信息
                     choices           ; 选择列表
                     nil               ; PREDICATE (可选的过滤函数)
                     t                 ; REQUIRE-MATCH (必须选择列表中的项)
                     nil               ; INITIAL-INPUT (初始输入)
                     'my-choices-history ; HISTORY (历史记录的变量名)
                     ))
         ;; 根据用户输入的文本，找到对应的函数
         (chosen-fn (cdr (assoc selection choices))))
    ;; 检查是否选择了有效选项并执行
    (if chosen-fn
        (progn
          (message "执行: %s" selection)
          (call-interactively chosen-fn)) ; 安全地调用交互式命令
      (message "未选择有效操作")))
  )

(use-package embark-consult
  :after (embark consult)
  )
(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  )
(use-package which-key
  :disabled
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
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
  (setq orderless-component-separator "[ &]")
  (setq orderless-matching-styles '(orderless-literal
				    ;; orderless-flex
                                    orderless-regexp
                                    )
        )
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        )
  
  (setq completion-preview-completion-styles '(orderless basic initials))
  
  )
(use-package hotfuzz
  :after orderless
  :init
  (setq completion-styles '(orderless hotfuzz basic))
  ;; (setq completion-category-defaults nil)
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
   ("M-SPC m" . consult-mark)
   ("M-SPC g m" . consult-global-mark)
   ("M-SPC g s" . consult-line-multi)
   ("M-SPC g i" . consult-imenu-multi)
   ("M-SPC b" . consult-buffer)
   ("M-SPC i" . consult-imenu)
   ("M-SPC e" . consult-flymake)
   ("M-SPC r r" . consult-register)
   ("M-SPC r s" . consult-register-store)
   ("M-SPC r l" . consult-register-load)
   
   )
  :config
  (setq consult-async-refresh-delay 0.5)
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  )
(use-package vertico
  :init
  (vertico-mode +1)
  (setq vertico-multiform-commands
	'((consult-imenu buffer indexed)
	  (consult-line buffer)
	  (consult-line-multi buffer)))
  (setq vertico-multiform-categories
	'((consult-grep buffer)))
  (vertico-multiform-mode t)
  )
(use-package posframe
  )
(use-package vertico-posframe
  :disabled
  :after (vertico posframe)
  :init
  (vertico-posframe-mode 1)
  )
;(vertico-posframe-mode +1)


(use-package company
  ;; :disabled
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (global-completion-preview-mode -1)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (global-set-key (kbd "C-,") #'company-complete)
  (global-set-key (kbd "C-M-i") #'completion-at-point)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-offset-display 'lines)
  (setq company-frontends
	'(company-preview-frontend company-echo-metadata-frontend company-pseudo-tooltip-frontend)
	)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)

  (define-key company-active-map (kbd "RET") #'newline)
  (define-key company-active-map (kbd "<return>") #'newline)
  (add-to-list 'company-backends 'company-ispell)
  

  (setq company-files-exclusions '(".git/" ".DS_Store"))
  (setq company-abort-manual-when-too-short t)
  
  (setq company-transformers '(
			       ;; delete-consecutive-dups
			       company-sort-prefer-same-case-prefix
                               ;; company-sort-by-occurrence
			       ))
  (defun insert-black ()
    (interactive)
    (insert " ")
    )
  (define-key company-active-map (kbd "M-SPC") #'insert-black)
  )

(use-package corfu
  :disabled
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-deply 0.1)
  (corfu-min-width 3)
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
   ;;   ("SPC" . 'corfu-insert-separator) ; set spc use in complete

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
  :after corfu
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
 ;; (setf super-cape (cape-capf-super #'cape-abbrev #'cape-elisp-block #'cape-elisp-symbol #'cape-file #'cape-emoji #'cape-dabbrev #'cape-sgml #'cape-sgml #'cape-tex #'cape-line #'cape-keyword))
  (setq completion-at-point-functions (list
                                       (cape-capf-super #'cape-abbrev #'cape-elisp-block #'cape-elisp-symbol #'cape-file #'cape-emoji #'cape-dabbrev #'cape-sgml #'cape-sgml #'cape-tex #'cape-line #'cape-keyword)
                                       'tags-completion-at-point-function
                                       ))
;;;;  (add-hook 'completion-at-point-functions #'super-cape)
  ;; (add-hook 'completion-at-point-functions #'cape-line)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;;; (add-hook 'completion-at-point-functions #'cape-history)
  
  ;; ...
  )


(use-package nerd-icons
  ;;:disabled
  :demand t
  :config
  ;; Fonts for nerd-icons need to be configured in graphical frames.
  (message "xxxxx nerd icon")
  (defun +nerd-icons--after-make-frame-h (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      ;;  `framep' returns t on terminal
      (unless (memq (framep (selected-frame)) '(t))
        (require 'nerd-icons)
        (nerd-icons-set-font))))
  (add-hook 'after-make-frame-functions '+nerd-icons--after-make-frame-h)
  (add-hook 'server-after-make-frame-hook '+nerd-icons--after-make-frame-h)

  ;; show nerd-icons on mode-line
  (setq-default mode-line-buffer-identification
                (seq-union '(
                             (:eval (nerd-icons-icon-for-buffer)) " "
                             )
                           mode-line-buffer-identification)
                )
  )
(use-package nerd-icons-grep
  :after nerd-icons
  :config
  (add-hook 'grep-mode-hook #'nerd-icons-grep-mode)
  )
(use-package nerd-icons-xref
  :after nerd-icons
  :config
  (add-hook 'xref--xref-buffer-mode-hook #'nerd-icons-xref-mode )
  (add-hook 'xref-etags-mode-hook #'nerd-icons-xref-mode)
  )
(use-package nerd-icons-dired
  :disabled
  :after nerd-icons
  :config
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  )
(use-package nerd-icons-ibuffer
  :after nerd-icons
  :config
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  )

(use-package nerd-icons-corfu
  :after nerd-icons
  :config
  (add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )
(use-package tab-line-nerd-icons
  ;;:disabled
  :after nerd-icons
  :config
  (tab-line-nerd-icons-global-mode t)
  )
(use-package nerd-icons-completion
  :after nerd-icons
  :config
  (nerd-icons-completion-mode t)
  )

(use-package nerd-icons-multimodal
  :load-path "lib/nerd-icons-multimodal/"
  :config
  (global-nerd-icons-multimodal-mode t)
  )
(use-package compile-multi-nerd-icons
  :disabled
  :after nerd-icons
  )


(use-package flymake
  :ensure nil
  :init
  (add-hook 'prog-mode-hook 'flymake-mode)
  (setq flymake-show-diagnostics-at-end-of-line t)
  (setq flymake-no-changes-timeout 0.9)
  (add-to-list 'elisp-flymake-byte-compile-load-path load-path)
  :bind
  (:map flymake-mode-map
   ("C-j C-n" . 'flymake-goto-next-error)
   ("C-j C-p" . 'flymake-goto-prev-error)
   )
  )

;; ====================     term
(use-package eat
;;  :disabled
  ;;  :if (not window-system)
  :if (or l/mac l/linux)
  :defer t
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
;;  :disabled
  )
(use-package magit
  ;; :disabled
  :defer t
  :bind
  ("C-j g" . #'magit)
  :config
  ;; (global-set-key (kbd "C-j g") #'magit)
  )
(use-package eglot
  :ensure nil
                                        ;  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format)
  :config
  (defun jdtls-command-contact (&optional interactive)
    (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
           (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
           (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
           ;; lombok 版本不能过低，会导致 dape 启动 lombok 不能加载。
           (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
                       "-Xmx8G"
                       ;; "-XX:+UseG1GC"
                       "-XX:+UseZGC"
                       "-XX:+UseStringDeduplication"
                       ;; "-XX:FreqInlineSize=325"
                       ;; "-XX:MaxInlineLevel=9"
                       "-XX:+UseCompressedOops"))
           (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
           ;; tell jdtls the data directory and jvm args
           (contact (append '("jdtls")
                            jvm-args
                            `("-data" ,data-dir)
                            `(:initializationOptions
                              (:bundles
                               [,(file-truename "~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.0/com.microsoft.java.debug.plugin-0.53.0.jar")])))
                    ))
      contact)
    )

  ;; (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)

  )
;;(message "%s" eglot-java-eclipse-jdt-args)
(use-package eglot-java
  :after eglot
  ;; :init
  :config
  ;;(setq lombok-path  (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
  ;; (add-to-list 'eglot-java-eclipse-jdt-args (concat "-javaagent:" lombok-path) 'append)
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
;;(setq package-quickstart t)

(use-package server
  ;; :disabled
  :if (or l/linux l/mac)
  ;;  window-system
  ;;     :commands (server-running-p)
  :commands (server-running-p)
  :init
  (unless (server-running-p) 
    (progn
      (server-mode +1)
      (message "Emacs Server …DONE")
      )
    )
  )
;;; config/ message window always in last
(use-package emacs
  :config
  (defun my-messages-buffer-auto-tail (&rest args)
    "Make *Messages* buffer auto-scroll to the end after each message."
    (let* ((buf-name "*Messages*")
           (buf (get-buffer buf-name)))
      (when (and buf (window-live-p (get-buffer-window buf)))
        ;; If *Messages* buffer is currently visible in a window,
        ;; move its point to the end.
        (dolist (win (get-buffer-window-list buf-name nil :all-frames))
          (unless (and win (eq (selected-window) win))
            (with-selected-window win
              (goto-char (point-max)))
            )
          )
        )
      )
    )

  ;; Advice the 'message' function to always jump to the end of *Messages* buffer
  (advice-add 'message :after #'my-messages-buffer-auto-tail)
  )
;; set font
(if t
    (progn
      ;; (setq line-spacing 0.1)
      (let (
            (use-font (font-spec :family "ubuntu mono" :size 16))
	    (use-font-new (font-spec :family "Fantasque Sans Mono" :size 16))
            (use-font-l (font-spec :family "ubuntu mono Ligaturized" :size 16))
            (last-font (font-spec :family "noto sans sc"))
	    (last-font-bak (font-spec :family "noto sans"))
            (symbol-font (font-spec :family "Segoe UI symbol"))
            (han-font (font-spec :family  "lxgw wenkai"))
	    (han-font-bak (font-spec :family "文泉驿微米黑"))
            ;;(han-font-sarasa (font-spec :family "sarasa gothic cl"))
            )
	(set-face-attribute 'default nil :font (font-spec :family "Fantasque Sans Mono" :size 16 ))
	;; (set-face-attribute 'default nil :font (font-spec :family "ubuntu mono" :size 16))
	;; (set-face-attribute 'default nil :font (font-spec :family "neko mono" :size 16))
	;; (default-line-height)
        (if (find-font use-font-new)
            (set-face-attribute 'default nil :font use-font-new )
	  (if (find-font use-font)
	      (set-face-attribute 'default nil :font use-font)
            (if (find-font use-font-l)
		(set-face-attribute 'default nil :font use-font-l)
              )
	    )
          )
        (if (find-font last-font)
	    (progn
	      (set-fontset-font (frame-parameter nil 'font) nil last-font)
	      (message "test2222")
	      )
          )
	
        (if (find-font last-font-bak)
	    (progn
	      (set-fontset-font (frame-parameter nil 'font) nil last-font-bak)
	      (message "testtest")
	      )
          )
	
        (if (find-font symbol-font)
            (set-fontset-font t nil symbol-font)
          )
        (if (find-font han-font)
            (progn
	      ;; ’prepend 不生效
              (set-fontset-font "fontset-default" 'han han-font nil 'prepend)
              (setq face-font-rescale-alist '(
                                              ("lxgw" . 1.0)
                                              ("ubuntu" . 1.0)
                                              )
                    )
              (setq line-spacing 0.00)
	      (message "test1")
              )
	  (if (find-font han-font-bak)
	      (set-fontset-font "fontset-default" 'han han-font-bak nil 'prepend)
	      )
          )
	(if nil
	    (if (find-font han-font-bak)
		(progn
		  ;; (set-fontset-font "fontset-default" 'han han-font-bak nil 'prepend)
		  ;; (set-fontset-font "fontset-default" 'han han-font-bak nil )
		  ;; (message "test")(frame-parameter nil 'font)
		  ;; (set-fontset-font "fontset-default" 'han (font-spec :family "文泉驿微米黑") nil 'prepend)
		  ;; (set-fontset-font (frame-parameter nil 'font) 'han (font-spec :family "文泉驿微米黑") nil 'prepend)
		  ;; (set-fontset-font t 'han (font-spec :family "文泉驿微米黑") nil 'prepend)
		  ;; (set-fontset-font "fontset-default" 'han (font-spec :family "文泉驿等宽微米黑") nil 'append)(default-line-height)(frame-parameter nil 'font)
		  ;; (set-face-attribute 'default nil :font (font-spec :family "文泉驿等宽微米黑") )
		  (set-fontset-font "fontset-default" '(#x5c31 . #x5c31) (font-spec :family "SJLishu") nil)
		  (set-fontset-font "fontset-default" '(#x5c31 . #x5c31) (font-spec :family "FengSSQLS") nil)
		  )
	      )
	  )
        ;;      (if (find-font han-font-sarasa)
        ;;          (set-fontset-font "fontset-default" 'han han-font-sarasa nil 'prepend)
        ;;        )
        )
      )  
  )
;; config/ mode line
(if nil
    ;;(boundp repeat-in-progress)
    (progn
      (defun l/repeat-mode-curstate ()
        (when repeat-in-progress
	  "🅡🅡")
	)
      (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (l/repeat-mode-curstate))))
      )
  )
(custom-face-attributes-get 'mode-line-active nil)
(custom-face-attributes-get 'mode-line-inactive nil)
(custom-face-attributes-get 'mode-line nil)

(if (boundp 'repeat-in-progress)
    (progn
      ;; save current
      (setq l/store-custom-face-mode-line-active (custom-face-attributes-get 'mode-line-active nil))

      (defun l/change-mode-line-in-repeat-mode (&rest args)
	(set-face-attribute 'mode-line-active nil
			    :foreground "grey90")
	)
      (defun l/change-mode-line-not-repeat-mode ()
	(set-face-attribute 'mode-line-active nil
			    :foreground "black")
	)
      ;; (add-hook 'post-command-hook #'l/change-custom-face-mode-line-active-by-repeatmap)
      ;;(advice-add 'repeat-get-map :before 'l/change-custom-face-mode-line-active-by-repeatmap)
      ;;(advice-remove 'repeat-get-map 'l/change-custom-face-mode-line-active-by-repeatmap)
      ;;(add-hook 'repeat-pre-hook 'l/change-mode-line-in-repeat-mode)
      (advice-add (symbol-value 'repeat-echo-function) :after 'l/change-mode-line-in-repeat-mode)
      (advice-add 'repeat-exit :after #'l/change-mode-line-not-repeat-mode)
      ;; (advice-remove 'repeat-get-map 'l/change-custom-face-mode-line-active-by-repeatmap)
      
      )
  
  )
;; syntax 
(use-package rime
  :if (or l/wsl2)
  :config
  (setq default-input-method "rime")
  ;; nix 系统安装librime 会导致报错，其他系统也可能这样
  (setq rime-share-data-dir "~/.local/share/rime")
  ;; 防止没有文件
  (unless (file-exists-p rime-share-data-dir)
    (make-directory rime-share-data-dir)
    )
  )
(use-package phi-search
  ;; use consult line
  :disabled
  :if (or l/wsl2)
  :config
  (global-set-key [remap isearch-forward] #'phi-search)
  (global-set-key [remap isearch-backward] #'phi-search-backward)
  )

(use-package telega
  ;;  :disabled
  :if (or l/linux l/mac)
  :init (setq telega-proxies
	      '((:server "localhost"
			 :port "7897"
                         :enable t
                         ;; :type (:@type "proxyTypeSocks5"))
                         :type (:@type "proxyTypeHttp"))
		)
              ;;telega-chat-show-avatars nil
	      )
  :config
  ;; comment because homebrew’s tdlib is v1.8.0 less than need version v1.8.4
;;  (if nil ;l/linux
;;      (setq telega-server-libs-prefix "/home/linuxbrew/.linuxbrew/opt/tdlib/")
;;      )
  )

;; Sublime-like multiple cursors.
(use-package multiple-cursors
;;  :disabled
  ;;:bind ("S-M-<mouse-1>" . mc/add-cursor-on-click)
  :bind ("S-M-<down-mouse-1>" . mc/add-cursor-on-click)
  :bind ("C->" . 'mc/mark-next-like-this)
  :bind ("C-<" . 'mc/mark-previous-like-this)
  :bind ("C-M->" . 'mc/mark-next-like-this)
  :bind ("C-M-<" . 'mc/mark-previous-like-this)
  )

;; translate
(use-package gt
  :config
  (setq gt-preset-translators
	`((ts-1 . ,(gt-translator
                    ;; :taker (gt-taker :langs '(en zh) :text 'word :prompt 'buffer)
                    :taker (gt-taker :langs '(en zh) :text 'word :prompt nil)
                    ;; :engines (gt-youdao-dict-engine)
		    :engines
		    (list
		     ;;(gt-google-engine :if 'word)                        ; 只有当翻译内容为单词时启用
                     ;; (gt-bing-engine :if '(and not-word parts))            ; 只有翻译内容不是单词且是多个段落时启用
		     (gt-bing-engine :if '(and not-word))                  ; 只有翻译内容不是单词时启用
		     ;;(gt-deepl-engine :if 'not-word :cache nil)          ; 只有翻译内容不是单词时启用; 不缓存
		     (gt-youdao-dict-engine :if '(or src:zh tgt:zh))       ; 只有翻译中文时启用
		     ;; (gt-deepl-engine :if '(and word))
		     (gt-youdao-suggest-engine :if '(and word src:en))     ; 只有翻译英文单词时启用
		     )
                    ;; :render (gt-overlay-render))
		    :render (gt-buffer-render)
		    ;; :render (gt-posframe-pop-render)
		    )
		)
          (ts-2 . ,(gt-translator
                    :taker (gt-taker :langs '(en fr ru) :text 'sentence)
                    :engines (gt-google-engine)
                    :render (gt-insert-render)))
          (ts-3 . ,(gt-translator
                    :taker (gt-taker :langs '(en zh) :text 'buffer
                                     :pick 'word :pick-pred (lambda (w) (length> w 6)))
                    :engines (gt-google-engine)
                    :render (gt-overlay-render :type 'help-echo))))
	)
  (global-set-key (kbd "M-SPC t") #'gt-translate)
  )

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package treemacs
  :config
  (add-hook 'treemacs-mode-hook #'(lambda ()
				   (treemacs-git-mode 'deferred))
	    )
  (global-set-key (kbd "M-SPC w") #'treemacs-select-window)
  ;;(setq treemacs-indent-guide-style 'block)
  ;;(add-hook 'treemacs-mode-hook 'treemacs-indent-guide-mode)
  )

(use-package wgrep
  :bind
  (:map grep-mode-map
	("C-c C-p" . wgrep-change-to-wgrep-mode)
	("e" . wgrep-change-to-wgrep-mode)
	)
  :config
  (add-to-list 'grep-find-ignored-files ".tag*")
  (add-to-list 'grep-find-ignored-files ".TAG*")
  (add-to-list 'grep-find-ignored-files "tag*")
  (add-to-list 'grep-find-ignored-files "TAG*")

  )

;;; 
;;; ui
(use-package emacs
  :ensure nil
  ;;:demand t
  :config
  ;; (setq-default   header-line-format   '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)))) )   )
  (setq which-func-display 'header)
  ;; (add-hook 'prog-mode-hook #'which-function-mode)
  ;; (which-function-mode t)
  )
(defun l/clean-window-element (window)
  "Disable some ui.
WINDOW use to change"
  (interactive)
  (with-selected-window window
    (setq-local header-line-format nil)
    (tab-line-mode -1)
    )
  window)
(defun l/clean-window-element-clean-header_line (window)
  "Test.  WINDOW is use to change.  CLEAN-FUNC-LIST is CHANGE window func."
  (interactive)
  (with-selected-window window
    (setq-local header-line-format nil)
    )
  )
(defun l/display-buffer-in-side-window-action-clean-header_line (buffer alist)
  "BUFFER.  ALIST."
  (let ((win (display-buffer-in-side-window buffer alist)))
    (when win
      (l/clean-window-element-clean-header_line win)
      )
    win)
  )

(defun l/display-buffer-in-side-window-action (buffer alist)
  "BUFFER buffer.
ALIST next list args"
  (let ((win (display-buffer-in-side-window buffer alist)))
    (when win
      ;; (l/clean-window-elementa win '((lambda () (setq-local header-line-format nil))))
      (l/clean-window-element win)
      ;; (with-selected-window win (read-only-mode 1))
      )
    win)
  )
(defun l/display-buffer-reuse-window-action (buffer alist)
  "BUFFER buffer.
ALIST next list args"
  (let ((win (display-buffer-reuse-window buffer alist)))
    (when win
      (l/clean-window-element win)
      ;; (with-selected-window win (read-only-mode 1))
      )
    win)
  )

(defun l/display-buffer-at-bottom-window-action (buffer alist)
  "BUFFER buffer.
ALIST next list args"
  (let ((win (display-buffer-at-bottom buffer alist)))
    (when win
      (l/clean-window-element win)
      ;; (with-selected-window win (read-only-mode 1))
      )
    win)
  )

(setq
 display-buffer-alist
 '(   
   ("^\\(magit-process.*\\)"                            ;正则匹配buffer name
    (display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     display-buffer-in-side-window)
    (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    (window-width . 0.4)                     ;emacs会自动把这个设置到window-parameter里
    ;; (window-height . 0.3)                   ;同上
    (slot . -10)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line . none)
     ;; (no-other-window . t)
     ))
   ("^\\(magit-diff.*\\)"                            ;正则匹配buffer name
    (display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     display-buffer-use-some-window)
    (side . left)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    ;;(window-width . 0.5)                     ;emacs会自动把这个设置到window-parameter里
    ;; (window-height . 0.2)                   ;同上
    (slot . 0)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    ;;(post-command-select-window . visible)
    ;;(body-function . l/clean-window-element)
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     ;; (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     ;; (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
     ))
   ("^\\(magit.*\\)"                            ;正则匹配buffer name
    (display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     display-buffer-in-side-window)
    (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    (window-width . 0.4)                     ;emacs会自动把这个设置到window-parameter里
    (window-height . 0.6)                   ;同上
    (slot . -10)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    ;;(post-command-select-window . visible)
    ;;(body-function . l/clean-window-element)
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     ;; (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     ;; (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
     ))
   ("^\\(\\*[Hh]elp.*\\)"                            ;正则匹配buffer name
    (l/display-buffer-reuse-window-action             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     l/display-buffer-in-side-window-action-clean-header_line)
    (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    ;;(window-width . 0.5)                     ;emacs会自动把这个设置到window-parameter里
    (window-width . 0.4)
    (window-height . 0.3)                   ;同上
    (slot . 2)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    ;;(post-command-select-window . visible)
    ;;(body-function . l/clean-window-element)
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
     ))
   ("^\\(\\*Warnings\\*\\)\\|\\(\\*Messages\\*\\)"
    (l/display-buffer-reuse-window-action l/display-buffer-in-side-window-action-clean-header_line)
    (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    (window-width . 0.4)                     ;emacs会自动把这个设置到window-parameter里
    (window-height . 0.3)                   ;同上
    (slot . 2)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
     )
    )
   ("^\\(\\*gt-result\\*\\)"
    (l/display-buffer-reuse-window-action l/display-buffer-in-side-window-action)
    (slot . 10)
    (post-command-select-window . visible)
    (window-parameters
     (quit . t)
     )
    )
   ("^\\(\\*vterm\\*\\)"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4)
    (post-command-select-window . visible)
    )
   ;;fallback
   ("^\\(\\*.*\\*\\)"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4)
    )
   )
  
 )


(defun redisplay-buffer-bottom ()
  (display-buffer "*Messages*")
  (display-buffer "*Help*")
  )
;; (redisplay-buffer-bottom)
(setq eldoc-echo-area-prefer-doc-buffer nil)
;;(setq eldoc-echo-area-use-multiline-p t)
(use-package eldoc-box
  :init
  (add-hook 'prog-mode-hook 'eldoc-box-hover-mode)
  ;;(eldoc-box-hover-at-point-mode t)
  
  :config
  ;;(setq eldoc-box-hover-display-frame-above-point t)
  
  )

(use-package bufferlo
  :config
  (global-set-key (kbd "C-x C-b") #'bufferlo-ibuffer)
  (global-set-key (kbd "C-x b") #'bufferlo-find-buffer-switch) ;different of bufferlo-find-buffer which this suffix switch is can choose some hide buffer and select it.
  )
;; (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button))
;;(custom-face-attributes-get 'mode-line-active (frame-focus))
;;(custom-face-attributes-get 'mode-line (frame-focus))
;;(set-face-attribute 'mode-line nil		    :background "blue")
;;(info "(use-package)")
(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(setq org-indent-mode-turns-on-hiding-stars nil)

(keymap-unset org-mode-map "C-j")
(keymap-set org-mode-map "C-j C-j" #'org-return-and-maybe-indent)

(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package xclip
  :disabled
  :init
  (add-hook 'tty-setup-hook #'xclip-mode)
  )
(use-package clipetty
  :ensure t
  ;; :hook (after-init . global-clipetty-mode)
  :config

  (defun l/clipetty-copy ()
    (interactive)
    (if (display-graphic-p)
	(clipboard-kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (region-beginning) (region-end))
      )
    )
  (defun l/clipetty-cut ()
    (interactive)
    (if (display-graphic-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (kill-region (region-beginning) (region-end))
      )
    )
  (defun l/clipetty-kill()
    (interactive)
    
    (when (use-region-p)
      (if clipetty-mode
	  (l/clipetty-cut)
	(clipetty-mode)
	(l/clipetty-cut)
	(clipetty-mode 0)
	)
      )
    )
  (defun l/clipetty-kill-ring-save ()
    (interactive)
    (when (use-region-p)
      (if clipetty-mode
          (l/clipetty-copy)
	(clipetty-mode)
	(l/clipetty-copy)
	(clipetty-mode 0)))
    )
  (global-set-key (kbd "C-w") #'l/clipetty-kill)
  (global-set-key (kbd "M-w") #'l/clipetty-kill-ring-save)
  (global-set-key (kbd "C-y") #'clipboard-yank)
  
  (setq x-select-enable-clipboard nil)
  )
(use-package run-stuff
  )
(use-package drag-stuff
  :config
  (global-set-key (kbd "M-<up>") #'drag-stuff-up)
  (global-set-key (kbd "M-<down>") #'drag-stuff-down)
  )


(setq fringes-outside-margins t)
(setq kill-whole-line nil)
(setq indicate-buffer-boundaries 'right)
(setq delete-by-moving-to-trash t)
(setq window-combination-resize t)
(setq x-stretch-cursor t)
(setq track-eol t)
(setq next-line-add-newlines nil)
(setq dired-listing-switches "-vhal")
(save-place-mode t) 			;保存上次光标位置
(global-set-key (kbd "<f5>") #'redraw-display)
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name) (get-char-property (pos) 'face)))) (if face (message "Face: %s" face) (message "No face at %d" pos)))
  )
(use-package emacs
  :load-path nil
  :ensure nil
  :config
  ;; (set-char-table-range char-width-table ?\uFE0F 0)
  ;; (set-char-table-range char-width-table ?⚡ 2)
  (set-char-table-range char-width-table ?’ 1)
  (set-char-table-range char-width-table ?‘ 1)
  (set-char-table-range char-width-table ?“ 1)
  (set-char-table-range char-width-table ?” 1)
  (set-char-table-range char-width-table ?° 1)
  (set-char-table-range char-width-table ?— 1)
  (set-char-table-range char-width-table ?… 1)

  ;; (tty-type)
  ;; (or standard-display-tableq
  ;; (setq standard-display-table (make-display-table)))
  ;; (aset standard-display-table
  ;; #xAD (vector (make-glyph-code ?- 'escape-glyph)))
  ;; (aset standard-display-table
  ;; #x1f64f (vector (make-glyph-code #xFFFD 'escape-glyph)))
  ;; (setq auto-composition-mode (tty-type))
  ;; (global-auto-composition-mode -1)
  ;; (add-hook 'auto-composition-mode-hook #'(lambda ()
  ;; (if (and (not (display-graphic-p)) (symbol-value auto-composition-mode))
  ;; (auto-composition-mode -1)
  ;; )
  ;; ))
  (add-hook 'tty-setup-hook #'(lambda () (global-auto-composition-mode -1)))

  )

;; (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
(use-package breadcrumb
  :init
  (breadcrumb-mode 1)
  :config
  ;; (add-to-list 'tab-bar-format '(:eval (breadcrumb--header-line)) t)
  ;; (add-to-list 'tab-bar-format 'breadcrumb--header-line t)
  )
(defun tab-bar-local-format ()
  "A."
  (let ((name (buffer-file-name)))
    (if name
	(propertize (abbreviate-file-name name) 'face (list :foreground "blue"))
      (buffer-name)
      )
    )
  )
(defun tab-bar-local-sperator()
  "A."
  "    ["
  )
(defun tab-bar-local-sperator-2()
  "A."
  "]      "
  )
(defun tab-bar-mode-line ()
  "A."
  ;; (format-mode-line mode-line-format)
  (let ((ml-str (format-mode-line mode-line-format))
	)
    ;; (propertize " "
    ;; 'display `(margin right-margin			  
    ;; ,
    ;; (concat 
    (propertize
     ;; (concat
		 ;; (propertize " " 'display `((space :align-to (-  (+ right right-margin right-fringe 1)  ,(string-width ml-str)))))
		 ml-str
		 ;; )
		'face (list :background "gray75"
			    :foreground "black")
		)
    ;; spacer
    ;; )
    
    ;; (face-attribute 'mode-line :background)
    ;; (face-attribute 'mode-line :foreground)
    )
  )

(add-to-list 'tab-bar-format 'tab-bar-local-sperator t)
(add-to-list 'tab-bar-format 'tab-bar-local-format t)
(add-to-list 'tab-bar-format 'tab-bar-local-sperator-2 t)
(add-to-list 'tab-bar-format 'tab-bar-mode-line t)


(provide 'init)
;;; init.el ends here

;; (put 'dired-find-alternate-file 'disabled nil)
