;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
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
;; ------------------- Common consts
(defconst l/mac (eq system-type 'darwin))
(defconst l/windows (eq system-type 'windows-nt))
(defconst l/linux (eq system-type 'gnu/linux))
(defconst l/wsl2 (string-match-p "WSL2" (shell-command-to-string "uname -r")))
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
(setq confirm-kill-emacs 'yes-or-no-p)
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
(setq process-adaptive-read-buffering t)
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
(global-set-key (kbd "C-z") #'repeat)
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
  "Clear line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  )
(global-set-key (kbd "C-j C-i") #'newline-and-indent-up)
(global-set-key (kbd "C-j C-o") #'newline-and-indent-down)
(global-set-key (kbd "C-j C-d") #'duplicate-line)
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
;; avoid kill no selection region, just kill a word, like vim
(setq kill-region-dwim 'emacs-word)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "b") #'dired-up-directory)
     (define-key dired-mode-map (kbd "F") #'dired-create-empty-file)
     )
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
;;(completion-preview-active-mode)
;; Don't let Emacs hurt my ears.
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq scroll-step 1)
(setq scroll-margin 2)                  ;set next page margin line
(setq scroll-conservatively 101)        ;if value greater than 100, will nerver scroll
(setq scroll-preserve-screen-position t)
(setq resize-mini-windows t)
(setq max-mini-window-height 5)
                                        


;; Disable fancy features when the file is too large
(global-so-long-mode t)

(repeat-mode +1)
(setq repeat-exit-key (kbd "i"))
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
    
    (dolist (it '(next-line previous-line forward-char backward-char forward-word backward-word back-to-indentation move-end-of-line left-word right-word previous-half-page-lines next-half-page-lines scroll-up-command scroll-down-command scroll-half-page-up scroll-half-page-down other-window kill-current-buffer
			    xref-find-definitions xref-find-references xref-go-back comment-line))
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
;; config/ window move
;; Use ace-window for quick window navigation
;; Sorry, `other-window', but you are too weak!
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ;; ("C-x C-o" . ace-window)
         )  ;; was delete-blank-lines
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))
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
  (global-set-key (kbd "M-Y") #'(lambda () (interactive) (yank-pop -1)))
  )

;; config undo
(setq undo-limit (* 100 1024 1024))
(setq undo-strong-limit (* 200 1024 1024))
(setq undo-outer-limit (* 50 1024 1024))
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
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package paren
  :init
  :config (setq show-paren-when-point-in-periphery t
                show-paren-when-point-inside-paren t
                show-paren-style 'mixed
                show-paren-delay 0.2
                ;;              show-paren-context-when-offscreen 'child-frame
                show-paren-context-when-offscreen t
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
  (:map embark-general-map
        ("e" . embark-export)
        )
  ("C-." . embark-act)
  ("M-SPC ." . embark-dwim)
  ("C-c h b" . embark-bindings)
  ("C-c h B" . embark-bindings-at-point)
  ;;  ("C-h" . embark-prefix-help-command)
  ;;("M-n" . embark-next-symbol)
  ;;("M-p" . embark-previous-symbol)
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
  (setq orderless-matching-styles '(orderless-literal
;;                                  orderless-flex
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


;; key bind mode
(use-package meow
  :disabled
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
   '("b" . meow-right)
   '("B" . clear-line)
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
   '("n" . meow-next)
   ;; '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   ;; '("p" . meow-yank)
   '("p" . meow-prev)
   '("P" . meow-yank)
   ;; '("q" . meow-quit)
   '("q" . meow-cancel-selection)
   '("Q" . meow-quit)
   ;; '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-left)
   '("S" . meow-left-expand)
   ;; '("s" . meow-kill)
   '("t" . meow-find)
   '("T" . meow-till)
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
   '("<escape>" . ignore)
   '("/" . meow-visit)
   '("?" . meow-search)
   '(":" . execute-extended-command)
   )
   (setq
    meow-cursor-type-normal 'hbar
    meow-cursor-type-insert '(bar . 4)
    meow-expand-hint-remove-delay 60.0
    )
   (add-to-list 'meow-mode-state-list '(ement-room-mode . insert))
   )
  (defun meow-setup-modeline ()
      (setq meow-replace-state-name-list
        '((normal . "🅝 NORMAL")
          (beacon . "🅑 BEACON")
          (insert . "🅘 INSERT")
          (motion . "🅜 MOTION")
          (keypad . "🅚 KEYPAD"))
        )
    )
  (meow-setup)
  (meow-setup-modeline)
  ;(meow-setup-line-number)
  (meow-setup-indicator)
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
    (meow-setup-indicator)
    )
  (defun meow-disable ()
    "Disable meow."
    (interactive)
    (meow-global-mode -1)
    (cursor-type-default)
    )
  (global-set-key (kbd "C-'") 'meow-enable)
  (global-set-key (kbd "C-.") 'meow-disable)
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
  :config
  (global-set-key (kbd "C-j g") #'magit)
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

  (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)

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
(setq package-quickstart t)

(use-package server
  :disabled
  :if window-system
;     :commands (server-running-p)
     :init
  (progn
      (server-mode +1)
      (message "Emacs Server …DONE")
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
            (use-font-l (font-spec :family "ubuntu mono Ligaturized" :size 16))
            (last-font (font-spec :family "noto sans sc"))
            (symbol-font (font-spec :family "Segoe UI symbol"))
            (han-font (font-spec :family  "lxgw wenkai"))
            ;;(han-font-sarasa (font-spec :family "sarasa gothic cl"))
            )
        (if (find-font use-font)
            (set-face-attribute 'default nil :font use-font )
          (if (find-font use-font-l)
              (set-face-attribute 'default nil :font use-font-l)
            )
          )
        (if (find-font last-font)
            (set-fontset-font (frame-parameter nil 'font) nil last-font)
          )
        (if (find-font symbol-font)
            (set-fontset-font t nil symbol-font)
          )
        (if (find-font han-font)
            (progn
              (set-fontset-font "fontset-default" 'han han-font nil 'prepend)
              (setq face-font-rescale-alist '(
                                              ("lxgw" . 1.0)
                                              ("ubuntu" . 1.0)
                                              )
                    )
              (setq line-spacing 0.00)
              )
          )
        ;;      (if (find-font han-font-sarasa)
        ;;          (set-fontset-font "fontset-default" 'han han-font-sarasa nil 'prepend)
        ;;        )
        )
      )
  )
;; config/ mode line
(if (boundp repeat-in-progress)
    (progn
      (defun l/repeat-mode-curstate ()
        (when repeat-in-progress
	  "🅡")
	)
      (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (l/repeat-mode-curstate))))
      )
  )
(or repeat-mode)
(use-package rime
  :if (or l/linux l/mac)
  :config
  (setq default-input-method "rime")
  )

(use-package telega
  ;;  :disabled
  :if (or l/linux l/mac)
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
  ;;(setq treemacs-indent-guide-style 'block)
  ;;(add-hook 'treemacs-mode-hook 'treemacs-indent-guide-mode)
  )
(add-to-list 'grep-find-ignored-files ".tag*")
(add-to-list 'grep-find-ignored-files ".TAG*")
(add-to-list 'grep-find-ignored-files "tag*")
(add-to-list 'grep-find-ignored-files "TAG*")

(use-package wgrep
  :bind
  (:map grep-mode-map
	("C-c C-p" . wgrep-change-to-wgrep-mode)
	("e" . wgrep-change-to-wgrep-mode)
	)
  :config
  )
(provide 'init)
;;; init.el ends here
