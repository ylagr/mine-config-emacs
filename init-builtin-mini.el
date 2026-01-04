; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:
;; --------------------pkg-emacs--------------------
;; set package archives. possibly set mirrors
(defconst +i-am-in-china +1)

(with-eval-after-load 'package
  (if +i-am-in-china
      (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                               ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                               ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))))


;; --------------------UI-emacs-------------------------
(setq mode-line-collapse-minor-modes '(eldoc-mode))
(icomplete-mode 1) ;;内置补全功能
(setq icomplete-in-buffer t) ;; ???
;;(load-file (expand-file-name "init-core.el" user-emacs-directory))
(global-completion-preview-mode 1) ;;全局补全预览
(electric-pair-mode 1)
(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(setq recenter-positions '(top 0.3 bottom)) ;;中心点
(setq confirm-kill-emacs 'yes-or-no-p)
(setq use-short-answers t)
(set-face-attribute 'default nil :background "grey94" :foreground "black" )
;; asdfasdfĵƣ
(setq echo-keystrokes 0.01) ;;fast echo key sequence
(setq isearch-lazy-count t
      ;; lazy-count-prefix-format "%s/%s " ;; 默认配置就是这个
      )
(symbol-value 'mode-line-format)
(set-face-attribute 'mode-line-inactive nil
		    :background (face-attribute 'default :background)
		    ;; :foreground (face-attribute 'default :background)
		    )
(setq dired-listing-switches "-vhal")
(save-place-mode t) ;;保存上次光标位置
(global-eldoc-mode)
(setq eldoc-echo-area-prefer-doc-buffer t)
;; (setq eldoc-display-functions '(eldoc-display-in-buffer))
(global-tab-line-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)

;; --------------------BEHAVIOR-emacs--------------------
(put 'dired-find-alternate-file 'disabled nil) ;; 设置取消疑惑的命令警告
(setq delete-by-moving-to-trash t)
(setq minibuffer-follows-selected-frame nil) ;; 让minibuffer只在启动minibuffer的frame生效
(setq enable-recursive-minibuffers t)	     ;;让minibuffer能递归使用
(delete-selection-mode +1)   ;;选中区域后插入删除选中文字
(global-auto-revert-mode +1) ;;实时刷新文件
(setq scroll-step 1)
(setq scroll-margin 2)		 ;;set next page margin line
(setq scroll-conservatively 101) ;;if value greater than 100, will nerver scroll
(setq scroll-preserve-screen-position t)
(global-so-long-mode t)	;; Disable fancy features when the file is too large
(setq kill-region-dwim 'emacs-word)


(setq org-indent-mode-turns-on-hiding-stars nil)
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

(when t
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
	;; `((".*" . ,temporary-file-directory))
	`((".*" . ,backups-dir))
	backup-by-copying t   ;; 自动备份
	delete-old-versions t ;; 自动删除旧的备份文件
	kept-new-versions 3   ;; 保留最近的3个备份文件
	kept-old-versions 1   ;; 保留最早的1个备份文件
	version-control t     ;; 多次备份
	)
  (setq auto-save-file-name-transforms
	;;      `((".*" ,temporary-file-directory t))
	`((".*" ,autosaves-dir t)) ;;自动保存临时文件
	)
  (setq auto-save-timeout 5) ;;set default auto save time without input
  ;;(setq create-lockfiles nil) ; 使用下方操作修改lock文件（.#*）位置
  (setq lock-file-name-transforms
	`((".*" ,backups-dir t))
	)
  )
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defun l/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt for a
file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))
  )

;; --------------------INPUT-emacs--------------------
(setq completion-styles '(basic flex partial-completion emacs22))

;; --------------------keybind-emacs--------------------

;; ----leader key----
(keymap-global-unset "C-j")
(keymap-global-set "C-j C-j" #'electric-newline-and-maybe-indent)
(keymap-unset lisp-interaction-mode-map "C-j")
(keymap-set lisp-interaction-mode-map "C-j C-j" #'eval-print-last-sexp)
(eval-after-load "org" `(progn (keymap-unset org-mode-map "C-j") (keymap-set org-mode-map "C-j C-j" #'org-return-and-maybe-indent) ))
(keymap-global-unset "M-SPC")
(keymap-global-set "M-]" #'cycle-spacing)
;;(keymap-global-set "M-SPC ")
;; ----leader key----end

(keymap-global-set "C-j C-i" #'newline-and-indent-up)
(keymap-global-set "C-j C-o" #'newline-and-indent-down)
(keymap-global-set "C-j D" #'duplicate-dwim)
(keymap-global-set "C-j C-d" #'l/duplicate-line)
(keymap-global-set "C-j d" #'l/duplicate-line)
(keymap-global-set "C-j C-k" #'kill-whole-line) ;; save in kill-ring
(keymap-global-set "C-j C-l" #'l/kill-current-line)	;; save in kill-ring
(keymap-global-set "C-j C-f" #'l/delete-whole-line)	;; not save in kill-ring
(keymap-global-set "C-j w" #'l/delete-whole-line)	;; not save
(keymap-global-set "C-j h" #'l/delete-line)	;; not save in kill-ring
(keymap-global-set "C-c C-_" #'comment-or-uncomment-region)
(keymap-global-set "C-c C-/" #'comment-or-uncomment-region)

(keymap-global-set "C-c C-n" #'scratch-buffer)
(keymap-global-set "C-x 4 o" #'display-buffer)

(keymap-global-set"<f5>" #'redraw-display)
(keymap-global-set "C-h C-j" #'eldoc-print-current-symbol-info)
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)
(keymap-set minibuffer-local-completion-map "M-n" #'minibuffer-next-completion)
(keymap-set completion-in-region-mode-map "M-n" #'minibuffer-next-completion)
(keymap-set minibuffer-local-completion-map "M-p" #'minibuffer-previous-completion)
(keymap-set completion-in-region-mode-map "M-p" #'minibuffer-previous-completion)
(keymap-global-set "C-x t" #'kill-current-buffer)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "C-x B" #'ibuffer)

(defun l/kill-current-line ()
  "Clear line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  )
(defun l/delete-whole-line()
  (interactive)
  (delete-line)
  )
(defun l/delete-line()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
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

(eval-after-load "dired"
  '(progn
     ;; (define-key dired-mode-map (kbd "b") #'dired-up-directory)
     (define-key dired-mode-map (kbd "F") #'dired-create-empty-file)
     (defun l/alternate-back-dir()
       "Back to up dir alternate."
       (interactive)
       (find-alternate-file "..")
       )
     (define-key dired-mode-map (kbd "b") #'l/alternate-back-dir)
     )
  )
  (defun l/scroll-half-page-up()
    
    )

(when t
  (defun l/open-init-file()
    "Open Emacs config file."
    (interactive)
    (find-file (concat user-emacs-directory "init.el"))
    )
  (global-set-key "\C-x\ ," #'l/open-init-file)
  (global-set-key "\C-x\ ，" #'l/open-init-file)
  (global-set-key (kbd "M-<f3>") #'l/open-init-file)
  )
(when t
  (defun l/refresh-buffer()
    "A."
    (interactive)
    (message "Refresh buffer...")
    (revert-buffer t)
    )
  (global-set-key (kbd "<f8>") #'l/refresh-buffer)
  )
(when t
  (defun l/next-half-page-lines()
    "Move cursor to next half-page lines."
    (interactive)
    (forward-line (/ (window-body-height) 2)))
  (defun l/previous-half-page-lines()
    "Move cursor to previous half-page lines."
    (interactive)
    (forward-line (/ (window-body-height) -2)))
  (global-set-key "\M-N" 'l/next-half-page-lines) ;; 光标向下移动 屏幕一半行
  (global-set-key "\M-P" 'l/previous-half-page-lines) ;; 光标向上移动屏幕一半行
  )
(when t
  (defun l/load-config(config-file-name)
    (load-file (expand-file-name config-file-name user-emacs-directory))
    )
  )

;; --------------------repeat-emacs--------------------
(when t
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

  (defvar l/buffer-lunch-repeat-map ; C-x <left> 或 <right>
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
      (define-key map (kbd "N") #'l/next-half-page-lines)
      (define-key map (kbd "j") #'l/next-half-page-lines)
      (define-key map (kbd "P") #'l/previous-half-page-lines)
      (define-key map (kbd "k") #'l/previous-half-page-lines)
      (define-key map (kbd "4") #'scroll-up-command)
      (define-key map (kbd "2") #'scroll-down-command)
      (define-key map (kbd "w") #'l/scroll-half-page-down)
      (define-key map (kbd "r") #'l/scroll-half-page-up)
      (define-key map (kbd "i") #'repeat-exit)
      (define-key map (kbd "o") #'other-window)
      (define-key map (kbd "O") #'other-frame)
      (define-key map (kbd "m") #'newline)
      (define-key map (kbd "x") #'delete-char)
      (define-key map (kbd "t") #'kill-current-buffer)
      (define-key map (kbd "c") #'comment-line)
      ;; (define-key map (kbd "") #'backward-delete-char-untabify)
      (define-key map (kbd ".") #'xref-find-definitions)
      (define-key map (kbd "/") #'xref-find-references)
      (define-key map (kbd ",") #'xref-go-back)
      (define-key map (kbd ">") #'end-of-buffer)
      (define-key map (kbd "<") #'beginning-of-buffer)
      (define-key map (kbd "0") #'delete-window)
      (define-key map (kbd "[") #'l/tab-line-switch-prev-tab)
      (define-key map (kbd "]") #'l/tab-line-switch-next-tab)      
      (dolist (it '(next-line previous-line forward-char backward-char forward-word backward-word back-to-indentation move-end-of-line left-word right-word l/previous-half-page-lines l/next-half-page-lines scroll-up-command scroll-down-command l/scroll-half-page-up l/scroll-half-page-down other-window kill-current-buffer   delete-char move-end-of-line
			      xref-find-definitions xref-find-references xref-go-back comment-line
			      embark-dwim end-of-buffer beginning-of-buffer ace-window delete-window
			      l/tab-line-switch-next-tab l/tab-line-switch-prev-tab))
	(put it 'repeat-map 'l/buffer-lunch-repeat-map))
      map)
    "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."
    )
  (defvar l/frame-lunch-repeat-map ; C-x <left> 或 <right>
    (let ((omap (make-sparse-keymap)))
      (define-key omap (kbd "O") #'other-frame)      
      (dolist (it '(other-frame))
	(put it 'repeat-map 'l/buffer-lunch-repeat-map))
      omap)
    "Keymap to repeat window buffer navigation key sequences.  Used in `repeat-mode'."
    )
  
  (setq mode-line-default-color (face-attribute 'mode-line :background))
  (defun l/repeat-mode-show-in-mode-line (&rest args)
    (set-face-attribute 'mode-line-active nil :background "firebrick4")
    ;; (set-face-attribute 'hl-line nil :background "red")
    ;; (setq tab-bar-local-format-color "yellow")
    ""
    )
  (defvar l/repeat-in-progress nil)
  (defun l/repeat-mode-hide-in-mode-line ()
    (set-face-attribute 'mode-line-active nil :background mode-line-default-color)
    (setq l/repeat-in-progress nil)
    ;; (set-face-attribute 'hl-line nil :background "darkseagreen2")
    ;; (setq tab-bar-local-format-color "blue")
    ""
    )
  (defun l/repeat-echo-function (keymap)
    (unless l/repeat-in-progress
      (repeat-echo-message keymap)
      (setq l/repeat-in-progress t)
      (l/repeat-mode-show-in-mode-line)
      )
    )
  ;; (advice-add (symbol-value 'repeat-echo-function) :after 'l/repeat-mode-show-in-tab-bar)
  (setq repeat-echo-function #'l/repeat-echo-function)
  (advice-add 'repeat-exit :after #'l/repeat-mode-hide-in-mode-line)

  )


;; --------------------window-emacs--------------------
(when t
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
  (defun l/display-buffer-reuse-window-action-clean-header_line (buffer alist)
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
  (defun l/display-buffer-in-side-window-action_show_tab-line (buffer alist)
    "A. BUFFER.  ALIST."
    (let ((win (display-buffer-in-side-window buffer alist)))
      (when win
	(tab-line-mode t)
	)
      )
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
     ("^\\(magit.*\\)\\|\\(\\*xref.*\\)"                            ;正则匹配buffer name
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
     ;; ("^\\(\\*[Hh]elp.*\\)"                            ;正则匹配buffer name
     ;;  (l/display-buffer-reuse-window-action             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     ;;   l/display-buffer-in-side-window-action-clean-header_line)
     ;;  (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
     ;;  ;;(window-width . 0.5)                     ;emacs会自动把这个设置到window-parameter里
     ;;  (window-width . 0.4)
     ;;  (window-height . 0.3)                   ;同上
     ;;  (slot . 2)                               ;这个会被display-buffer-in-side-window使用，控制window位置
     ;;  (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
     ;;  ;;(post-command-select-window . visible)
     ;;  ;;(body-function . l/clean-window-element)
     ;;  (haha . whatever)                        ;当然随你放什么
     ;;  (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     ;;   (select . t)                            ;自定义的param
     ;;   (quit . t)                              ;同上
     ;;   (popup . t)                             ;同上
     ;;   (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     ;;   (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
     ;;   ))
     ("^\\(\\*Warnings\\*\\)\\|\\(\\*Messages\\*\\)\\|\\(\\*[Hh]elp\\*\\)\\|\\(\\*Metahelp\\*\\)"
      (display-buffer-reuse-window l/display-buffer-in-side-window-action-clean-header_line)
      ;; (l/display-buffer-reuse-window-action-clean-header_line l/display-buffer-in-side-window-action-clean-header_line)
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
      (l/display-buffer-reuse-window-action l/display-buffer-in-side-window-action_show_tab-line)
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
     ("^\\(\\*eldoc.*\\)"
      (display-buffer-reuse-window  l/display-buffer-in-side-window-action-clean-header_line)
      (side . right)
      (slot . 100)
      ;; (window-width . 0.4)
      (window-height . 0.1)
      (window-parameters
       (mode-line-format . none)
       (no-other-window . t)
       )
      )
     ;;fallback
     ("^\\(\\*.*\\*\\)"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (window-width . 0.4)
      )
     )
   )
  )
;; --------------------thirdpkg-emacs--------------------
(defun l/ace-window()
  (interactive)
  (package-initialize t)
  (use-package ace-window
    :ensure t
    :demand t
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
    ;; (ace-window arg)
    )
  )
(keymap-global-set "C-x o" #'l/ace-window)

;; --------------------frame-emacs--------------------
(when t
  (global-set-key (kbd "<f12>") #'normal-frame)
  (global-set-key (kbd "<f9>") #'mini-frame)
  )
(unless (daemonp)  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b") ) ) )  )
(defun l/after-make-frame-func(frame)
  (select-frame frame)
  (message "test-after-make-frame")
  ;; (if eldoc-mode (eldoc-doc-buffer))
  (set-frame-height frame (- (/ (display-pixel-height) (frame-char-height)) 5))
  (set-frame-width frame (- (/ (display-pixel-width) (frame-char-width)) 5))
  (raise-frame frame)
  )
;; (add-hook 'after-make-frame-functions #'l/after-make-frame-func)

;; --------------------char-emacs--------------------
(when t
  (set-char-table-range char-width-table ?’ 1)
  (set-char-table-range char-width-table ?‘ 1)
  (set-char-table-range char-width-table ?“ 1)
  (set-char-table-range char-width-table ?” 1)
  (set-char-table-range char-width-table ?° 1)
  (set-char-table-range char-width-table ?— 1)
  (set-char-table-range char-width-table ?… 1)
  )
(when (and (not (daemonp)) (not (display-graphic-p)))
  (message "tty-fix")
  (global-completion-preview-mode -1)
  )
;; (require 'cl-lib)
(provide 'init-builtin-mini)
;;; init-builtin-mini.el ends here

