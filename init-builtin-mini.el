; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:
;; --------------------const-emacs -----------------
(setq package-quickstart t)
(setq gc-cons-threshold 16000000)
(defconst +only-tty  (and (not (daemonp)) (not (display-graphic-p))))

;; --------------------pkg-emacs--------------------
;; set package archives. possibly set mirrors
(defconst +i-am-in-china +1)
(setq use-package-expand-minimally t)
(with-eval-after-load 'package
  (if +i-am-in-china
      (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                               ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                               ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))))


;; --------------------UI-emacs-------------------------
;; avoid long line start
(setq-default bidi-paragraph-direction 'left-to-right
	      bidi-display-reordering 'left-to-right)
(setq long-line-threshold 300
      large-hscroll-threshold 300
      syntax-wholeline-max 300
      ;; bidi-inhibit-bpa t
      )
;; avoid long line ends
(setq mode-line-collapse-minor-modes '(eldoc-mode))
(icomplete-mode 1) ;;内置补全功能
(setq icomplete-in-buffer t) ;; ???
;;(load-file (expand-file-name "init-core.el" user-emacs-directory))
(global-completion-preview-mode 1) ;;全局补全预览
(electric-pair-mode 1)		   ;; 自动补全括号

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(setq recenter-positions '(top 0.3 bottom)) ;;中心点
(setq confirm-kill-emacs 'yes-or-no-p)
(setq use-short-answers t)
(defvar default-background-color "grey94")
(set-face-attribute 'default nil :background default-background-color :foreground "black" )
(setq default-modeline-box-face (face-attribute 'mode-line :box))
(set-face-attribute 'mode-line nil  :box '(:line-width 1 :color "#a1afc9"))
;; (setq fringe-styles "default") ;; fringe 在终端没用 fringe
(setq-default left-margin-width 1)
(setq-default right-margin-width 1)


(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t
      show-paren-style 'mixed
      show-paren-delay 0.2
      blink-matching-paren-highlight-offscreen t ;; 可以显示匹配的括号特殊样式
      ;; show-paren-context-when-offscreen 'child-frame
      show-paren-context-when-offscreen 'overlay
      ;; show-paren-context-when-offscreen t
      )

(setq echo-keystrokes 0.01) ;;fast echo key sequence
(setq isearch-lazy-count t
      ;; lazy-count-prefix-format "%s/%s " ;; 默认配置就是这个
      )
(setq whitespace-line-column 880) ;; whitespace-line-mode show ugly, set this can set line large this num show ugly ui.
(eval-after-load "whitespace" `(set-face-attribute 'whitespace-space nil :background default-background-color))

(set-face-attribute 'mode-line-inactive nil
		    :background (face-attribute 'default :background)
		    ;; :foreground (face-attribute 'default :background)
		    )
(with-eval-after-load 'tab-line
  (set-face-attribute 'tab-line-tab-current nil :box '(:line-width 1 :color "#f5f500") :weight 'heavy )
  (set-face-attribute 'tab-line-tab nil :box '(:line-width 1 :color "grey75"))
  (set-face-attribute 'tab-line-tab-modified nil :underline '(:style line) )
  )

(setq dired-listing-switches "-vhal")
(save-place-mode t) ;;保存上次光标位置
(global-eldoc-mode)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-help-at-pt t) 		;; help key at point
;; (setq eldoc-display-functions '(eldoc-display-in-buffer))
(global-tab-line-mode 1)
;; (global-display-line-numbers-mode 1)
;; (global-visual-line-mode 1)
(column-number-mode 1)


;;;; behavior
;; --------------------BEHAVIOR-emacs--------------------
(setq-default history-length 1000
	      savehist-additional-variables '(extended-command-history
					      file-name-history
					      mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring)
	      savehist-autosave-interval 300)
(savehist-mode 1)
(setq compilation-scroll-output t) ;; compilation auto refresh
(setq view-lossage-auto-refresh t) ;; C-h l auto refresh
(setq electric-indent-actions '(yank ))	;electric 自动格式化的动作
(setq shell-command-switch "-c");; 使用-ic可以让compile之类的动作可用，但是不能使用project-grep了。很奇怪
(setq hs-display-lines-hidden t)
(setq hs-show-indicators t)
(setq hs-indicator-type nil)
(setq ibuffer-use-header-line 'title)
(setq ibuffer-human-readable-size t)
(setq Buffer-menu-human-readable-sizes t)
(setq even-window-heights nil)

(setq recentf-auto-cleanup 'never)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(recentf-mode)
;; Kill
(setq kill-ring-max 300)

;;(add-hook 'find-file-hook #'read-only-mode)
(defun l/change-buffer-in-view-when-editable()
  (selected-window)
  )
;;(setq window-buffer-change-functions )
(setq x-select-enable-clipboard nil)	 ;; 关闭和系统剪贴板的交互
(setq truncate-partial-width-windows 10) ;; 截断行判断窗口宽度调整，默认50
(setq compile-command "")
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
;; Mark
(setq global-mark-ring-max 300)
(setq mark-ring-max 500)
(setq set-mark-command-repeat-pop t)

(setq org-indent-mode-turns-on-hiding-stars nil)

(eval-after-load "grep"
  `(progn
     (add-to-list 'grep-find-ignored-files ".tag*")
     (add-to-list 'grep-find-ignored-files ".TAG*")
     (add-to-list 'grep-find-ignored-files "tag*")
     (add-to-list 'grep-find-ignored-files "TAG*")
     ))

(use-package wgrep
  :load-path "user-lisp/wgrep/"
  :bind
  (:map grep-mode-map
	("C-c C-p" . wgrep-change-to-wgrep-mode)
	;; ("e" . wgrep-change-to-wgrep-mode) ;; e有默认的grep-change-to-grep-edit-mode
	)
  :config

  )
(use-package undohist
  :load-path "user-lisp/undohist/"
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
(use-package goto-last-change
  :load-path "user-lisp/goto-last-change/"
  :bind (
	 ("C-M-<" . #'goto-last-change)
	 )
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

;;;; keybind
;; --------------------keybind-emacs--------------------
;; fix tty-key
(defun l/quoted-insert-with-read-key (arg)
  "Insert the next character using read-key, not read-char."
  (interactive "*p")
  (let ((char (read-key))
	)
    ;; Ensure char is treated as a character code for insertion
    (if (eq char 67108896)
	(insert-and-inherit ?\0)
      (unless (characterp char)
	(user-error "%s is not a valid character"
                    (key-description (vector char))))
      (when (numberp char)
	(while (> arg 0)
          (insert-and-inherit char)
          (setq arg (1- arg))))))
  )
(keymap-global-set "C-q" #'l/quoted-insert-with-read-key)

;; ----leader key----
(keymap-global-unset "C-j")
(keymap-global-set "C-j C-j" #'electric-newline-and-maybe-indent)
(keymap-unset lisp-interaction-mode-map "C-j")
(keymap-set lisp-interaction-mode-map "C-j C-j" #'eval-print-last-sexp)
(eval-after-load "org" `(progn (keymap-unset org-mode-map "C-j") (keymap-set org-mode-map "C-j C-j" #'org-return-and-maybe-indent) ))
(keymap-global-unset "M-SPC")
(keymap-global-set "M-RET" #'cycle-spacing)
;;(keymap-global-set "M-SPC ")
;; ----leader key----end
(eval-after-load "hideshow"
  `(keymap-set hs-minor-mode-map "C-." #'hs-cycle)
  )

(keymap-global-set "M-L" #'global-display-line-numbers-mode)
(keymap-global-set "C-M-w" #'yank)
(keymap-global-set "C-j C-i" #'newline-and-indent-up)
(keymap-global-set "C-j C-o" #'newline-and-indent-down)
(keymap-global-set "C-j D" #'duplicate-dwim)
(keymap-global-set "C-j C-d" #'l/duplicate-line)
(keymap-global-set "C-j d" #'l/duplicate-line)
(keymap-global-set "C-j C-k" #'kill-whole-line) ;; save in kill-ring
(keymap-global-set "C-j C-l" #'l/kill-current-line)	;; save in kill-ring
(keymap-global-set "C-j C-f" #'l/delete-whole-line)	;; not save in kill-ring
(keymap-global-set "M-D" #'l/delete-whole-line)
(keymap-global-set "C-j w" #'l/delete-whole-line)	;; not save
(keymap-global-set "C-j h" #'l/delete-line)	;; not save in kill-ring
(keymap-global-set "C-c h" #'l/delete-line)	;; not save in kill-ring
(keymap-global-set "C-j m" #'l/push-mark)
(keymap-global-set "C-c m" #'l/push-mark)
(keymap-global-set "M-k" #'l/move-forward-of-bounds-of-thing-at-point)
(keymap-global-set "M-K" #'kill-sentence)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "M-\"" #'l/choose-inner)
(keymap-global-set "C-M-;" #'l/choose-inner)
(keymap-global-set "C-M-'" #'l/choose-outer)


(keymap-global-set "C-S-v" #'clipboard-yank)
(keymap-global-set "C-M-]" #'undo-only)
(keymap-global-set "C-j c" #'comment-line)

(keymap-global-set "C-c C-_" #'comment-or-uncomment-region)
(keymap-global-set "C-c C-/" #'comment-or-uncomment-region)
;; ----edit ----end
(keymap-global-set "C-j r" #'compile) 	;; run shell command
(keymap-global-set "C-c c c" #'compile)
(keymap-global-set "C-c r o" #'recentf-open)
(keymap-global-set "C-c r f" #'recentf-open-files)


(use-package outline
  :bind (("C-j o" . #'outline-minor-mode)
	 (:map outline-minor-mode-map
	       ("C-c TAB" . #'outline-show-subtree)
	       ("<backtab>" . #'outline-cycle)
	       ("C-j p" . #'outline-show-entry)
	     )
	 )
  )
;; ----code ----end
(keymap-global-set "C-x F" #'set-fill-column)
(keymap-global-set "C-x f" #'find-file-at-point)
(keymap-global-set "C-c C-n" #'scratch-buffer)
(keymap-global-set "C-x 4 o" #'display-buffer)

(keymap-global-set"<f5>" #'redraw-display)

(keymap-global-set "C-M-i" #'completion-at-point)
;;(global-set-key (kbd "C-M-i") #'completion-symbol)
;; (keymap-global-set "C-h C-j" #'eldoc-print-current-symbol-info)
(keymap-global-set "C-h C-j" #'eldoc-doc-buffer)
(keymap-global-set "C-h C-k" #'eldoc-print-current-symbol-info)
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)
(keymap-set minibuffer-local-completion-map "M-n" #'minibuffer-next-completion)
(keymap-set completion-in-region-mode-map "M-n" #'minibuffer-next-completion)
(keymap-set minibuffer-local-completion-map "M-p" #'minibuffer-previous-completion)
(keymap-set completion-in-region-mode-map "M-p" #'minibuffer-previous-completion)
(keymap-global-set "C-j t" #'kill-current-buffer)
(keymap-global-set "C-c t" #'kill-current-buffer)
(keymap-global-set "C-j v" #'view-mode)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "C-x B" #'ibuffer)

(keymap-global-set "C-SPC" #'l/set-marker)
(keymap-global-set "C-@" #'l/set-marker)
(keymap-global-set "C-c C-@" #'set-mark-command)
(keymap-global-set "C-c C-SPC" #'set-mark-command)
;;(keymap-global-set "C-@" #'set-mark-command)

(defvar l/bracket-pairs
  '((?\( . ?\))   ; 圆括号
    (?\[ . ?\])   ; 方括号  
    (?{ . ?})     ; 花括号
    (?< . ?>)     ; 尖括号
;;    (?' . ?')     ; 单引号
;;    (?\" . ?\")   ; 双引号
;;    (?` . ?`)     ; 反引号
    (?« . ?»)     ; 书名号
    (?『 . ?』)   ; 中文方括号
    (?「 . ?」)   ; 中文引号
    (?（ . ?）)   ; 中文圆括号
    (?【 . ?】)   ; 中文六角括号
    (?〖 . ?〗)   ; 中文方头括号
    (?〔 . ?〕)   ; 中文方头括号
    (?［ . ?］)   ; 中文方头括号
    (?〚 . ?〛)   ; 中文方头括号
    (?〘 . ?〙)   ; 中文方头括号
    (?｛ . ?｝)   ; 中文方头括号
    (?《 . ?》)   ; 中文方头括号
    (?〈 . ?〉)   ; 中文方头括号
    (?‹ . ?›)   ; 中文方头括号
    (?⟨ . ?⟩)   ; 中文方头括号
    (?˂ . ?˃)   ; 中文方头括号
    (?˱ . ?˲)   ; 中文方头括号
    )
  "List of bracket pairs for matching.")

(defun l/choose(inner outer char)
  (when (use-region-p) (deactivate-mark) )
  (let ((cur-pos (point))
	(pair (assoc char l/bracket-pairs))
	(pair-match (rassoc char l/bracket-pairs))
	)
    (let ((forward-char (cond
			 (pair (cdr pair))
			 (pair-match (cdr pair-match))
			 (t char)
			 ))
	  (backward-char (cond
			  (pair (car pair))
			  (pair-match (car pair-match))
			  (t char)
			  ))
	  )
      (search-forward (string forward-char))
      (if outer (forward-char))
      (set-marker (mark-marker) (- (point) 1))
      (goto-char cur-pos)
      (search-backward (string backward-char))
      (if inner (forward-char))
      (setq mark-active t)
      )
    )
  
  )

(defun l/choose-inner (char)
  (interactive (list
		;;(prefix-numeric-value current-prefix-arg)
		(read-char-from-minibuffer "Inner to char: " nil 'read-char-history)
		))
  (l/choose t nil char)
  )
(defun l/choose-outer (char)
  (interactive (list
		(read-char-from-minibuffer "Outer to char: " nil 'read-char-history)
		))
  (l/choose nil t char)
  )


(defun l/set-marker()
  (interactive)
  (set-marker (mark-marker) (point))
  (setq mark-active t)
  )

(defun l/push-mark()
  "Interactive."
  (interactive)
  (push-mark)
  )

;; region移动 C-x C-x

;; (require 'cl-generic)
(defun l/move-forward-of-bounds-of-thing-at-point ()
  "Base on `'isearch-forward-thing-at-point ."
  (interactive)
  (if (l/select-bound-thing-at-point)
      nil
    (back-to-indentation)
    (l/select-bound-thing-at-point)
      )
  

  )
(defun l/select-bound-thing-at-point()
  (require 'cl-generic)
  (let ((bounds (seq-some (lambda (thing) (bounds-of-thing-at-point thing))
			  ;; `isearch-forward-thing-at-point
			  '(url symbol sexp)
			  )))
    (cond
     (bounds
      (when (use-region-p)
        (deactivate-mark)
	)
      ;;(when (< (car bounds) (point))
      (goto-char (cdr bounds))
      ;;	(set-mark (cdr bounds))
      (set-marker (mark-marker) (cdr bounds))
      (goto-char (car bounds))
      (setq mark-active t)
      )
     )
    )
  )

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
;;	(point (point))
	(line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	)
    (end-of-line)
    (newline)
    (insert line)
    (beginning-of-line)
    ;;    (goto-char point)
    (move-to-column column)
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

(add-to-list 'save-some-buffers-action-alist
	     '(?~ (lambda (buf)
		    (with-current-buffer buf
		      (set-buffer-modified-p nil))
		    ;; Return t so we don't ask about BUF again.
		    t)
		  "skip this buffer and mark it unmodified")
	     )
(eval-after-load "project"
  `(progn
     (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
     (keymap-set project-prefix-map "m" #'magit-project-status)
     ))
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z") #'dired-up-directory)
     (define-key dired-mode-map (kbd "F") #'dired-create-empty-file)
     (keymap-set dired-mode-map "_" #'dired-create-empty-file)
     (keymap-set dired-mode-map "—" #'dired-create-empty-file)
     (defun l/alternate-back-dir()
       "Back to up dir alternate."
       (interactive)
       (find-alternate-file "..")
       )
     (define-key dired-mode-map (kbd "b") #'l/alternate-back-dir)
     )
  )
(defun l/scroll-half-page-up()
  "Scroll up half the page."
  (interactive)
  (scroll-up (/ (window-body-height) 2))
  )
(defun l/scroll-half-page-down()
  "Scroll down half the page."
  (interactive)
  (scroll-down (/ (window-body-height) 2))
  )

(when t
  (defun l/open-init-file()
    "Open Emacs config file."
    (interactive)
    (find-file (concat user-emacs-directory "init.el"))
    )
  (keymap-global-set "C-x ," #'l/open-init-file)
  (keymap-global-set "C-x ，" #'l/open-init-file)
  (keymap-global-set "M-<f3>" #'l/open-init-file)
  )
(when t
  (defun l/refresh-buffer()
    "A."
    (interactive)
    (message "Refresh buffer...")
    (revert-buffer t)
    )
  (keymap-global-set  "<f8>" #'l/refresh-buffer)
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
  (keymap-global-set "M-N" 'l/next-half-page-lines) ;; 光标向下移动 屏幕一半行
  (keymap-global-set "M-P" 'l/previous-half-page-lines) ;; 光标向上移动屏幕一半行
  )
(when t
  (defun l/load-config(config-file-name)
    (load-file (expand-file-name config-file-name user-emacs-directory))
    )
  )
(use-package view
  :bind((:map view-mode-map
	      ("f" . #'View-scroll-page-forward)
	      ("b" . #'View-scroll-page-backward)
	      ("j" . #'next-line)
	      ("k" . #'previous-line)
	      ("h" . #'backward-char)
	      ("l" . #'forward-char)
	 ))
  )
(use-package drag-stuff
  :load-path "user-lisp/drag-stuff/"
  ;;:ensure t
  :bind
  ("M-<up>"  . drag-stuff-up)
  ("M-<down>" . drag-stuff-down)
  )
(use-package praise
  :after async
  :load-path "lib/praise"
  ;;:bind
  
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
  (defun l/backward-kill-word()
    "Avoid M-DEL in repeat-progress."
    (interactive)
    (backward-kill-word 1)
    )
  (defun l/delete-char()
    "Avoid C-d in repeat-progress."
    (interactive)
    (delete-char 1)
    )
  (defun l/next-line()
    "Local"
    (interactive)
    (forward-line 1)
    )
  (defun l/previous-line()
    "Local"
    (interactive)
    (forward-line -1)
    )

  ;;  (setq repeat-check-key t)
  (keymap-global-set "C-c [" #'l/tab-line-switch-prev-tab)
  (keymap-global-set "C-c ]" #'l/tab-line-switch-next-tab)
  (defvar l/buffer-lunch-repeat-map ; C-x <left> 或 <right>
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'l/next-line)
      (define-key map (kbd "p") #'l/previous-line)
      ;; (define-key map (kbd "f") #'forward-char)
      ;; (define-key map (kbd "b") #'backward-char)
      ;; (define-key map (kbd "e") #'previous-line)
      ;; (define-key map (kbd "d") #'next-line)
      ;; (define-key map (kbd "s") #'backward-char)
      ;; (define-key map (kbd "h") #'backward-char)
      ;; (define-key map (kbd "l") #'forward-char)
      ;; (define-key map (kbd "v") #'forward-word)
      ;; (define-key map (kbd "z") #'backward-word)
      (define-key map (kbd "3") #'move-beginning-of-line)
      (define-key map (kbd "a") #'back-to-indentation)
      (define-key map (kbd "g") #'move-end-of-line)
      
      ;; (define-key map (kbd "k") #'l/previous-half-page-lines)
      ;; (define-key map (kbd "j") #'l/next-half-page-lines)

      ;; (define-key map (kbd "4") #'scroll-up-command)
      ;; (define-key map (kbd "2") #'scroll-down-command)
      ;; (define-key map (kbd "w") #'l/scroll-half-page-down)
      ;; (define-key map (kbd "r") #'l/scroll-half-page-up)

      (define-key map (kbd "P") #'tab-previous)
      (define-key map (kbd "N") #'tab-next)
      (define-key map (kbd "{") #'tab-previous)
      (define-key map (kbd "}") #'tab-next)
      ;; (define-key map (kbd "'") #'tab-switcher)
      (define-key map (kbd "y") #'tab-switcher)
      
      (define-key map (kbd "q") #'quit-window)
      (define-key map (kbd "i") #'repeat-exit)
      (define-key map (kbd "o") #'other-window)
      (define-key map (kbd "O") #'other-frame)
      ;; (define-key map (kbd "m") #'l/push-mark)
      ;; (define-key map (kbd "x") #'l/backward-kill-word)
      (define-key map (kbd "t") #'kill-current-buffer)
      (define-key map (kbd "c") #'comment-line)
      (define-key map (kbd "d") #'l/duplicate-line)
      (define-key map (kbd "w") #'l/delete-whole-line)
      ;; (define-key map (kbd "") #'backward-delete-char-untabify)
      ;; (define-key map (kbd ".") #'xref-find-definitions)
      ;; (define-key map (kbd "/") #'xref-find-references)
      ;; (define-key map (kbd ",") #'xref-go-back)
      (define-key map (kbd ">") #'end-of-buffer)
      (define-key map (kbd "<") #'beginning-of-buffer)
      (define-key map (kbd "0") #'delete-window)
      (define-key map (kbd "9") #'ace-window)
      (define-key map (kbd "[") #'l/tab-line-switch-prev-tab)
      (define-key map (kbd "]") #'l/tab-line-switch-next-tab)
      (dolist (it '(
		    ;; next-line previous-line
		    l/next-line l/previous-line
			      ;; forward-char backward-char forward-word backward-word
			      ;; back-to-indentation left-word right-word
			      l/previous-half-page-lines l/next-half-page-lines
			      scroll-up-command scroll-down-command
			      l/scroll-half-page-up l/scroll-half-page-down
			      other-window kill-current-buffer quit-window ace-window delete-window
			      l/delete-char l/backward-kill-word
			      ;; find-file find-file-at-point
			      find-file--read-only
			      view-mode-enter view-mode-enable view-mode view-mode-exit View-exit
			      switch-to-buffer project-find-file-in
			      move-end-of-line move-beginning-of-line end-of-visual-line end-of-visible-line beginning-of-visual-line
			      tab-previous tab-next tab-switcher tab-switcher-select
			      xref-find-definitions xref-find-references xref-go-back
			      comment-line l/delete-whole-line l/duplicate-line
			      embark-dwim end-of-buffer beginning-of-buffer l/push-mark
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
(setq window-min-height 2)
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
  
  (setq
   display-buffer-alist
   `(
     ("^\\(magit-process.*\\)"                            ;正则匹配buffer name
      ;;(display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
      (display-buffer-in-side-window)
      (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
      (window-width . 0.4)                     ;emacs会自动把这个设置到window-parameter里
      ;; (window-height . 0.3)                   ;同上
      (slot . -10)                               ;这个会被display-buffer-in-side-window使用，控制window位置
      (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
      (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
       (select . t)                            ;自定义的param
       (quit . t)                              ;同上
       (popup . t)                             ;同上
       (mode-line . nil)
       ;; (no-other-window . t)
       ))
     ("^\\(magit-.*\\)"                            ;正则匹配buffer name
      ;;(display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
      (display-buffer-use-some-window)
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
      ;;(display-buffer-reuse-window             ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
      (display-buffer-in-side-window)
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
      ;;(display-buffer-reuse-window l/display-buffer-in-side-window-action-clean-header_line)
      (display-buffer-in-side-window)
      ;; (l/display-buffer-reuse-window-action-clean-header_line l/display-buffer-in-side-window-action-clean-header_line)
      (side . right)                          ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
      (window-width . 0.4)                     ;emacs会自动把这个设置到window-parameter里
      (window-height . 0.4)                   ;同上
      (slot . 2)                               ;这个会被display-buffer-in-side-window使用，控制window位置
      (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
      (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
       (select . t)                            ;自定义的param
       (quit . t)                              ;同上
       (popup . t)                             ;同上
       (mode-line-format . nil)               ;emacs version > 25， none会隐藏mode line，nil会显示...
       (no-other-window . t)                   ;随你设置其他的window-parameter，看文档 ;可以使用ace-window切换过去
       )
      )
     ("^\\(\\*gt-result\\*\\)"
      ;;(l/display-buffer-reuse-window-action l/display-buffer-in-side-window-action_show_tab-line)
      (display-buffer-in-side-window)
      (slot . 10)
      (post-command-select-window . visible)
      (window-parameters
       (quit . t)
       )
      )
     ("^\\(\\*vterm\\*\\)"
      ;;      (display-buffer-reuse-window display-buffer-in-side-window)
      (display-buffer-in-side-window)
      (side . right)
      (window-width . 0.4)
      (post-command-select-window . visible)
      )
     ("^\\(\\*eldoc.*\\)"
      ;;      (display-buffer-reuse-window  l/display-buffer-in-side-window-action-clean-header_line)
      ;; (display-buffer-in-side-window)
      ;; (display-buffer-at-bottom)
      (display-buffer-in-side-window)
      ;; (side . bottom)
      (side . top)
      (slot . 100)
      ;; (window-width . 0.4)
      ;;(window-height . shrink-window-if-larger-than-buffer)
      (window-min-height . 1)
      ;; (window-height . fit-window-to-buffer)
      (window-height . 2)
      ;; (window-height . 0.05)
      (window-parameters
       (mode-line-format . none)
       (no-other-window . t)
       (tab-line-format . none)
       (header-line-format . none)
       )
      )
     ;;fallback
     ("^\\(\\*.*\\*\\)"
      ;;(display-buffer-reuse-window display-buffer-in-side-window)
      (display-buffer-in-side-window)
      (side . right)
      (window-width . 0.4)
      )
     )
   )
  )

;; --------------------thirdpkg-emacs--------------------
(defun l/plugin-start()
  "Start plugin."
  (interactive)
  (l/load-config "init-package.el")
  )
(if (boundp 'l/init-plugin)
    (l/plugin-start)
  )

(unless (boundp 'l/plugin-start)
  (keymap-global-set "C-." #'l/plugin-start)
  (keymap-global-set "C-," #'l/plugin-start)
  (keymap-global-set "C-j 9" #'l/plugin-start)
  (keymap-global-set "C-j g" #'l/plugin-start)
  (keymap-global-set "C-x u" #'l/plugin-start)
  (keymap-global-set "C-x o" #'l/plugin-start)
  (keymap-global-set "C-j j" #'l/plugin-start)
  (keymap-global-set "C-x c u" #'l/plugin-start)
  (keymap-global-set "C-x c p" #'l/plugin-start)
  (keymap-global-set "C-x c ;" #'l/plugin-start)
  (keymap-global-set "C-x c j" #'l/plugin-start)
  (keymap-global-set "M-SPC t" #'l/plugin-start)
  (keymap-global-set "M-SPC s" #'l/plugin-start)
  (keymap-global-set "C-j s" #'l/plugin-start)
  (keymap-global-set "M-SPC m" #'l/plugin-start)
  (keymap-global-set "M-SPC g m" #'l/plugin-start)
  (keymap-global-set "M-SPC g i" #'l/plugin-start)
  (keymap-global-set "M-SPC b" #'l/plugin-start)
  (keymap-global-set "M-SPC i" #'l/plugin-start)
  (keymap-global-set "M-SPC e" #'l/plugin-start)
  (keymap-global-set "M-SPC r r" #'l/plugin-start)
  (keymap-global-set "M-SPC r s" #'l/plugin-start)
  (keymap-global-set "M-SPC r l" #'l/plugin-start)
  )


;; --------------------frame-emacs--------------------
(unless  +only-tty
  (keymap-global-set  "M-<f12>" #'l/normal-frame)
  (keymap-global-set  "M-<f9>" #'l/mini-frame)
  )
;;(unless (daemonp)
  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b") ) ) )
;;  )


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
(when +only-tty
  (global-auto-composition-mode -1)	;; fix only emacs run in terminal command
  )
;; (require 'cl-lib)
(provide 'init-builtin-mini)
;;; init-builtin-mini.el ends here

