; init-builtin.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

;;(setq gc-cons-threshold 16777216) ;; 16m


;;; custom-function-of-core
(defun l/func/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt for a
file to visit if current buffer is not visiting a file."
  (interactive "P")
  (let ((pos (point)))
    (if (or arg (not buffer-file-name))
	;; /sudo:root@localhost: 可以简化/sudo::
	(find-file (concat "/sudo::"
			   (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo::" buffer-file-name))
      )
    (goto-char pos)
    )
  )

(defun l/func/user-edit (&optional arg)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (if (and file-name (tramp-tramp-file-p file-name))
        (let ((local-path (tramp-file-name-localname (tramp-dissect-file-name file-name)))
              (pos (point)))
          (find-alternate-file local-path)
          (goto-char pos)
          ;; (message "已恢复为普通用户权限"))
          (message "Recover file to normal user chown"))
      ;; (message "当前文件不是通过 TRAMP/Sudo 打开的")))
      (message "Current file is not open by TRAMP/Sudo. ")))
  )

(defun l/func/abbreviate-file-name()
  (let ((buffer-filename (buffer-file-name))
	)
    (if buffer-filename
	(abbreviate-file-name buffer-filename)
      "%b"
      ))
  )
;;; custom-function-of-option
(defun l/func/speed-up ()
  "Make emacs fast."
  (when l/+windows

    ;; exec
    (when (eq system-type 'windows-nt)
      (setq ;;exec-suffixes '("" ".exe" ".bat")
            exec-path (delete-dups (mapcar #'directory-file-name exec-path))))

    ;; (defvar my-executable-find-cache (make-hash-table :test 'equal :size 100))
    ;; (defvar my-executable-find-cache-miss (make-symbol "miss"))

    (defun my-executable-find-clear-cache ()
      (clrhash my-executable-find-cache))

    ;; (define-advice executable-find
    ;; 	(:around (orig-fun command &optional remote)
    ;; 		 my-executable-find-cache-advice)
    ;;   (if remote
    ;;       (funcall orig-fun command remote)
    ;; 	(let ((cached (gethash command my-executable-find-cache my-executable-find-cache-miss)))
    ;;       (if (eq cached my-executable-find-cache-miss)
    ;;           (puthash command (funcall orig-fun command) my-executable-find-cache)
    ;;         cached))))

    ;; dired
    (setq shell-command-guess-open nil)

    (defun my-open-in-external-app (&optional filename)
      "Open the current file or Dired marked files in external app.
When called from Lisp, if FILENAME is given, open that."
      (interactive)
      (let* ((file-list (cond
			 (filename `(,filename))
			 ((eq major-mode 'dired-mode) (dired-get-marked-files))
			 (t '(,(buffer-file-name)))))
             (do-it-p (if (<= (length file-list) 5)
                          t (y-or-n-p "Open more than 5 files? "))))
	(when do-it-p
          (dolist (fpath file-list)
            (let ((expanded (expand-file-name fpath)))
              (pcase system-type
		('windows-nt
		 (start-process
                  "open-external" nil
                  "powershell" "-Command" "Invoke-Item -LiteralPath"
                  expanded))
		('darwin
		 (start-process "open-external" nil "open" expanded))
		('gnu/linux
		 (let ((process-connection-type nil))
                   (start-process "open-external" nil "xdg-open" expanded)))))))))

    (with-eval-after-load 'dired
      (define-key dired-mode-map [remap dired-do-open] #'my-open-in-external-app))

    ;; find-gile
    (remove-hook 'before-save-hook #'vc-before-save nil)
    (remove-hook 'find-file-hook #'vc-refresh-state)
    (remove-hook 'find-file-hook #'epa-file-find-file-hook)
    )
  )

(defun l/func/active/hl-line ()
  (use-package hl-line
    :hook (after-init . global-hl-line-mode)
    :config
    (setq hl-line-sticky-flag nil)
    ;; Highlight starts from EOL, to avoid conflicts with other overlays
    (setq hl-line-range-function (lambda () (cons (line-end-position) (line-beginning-position 2))))
    )
  )

;;;; mode-line
(defun l/func/set/mode-line ()
  (setq mode-line-front-space " ")
  (setq mode-line-end-spaces " ")
  )

;;;; tab-bar
(defun l/func/set/tab-bar ()
  (defvar l/global-mode-string '(
				 "%e"
				 mode-line-front-space
				 "%I ["
				 (:eval (l/func/abbreviate-file-name))
				 "]"
				 mode-line-end-spaces
				 )
    "set local global-mode-string , avoid other mode change default value, or set a fix value make other plugin cannot change these local string ")
  (put 'l/global-mode-string 'risky-local-variable t)
  (add-to-list 'global-mode-string  'l/global-mode-string)
  (setq tab-bar-position t)
  (setq tab-bar-format '(tab-bar-format-history
			 tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab
			 tab-bar-format-align-right tab-bar-format-global
			 ))
  (set-face-attribute 'tab-bar nil :foreground "black" :background "grey75" :underline '(:color foreground-color :style line :position t))
  ;; (tab-bar-mode 1)
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  )



;;;; line splitter
(defun l/func/set/line-splitter ()
;;; 换行符修改
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))

  ;; 修改折行标志字符（第1号插槽）
  ;; 下面将 '\' 修改为 '↩' (你也可以换成任何你喜欢的字符，例如 '>' ↩)
  (set-display-table-slot standard-display-table 'wrap ?¬)

  ;; 如果你也想修改截断（truncation）标志字符（默认是 '$'，第0号插槽）
  ;; (set-display-table-slot standard-display-table 'truncation ?…)
  )

;;;; avoid long line
(defun l/func/set/avoid-long-line ()
  ;; avoid long line start
  (setq-default bidi-paragraph-direction 'left-to-right
		bidi-display-reordering 'left-to-right)
  (setq long-line-threshold 300
	large-hscroll-threshold 300
	syntax-wholeline-max 300
	;; bidi-inhibit-bpa t
	)
  ;; avoid long line ends
  )
(defun l/func/set/backup-dir-and-autosave-dir ()
  (defvar autosaves-dir (expand-file-name ".autosaves/" user-emacs-directory))
  (defvar backups-dir (expand-file-name ".backups/" user-emacs-directory))
  (unless (file-exists-p autosaves-dir)
    (make-directory autosaves-dir t)
    )
  (unless (file-exists-p backups-dir)
    (make-directory backups-dir t)
    )
  ;; change default action
  (setq-default default-directory (expand-file-name "~/"))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (get-buffer "*scratch*")
                (with-current-buffer "*scratch*"
                  (setq default-directory (expand-file-name "~/"))))
              (setq default-directory (expand-file-name "~/"))))
  (setq backup-directory-alist
	;; `((".*" . ,temporary-file-directory))
	`((".*" . ,backups-dir))
	version-control t     ;; make backup file have version
	backup-by-copying t   ;; avoid damage link file
	delete-old-versions t ;; auto remove old version backup file.
	kept-new-versions 5   ;; keep newest file count
	kept-old-versions 2   ;; keep oldest file count
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
   

;;;; builtin package
(defun l/func/enable/builtin-package ()
  
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
    (add-hook l:+after-init-hook
	      ;; 'after-init-hook
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

  )

;;; custom-option
;;;; speed & package

(defconst l/+mac (eq system-type 'darwin))
(defconst l/+windows (eq system-type 'windows-nt))
(defconst l/+linux (eq system-type 'gnu/linux))

(l/func/speed-up)

(l/func/set/backup-dir-and-autosave-dir)
;;;; ui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq elisp-fontify-semantically -1) ;; make speed lower
;; (cursor-sensor-mode -1) ;; may not use

(windmove-default-keybindings)
(windmove-mode +1)
(l/func/active/hl-line)
(l/func/set/mode-line) ;; need use first of tab-bar
(l/func/set/tab-bar)
(l/func/set/line-splitter)
(l/func/set/avoid-long-line)

(setq-default left-margin-width 1)
(setq-default right-margin-width 1)

;;(setq default-modeline-box-face (face-attribute 'mode-line :box))
;;(setq default-modeline-active-box-face (face-attribute 'mode-line-active :box))


(column-number-mode 1)
(setq mode-line-collapse-minor-modes '(eldoc-mode company-posframe-mode))

(global-completion-preview-mode 1)

;;;; minibuffer-complete
(icomplete-mode 1)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq completion-styles '(basic partial-completion emacs22 flex))
;;;; behavior
(electric-pair-mode 1)
(setq recenter-positions '(top 0.4 bottom)) ;; center point
(setq confirm-kill-emacs 'yes-or-no-p)
(setq use-short-answers t)

(defun l/func/show-paren-enclosing-data ()
  "Show paren when point in everywhere."
  (or (show-paren--default)
      (let* ((ppss (syntax-ppss))
             (open-pos (nth 1 ppss))) ; get paren open pos
        (when open-pos
          (let ((close-pos (condition-case nil
                               (scan-sexps open-pos 1) ; scan close paren pos
                             (error nil))))
            (when close-pos
              ;; by (HERE-BEG HERE-END THERE-BEG THERE-END MISMATCH) format return
              (list open-pos (1+ open-pos)
                    (1- close-pos) close-pos
                    nil)))))))

;; replace show-paren function
(setq show-paren-data-function #'l/func/show-paren-enclosing-data)

(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t
      show-paren-delay 0.125 ;; default-value is 0.125
      show-paren-style 'mixed
      show-paren-context-when-offscreen t  ;; when it's value is t, show paren first value in echo area.
      blink-matching-paren nil   ;; default-value is t ;; when useing show-paren-mode, it does not take effect.
      blink-matching-paren-highlight-offscreen nil ;; see paren first line in echo area
      )

(setq echo-keystrokes 0.01) ;; fast echo key sequence
(setq whitespace-line-column 880) ;; whitespace-line-mode show ugly, set this can set line large this num show ugly ui.
(eval-after-load "whitespace" `(set-face-attribute 'whitespace-space nil :background l:theme:default-background-color))
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq isearch-lazy-count t)
;; (setq lazy-count-prefix-format "%s/%s ") ;; default-value is this
(with-eval-after-load 'tab-line
;;  (face-attribute 'tab-line :box)
;;  (set-face-attribute 'tab-line nil )
  (set-face-attribute 'tab-line-tab nil :box '(:line-width -1 :color "grey75"))
  (set-face-attribute 'tab-line-tab-current nil :box '(:line-width -1 :color "#f5f500") :weight 'heavy )
  (set-face-attribute 'tab-line-tab-modified nil :underline '(:style line) )
  )
(global-tab-line-mode 1)

;;;; dired
(setq dired-listing-switches "-vhal")
(setq delete-by-moving-to-trash t)
(put 'dired-find-alternate-file 'disabled nil) ;; cacel warnning

;;;; minibuffer
(setq enable-recursive-minibuffers t)	     ;;minibuffer can recursive

;;;; edit
(delete-selection-mode +1)  
(global-auto-revert-mode +1)
(setq scroll-margin 3) 	 ;;set next page margin line
(setq scroll-step 1)
(setq scroll-conservatively 101)  ;;if value greater than 100, will nerver scroll
(setq scroll-preserve-screen-position t)
(setq kill-region-dwim 'emacs-word)

(global-so-long-mode t)	;; Disable fancy features when the file is too large

;; Mark
(setq global-mark-ring-max 300)
(setq mark-ring-max 500)
(setq set-mark-command-repeat-pop t)

;; Kill
(setq kill-ring-max 300)
;; history
(setq-default history-length 1000
	      savehist-additional-variables '(extended-command-history
					      file-name-history
					      mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring)
	      savehist-autosave-interval 300
	      )
(savehist-mode 1)

(save-place-mode t) 
(global-eldoc-mode)
(setq eldoc-echo-area-prefer-doc-buffer t)
;; 减少docbuffer的无用空行
(setq eldoc-doc-buffer-separator
      (propertize "\r\n" 'face '(:inherit underline :extend  t))
      )
;; 异步显示所有结果
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

(setq display-line-numbers-grow-only t)

(setopt cursor-in-non-selected-windows 'hbar)



(setq compilation-scroll-output t) ;; compilation auto refresh
(setq view-lossage-auto-refresh t) ;; C-h l auto refresh

(setq electric-indent-actions '(yank ))	;electric auto format

(setq hs-display-lines-hidden t) ;; use this can see hide line number
(setq hs-show-indicators t)      ;; see hide line indicators
(setq hs-indicator-type nil)
(hs-minor-mode t)
(setq ibuffer-use-header-line 'title)
(setq ibuffer-human-readable-size t)
(setq Buffer-menu-human-readable-sizes t)

(setq recentf-auto-cleanup 'never)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
;;(recentf-mode)


(setq even-window-heights nil) ;; when display buffer change window heights

(setq compile-command "")

(desktop-save-mode +1)
(desktop-read)				; init-builtin file load after init hook 
(l/func/enable/builtin-package)
(repeat-mode)
;;; keymap set
(defcustom l:leader-keymap (make-sparse-keymap) "Leader keymap." :group 'l:custom)
(defcustom l:leader-keymap-mode-keymap (make-sparse-keymap) "Leader keymap mode keymap." :group 'l:custom)
(define-key l:leader-keymap-mode-keymap (kbd l:leader-key) l:leader-keymap)
(define-minor-mode l:minor-leader-keymap-mode "Local leader keymap minor mode."
  :lighter llkm
  :keymap l:leader-keymap-mode-keymap
  :group 'l:custom)
(define-global-minor-mode l:global-leader-keymap-mode l:minor-leader-keymap-mode
  l:minor-leader-keymap-mode
  :group 'l:custom)
(l:global-leader-keymap-mode)

(defcustom l:global-keymap (make-sparse-keymap) "Local global keymap." :group 'l:custom)
(define-minor-mode l:minor-global-keymap-mode "Local global keymap minor mode."
  :lighter lgkm
  :keymap l:global-keymap
  :group 'l:custom)
(define-global-minor-mode l:global-global-keymap-mode l:minor-global-keymap-mode
  l:minor-global-keymap-mode
  :group 'l:custom)
(l:global-global-keymap-mode)
;;; define key
(keymap-set l:global-keymap "<f6>" #'window-toggle-side-windows)
(keymap-set l:global-keymap "M-L" #'display-line-numbers-mode)
(keymap-set l:global-keymap "M-W" #'whitespace-mode)
(keymap-set l:global-keymap "C-M-w" #'yank)
(keymap-set l:global-keymap "S-<return>" #'l:func:newline-and-indent-down)
(keymap-set l:global-keymap "C-<return>" #'l:func:newline-and-indent-down)
(keymap-set l:global-keymap "C-M-<return>" #'l:func:newline-and-indent-up)

(keymap-set l:leader-keymap "i" #'l:func:newline-and-indent-up)
(keymap-set l:leader-keymap "o" #'l:func:newline-and-indent-down)
;; (keymap-set l:leader-keymap "D D" #'duplicate-dwim)
(keymap-set l:leader-keymap "C-d" #'duplicate-dwim)
(keymap-set l:leader-keymap "d" #'duplicate-line)
;; save in kill-ring
(keymap-set l:leader-keymap "k" #'kill-whole-line)
(keymap-set l:leader-keymap "l" #'l:func:kill-current-line)
;; mark func avoid C-SPC can not use
(keymap-set l:leader-keymap "SPC" #'set-mark-command)

(keymap-set l:global-keymap "M-c" #'capitalize-dwim)
(keymap-set l:global-keymap "M-u" #'upcase-dwim)
(keymap-set l:global-keymap "M-z" #'zap-up-to-char)

(with-eval-after-load 'dired
  (defun l:func:alternate-back-dir ()
    "Back to up dir alternate."
    (interactive)
    (find-alternate-file ".."))
  (define-key dired-mode-map "b" #'l:func:alternate-back-dir)
  (define-key dired-mode-map "F" #'dired-create-empty-file)
  (define-key dired-mode-map "z" #'dired-up-directory)
  (setq dired-dwim-target t)
  )
(defun l:func:open-emacs-file (file)
  "Open emacs config file."
  (interactive)
  (find-file (concat user-emacs-directory file)))
(defun l:func:open-emacs-init-file () (interactive) (l:func:open-emacs-file "init.el"))
(keymap-set l:leader-keymap "," #'l:func:open-emacs-init-file)
(keymap-set l:leader-keymap "，" #'l:func:open-emacs-init-file)
(keymap-set l:leader-keymap "t" #'kill-current-buffer)
(keymap-set l:global-keymap "C-h C-j" #'eldoc-doc-buffer)
(keymap-set l:leader-keymap "C-v" #'view-mode)
(keymap-set l:leader-keymap "C-b" #'ibuffer)
(setq native-comp-jit-compilation-deny-list '("lsp-bridge"))
;;; provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-builtin)
