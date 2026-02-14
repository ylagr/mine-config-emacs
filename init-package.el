; init-package.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

;;(setq package-quickstart t)
;; 只有在 quickstart 文件存在时才跳过初始化

(unless (file-exists-p package-quickstart-file)
  (package-initialize))
(if +only-tty
    (package-initialize)
    )

(defvar l/plugin-start nil)
(use-package multiple-cursors
  :bind (
	 ("C-M--" . #'mc/mark-next-like-this)
	 ("C-M-0" . #'mc/skip-to-next-like-this)
	 ("C-M-=" . #'mc/mark-all-in-region)
	 ("C-M-9" . #'mc/unmark-next-like-this)
	 )
  )
(use-package magit
  ;; :disabled
  :defer t
  :ensure t
  :bind
  (
   ("C-j g" . #'magit)
   (:map magit-mode-map
	 ("," . #'magit-process-buffer)
	 ("，" . #'magit-process-buffer)
	 )
   )
  :config
  ;; (global-set-key (kbd "C-j g") #'magit)
  )

(use-package diff-hl
  :ensure t
  :defer t
  :commands find-file
  :hook
  (dired-mode . diff-hl-dired-mode)
  :init
  (global-diff-hl-mode)
  :config
  ;;(global-diff-hl-mode 1)
  ;;(global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)

  )

(use-package prescient
  :ensure t
  :config
;;  (setq completion-styles '(basic prescient partial-completion emacs22))
  (setq completion-styles '(prescient))
  (prescient-persist-mode)
  )
(use-package hotfuzz
  :after prescient
  :ensure t
  :config
  ;;  (setq completion-styles '(basic prescient hotfuzz partial-completion emacs22))
  (add-to-list 'completion-styles 'hotfuzz t)
  )
(use-package orderless
  :after prescient
  :ensure t
  :config
;;  (setq orderless-component-separator "[ &]")
  (setq orderless-component-separator "+")
  (setq orderless-matching-styles '(orderless-literal
				    ;; orderless-flex
                                    ;; orderless-regexp
                                    )
        )
  (add-to-list 'completion-styles 'orderless t)
;;  (setq completion-styles '(orderless basic)
;;        completion-category-defaults nil
;;        completion-category-overrides '((file (styles basic partial-completion)))
;;        )
  
;;  (setq completion-preview-completion-styles '(orderless basic initials))
  
  )

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
	 ("C-j 9" . ace-window)
	 ("C-j 0" . ace-delete-window)
	 ("C-x M-0" . ace-delete-window)
	 ("C-x M-o" . ace-delete-window)
         ;; ("C-x C-o" . ace-window)  ;; was delete-blank-lines
         )
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0 :background "yellow")))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  )

(use-package vundo
  :ensure t
  :bind
  ("C-x u" . vundo)
  )

;; translate
(use-package gt
  :bind ("M-SPC t" . gt-translate)
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
  ;;  (global-set-key (kbd "M-SPC t") #'gt-translate)
  )

(use-package citre
  :ensure t
  :defer
  :bind
  (
   ("C-x c j" . #'citre-jump)
   ("C-x c J" . #'citre-jump-back)
   ("C-x c ;" . #'citre-ace-peek)
   ("C-x c p" . #'citre-peek)
   ("C-x c u" . #'citre-update-this-tags-file)
   )
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  (remove-hook 'find-file-hook 'citre-auto-enable-citre-mode)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  ;;  (global-set-key (kbd "C-x c j") 'citre-jump)
  ;;  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  ;;  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  ;;  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; set default tags file generate location
   citre-default-create-tags-file-location 'global-cache
   ;; Set this if you'd like to use ctags options generated by Citre
   ;; directly, rather than further editing them.
   citre-edit-ctags-options-manually nil
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   ;; If you only want the auto enabling citre-mode behavior to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)
   )
  ;;  (eval-after-load "company" `(progn ))
  )

(use-package company
  :ensure t
  :defer
  :bind
  ("C-j j" . company-complete)
  :init
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'prog-mode-hook 'company-mode)
  ;; (company-mode 1)
  (global-company-mode 1)
  (global-completion-preview-mode -1)
  
  :config

  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (keymap-global-set "C-," #'company-complete)
  ;;(keymap-global-set "C-j j" #'company-complete)
  ;; fringe
  
  (setq company-dabbrev-code-everywhere t)
    (setq company-tooltip-align-annotations t)
  (setq company-tooltip-offset-display 'lines)
  (setq company-frontends
	'(company-preview-frontend company-echo-metadata-frontend company-pseudo-tooltip-frontend)
	)
  ;; 默认的 tab 是 循环选项,替换成补全
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)

  ;;(define-key company-active-map (kbd "RET") #'newline)
  ;;(define-key company-active-map (kbd "<return>") #'newline)
  ;;(add-to-list 'company-backends 'company-ispell)
  

  (setq company-files-exclusions '(".git/" ".DS_Store"))
  (setq company-abort-manual-when-too-short t)
  
  (setq company-transformers '(
			       delete-consecutive-dups
			       company-sort-prefer-same-case-prefix
			       ;; company-transformers-filter-case-insensitive
                               company-sort-by-occurrence
			       ))
 (setq company-backends `(company-capf company-files (company-dabbrev-code company-gtags company-etags company-keywords) company-dabbrev))
 )

(if (>= emacs-major-version 31)
    (use-package company-posframe
      :ensure t
      :after company
      
      :hook (company-mode . company-posframe-mode)
      :config
      ;;  (company-posframe-mode)
      )
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
   ("M-SPC r f" . consult-recent-file)
   ("M-SPC r r" . consult-register)
   ("M-SPC r s" . consult-register-store)
   ("M-SPC r l" . consult-register-load)
   ("M-SPC f" . consult-find)
   ("M-SPC SPC" . consult-ripgrep)
   )
  :config
  (setq consult-async-refresh-delay 0.5)
  ;;(global-set-key (kbd "M-y") #'consult-yank-pop)
  )

(use-package vertico
  ;;:disabled ;; 有点卡
  :defer
  :init
  (vertico-mode +1)
  ;;  (icomplete-mode 1)
  ;; (setq vertico-multiform-commands
  ;; 	'((consult-imenu buffer indexed)
  ;; 	  (describe-variable buffer indexed)
  ;; 	  (describe-bindings buffer indexed)
  ;; 	  (describe-function buffer indexed)
  ;; 	  (describe- buffer indexed)
  ;; 	  (describe-variable buffer indexed)
  ;; 	  (describe-variable buffer indexed)
  ;; 	  (execute-extended-command buffer indexed)
  ;; 	  (consult-line buffer)
  ;; 	  (consult-global-mark buffer indexed)
  ;; 	  (consult-mark buffer indexed)
  ;; 	  (consult-line-multi buffer)))
  ;;  (setq vertico-multiform-categories
  ;;	'((consult-grep buffer)))
  ;;  (vertico-multiform-mode t)
  (vertico-buffer-mode t) ;; 使用buffermode感受不到卡顿了,可能是buffer大小固定了
  (setq vertico-cycle t)
  )

(use-package clipetty
  :ensure t
;;  :disabled
  ;; :hook (after-init . global-clipetty-mode)
  :config
  (defun l/copy ()
    (interactive)
    (if (display-graphic-p)
	(clipboard-kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (region-beginning) (region-end))
      )
    )
  (defun l/cut ()
    (interactive)
    (if (display-graphic-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (kill-region (region-beginning) (region-end))
      )
    )
  (defun l/clipetty-kill(beg end &optional region)
    (interactive (progn
                   (let ((beg (mark kill-region-dwim))
			 (end (point)))
                     (cond
                      ((and kill-region-dwim (not (use-region-p)))
                       (list beg end kill-region-dwim))
                      ((not (and beg end))
                       (user-error "The mark is not set now, so there is no region"))
                      ((list beg end 'region))))))    
    (if (use-region-p)
	(if clipetty-mode
	    (l/cut)
	  (clipetty-mode)
	  (l/cut)
	  (clipetty-mode 0)
	  )
      (kill-region beg end region)
      )
    )
  (defun l/clipetty-kill-ring-save (beg end &optional region)
    (interactive (list (mark) (point) 'region))
    (if (use-region-p)
	(if clipetty-mode
            (l/copy)
	  (clipetty-mode)
	  (l/copy)
	  (clipetty-mode 0))
      (kill-ring-save beg end region)
      )
    )
;;  (keymap-global-set "C-w" #'kill-region)
  (keymap-global-set "C-w" #'l/clipetty-kill)
  (keymap-global-set "M-w" #'l/clipetty-kill-ring-save)
  
  ;; 设置emacs killring 不进入系统剪贴板
  (setq x-select-enable-clipboard nil)
  (defun l/copy-current-dir ()
    "快速复制当前 buffer 所在的目录路径。"
    (interactive)
    (let ((path default-directory)
	  (select-enable-clipboard t))
      (if clipetty-mode
          (kill-new path)
	(clipetty-mode)
	(kill-new path)
	(clipetty-mode 0))
      (message "已复制当前目录: %s" path))
    )
  (defun l/copy-project-root ()
    "快速复制当前项目根目录路径。"
    (interactive)
    (let ((project (project-current)))
      (if project
          (let ((root (project-root project))
		(select-enable-clipboard t))
	    (if clipetty-mode
		(kill-new root)
	      (clipetty-mode)
	      
	      (kill-new root)
	      (clipetty-mode 0))
            (message "已复制项目根目录: %s" root))
	(message "当前不在任何项目中 (未找到 .git 等标识)")))
    )
  )

(use-package async
  :ensure t
  )
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1)
  )

(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . #'embark-act)
	 ("C-M->" . #'embark-act-all)
	 )
  )
(use-package colorful-mode
  :ensure t
  :defer t
  )


(setq l/plugin-start t)

(setq completion-styles (seq-difference completion-styles '(partial-completion))) ;; 移除partial-completion,防到最后
(add-to-list 'completion-styles 'partial-completion t) ;; 补充默认的补全功能


