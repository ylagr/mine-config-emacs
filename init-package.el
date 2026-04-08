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
(use-package ace-pinyin
  :ensure t
  :bind ("M-I" . ace-pinyin-jump-char-2)
  :init
  ;; (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1)
  :config
  (setq ace-pinyin-simplified-chinese-only-p nil)
  )
(use-package vlf
  :ensure t
  :init
  (require 'vlf-setup)
  )
(use-package iedit
  :ensure t
  :bind
  ("M-_" . iedit-mode)
  )
(use-package multiple-cursors
  :bind (
	 ("C-M--" . #'mc/mark-next-like-this)
	 ("C-M-0" . #'mc/skip-to-next-like-this)
	 ("C-M-=" . #'mc/mark-all-in-region)
	 ("C-M-9" . #'mc/unmark-next-like-this)
	 )
  )
(use-package yasnippet-snippets :ensure t)
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
(use-package flash
  :ensure t
  :bind (
	 ("M-U" . flash-jump)
	 ("M-S-u" . flash-jump)
	 ("C-j f" . flash-char-find)
	 ("C-j b" . flash-char-find-backward)
	 )
  :init
  ;; (flash-isearch-mode 1)  ;; 替换isearch 使用flash跳转指定位置，不用按多次isearch快捷键了 ;; 使用pyim的时候会导致无法输入更多字符，这个还是使用 flash jump处理吧，flash jump 不能处理中文字符，就这样吧
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
  ;; (run-with-idle-timer 5 nil '(lambda ()
  (global-company-mode 1)
  (global-completion-preview-mode -1)
  ;; ))
  
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

(if (< emacs-major-version 31)
    ""
  (use-package company-posframe
    :ensure t
    :after company
    
    :hook (company-mode . company-posframe-mode)
    :config
    ;;  (company-posframe-mode)
    )
  (use-package lsp-bridge
    ;; :defer t
    :load-path "lib/lsp-bridge"
    ;; :after yasnippet
    ;; markdown-mode
    :init
    ;; (run-with-idle-timer 6 nil '(lambda () (require 'lsp-bridge)))
    ;; (defun l/lsp-bridge-init()
      ;; (require 'lsp-bridge)
      ;; (remove-hook 'prog-mode-hook 'l/lsp-bridge-init)
      ;; )
    ;; (add-hook 'prog-mode-hook 'l/lsp-bridge-init)
    :config
    (add-to-list 'exec-path "~/.local/bin/")
    (if (file-exists-p "~/.local/bin/python-lsp-bridge" )
	"Check finish."  
      (shell-command (concat "ln -s " (expand-file-name "lib/lsp-bridge/python-lsp-bridge" user-emacs-directory) " " "~/.local/bin/" ))
      )
    (with-eval-after-load 'citre
      (setq lsp-bridge-find-ref-fallback-function 'citre-backend-find-reference)
      (setq lsp-bridge-find-def-fallback-function 'citre-backend-find-definition)
      (setq acm-enable-citre t)
      )
    (with-eval-after-load 'acm
      ;; 将 acm-mode-map 放入仿真模式列表，确保它在所有 Minor Mode 之上
      (add-to-list 'emulation-mode-map-alists
		   `((acm-mode . ,acm-mode-map))))
    ;; 1. 定义你专供 lsp-bridge 使用的超级 Cape 函数
    ;; 你可以根据需求在里面组合不同的 cape 后端
    (with-eval-after-load 'cape
      (defun my/lsp-bridge-super-cape()
	(interactive)
	(cape-wrap-super #'cape-dabbrev #'cape-keyword  #'cape-file #'cape-keyword #'cape-abbrev)
	)

      ;; 2. 使用 advice 劫持 lsp-bridge 的查询方法
      (defun my/lsp-bridge-capf-with-super-cape (orig-fun &rest args)
	"当 acm-backend-capf-candidates 运行时，强制使用自定义的 cape 函数"
	;; 动态绑定 completion-at-point-functions，确保只影响当前作用域
	(let ((completion-at-point-functions (list 'my/lsp-bridge-super-cape)))
	  ;; (symbol-value 'completion-at-point-functions)
	  ;; (message "aaaaa")
	  (apply orig-fun args)))

      ;; 3. 挂载到 lsp-bridge 的补全后端上
      (advice-add 'acm-backend-capf-candiates :around #'my/lsp-bridge-capf-with-super-cape)
      
      )
    ;;;;;;;;;;
    (add-to-list 'lsp-bridge-default-mode-hooks 'sql-mode-hook)
    (add-hook 'sql-mode 'sql-indent-enable)
    (add-to-list 'acm-backend-capf-mode-list 'text-mode)
    (add-to-list 'acm-backend-capf-mode-list 'prog-mode)
    (add-to-list 'acm-backend-capf-mode-list 'sql-mode)
    (add-to-list 'acm-backend-capf-mode-list 'lisp-interaction-mode)
    (add-to-list 'acm-backend-capf-mode-list 'yaml-mode)
    (add-to-list 'acm-backend-capf-mode-hooks 'yaml-mode-hook)
    (add-to-list 'acm-backend-capf-mode-hooks 'lisp-interaction-mode-hook)
    (add-to-list 'acm-backend-capf-mode-hooks 'prog-mode-hook)
    (add-to-list 'acm-backend-capf-mode-hooks 'text-mode-hook)
    (add-to-list 'acm-backend-capf-mode-hooks 'sql-mode-hook)

    (setq lsp-bridge-symbols-enable-which-func t)
    (setq lsp-bridge-enable-org-babel t)
    (setq lsp-bridge-disable-backup nil)
    (setq acm-enable-quick-access t)
    (setq acm-enable-capf t)
    (setq acm-enable-doc t)

    (setq acm-enable-lsp-workspace-symbol nil)
    (setq acm-backend-lsp-case-mode "smart")
    (setq acm-backend-search-file-words-enable-fuzzy-match t)
    (setq acm-backend-lsp-show-progress t)
    
    (setq lsp-bridge-python-command "python-lsp-bridge")
    ;; (setq lsp-bridge-python-command "python3")
    ;; 用起来有点问题
    ;; (with-eval-after-load 'orderless
    ;; (setq acm-candidate-match-function 'orderless-flex)
    ;; )

    (setq use-lsp-bridge nil)
    (setq use-company nil)
    (if (and
	 (find-system-process-exist "lsp-bridge")
	 (not (lsp-bridge-process-live-p))
	 )
	(progn
	  (setq use-company t)
	  (setq use-lsp-bridge nil)
	  )
      (setq use-lsp-bridge t)
      )
    (with-eval-after-load 'company
      (if (not use-company)
	  (global-company-mode -1)
	)
      )
    (if use-lsp-bridge
	(progn
	  (global-lsp-bridge-mode)
	  )
      )
    )
  )
(use-package cape
  :ensure t
  :demand 
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
   ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :bind ("M-+" . cape-prefix-map)
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
  ;; (setq completion-at-point-functions (list
  ;;                                      (cape-capf-super #'cape-abbrev #'cape-elisp-block #'cape-elisp-symbol #'cape-file #'cape-emoji #'cape-dabbrev #'cape-sgml #'cape-sgml #'cape-tex #'cape-line #'cape-keyword)
  ;;                                      'tags-completion-at-point-function
  ;;                                      ))
  ;; https://github.com/minad/cape
  ;; 还是手动触发吧
  ;; (defun super-cape()
    ;; (cape-wrap-super #'cape-dabbrev #'cape-file #'cape-emoji #'cape-keyword #'cape-abbrev)
    ;; )
  ;; (add-hook 'after-change-major-mode-hook #'(lambda () (add-to-list 'completion-at-point-functions #'super-cape)))
 
  
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
(use-package org-super-agenda
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'org-agenda
    (org-super-agenda-mode)
    (setq org-super-agenda-groups
          '((:name "按项目分组"
                   :auto-parent t))
	  )    
    )
  
  )
;; ime
(use-package rime
  :ensure t
  :bind
  (:map
   rime-mode-map
   ("s-R" . #'rime-force-enable)
   ("s-i" . #'rime-inline-ascii))
  :init
  (setq default-input-method "rime")
  (when +linux
    (setq rime-emacs-module-header-root (file-truename (concat (file-name-directory (directory-file-name (file-truename invocation-directory))) "include")))
    (setq rime-librime-root (file-name-directory(directory-file-name(file-name-directory (file-truename (executable-find "rime_deployer"))))))
    (setq rime-share-data-dir "~/.local/share/fcitx5/rime")
    )
  ;; nix 系统安装librime 会导致报错，其他系统也可能这样
  ;; 防止没有文件
  (unless (file-exists-p rime-share-data-dir)
    (make-directory rime-share-data-dir)
    )
  
  (with-eval-after-load "rime"
    (use-package posframe :ensure t)
    (setq
     rime-show-candidate 'posframe
     rime-show-preedit 'inline
     rime-posframe-properties nil
     )
    (use-package phi-search
      :ensure t
      :init
      (keymap-global-set "s-s s" #'phi-search)
      (keymap-global-set "s-s r" #'phi-search-backward)
      )
    (defun rws-rime-predicate-after-most-ascii-char-p ()
      "If the cursor is after a ascii character expect # [ \ ]"
      (and (> (point) (save-excursion (back-to-indentation) (point)))
           (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
             (string-match-p "[a-zA-Z0-9\x21-\x22\x24-\x2f\x3a-\x40\x5e-\x60\x7b-\x7f]$" string))))

    (defun rws-rime-predicate-space-after-most-cc-p ()
      "If cursor is after a whitespace which follow a non-ascii (except `→') character."
      (and (> (point) (save-excursion (back-to-indentation) (point)))
           (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
             (and (not (string-match-p "→ +$" string))
		  (not (string-match-p "-> +$" string))
                  (string-match-p "\\cc +$" string)))))

    (defun rws-rime-predicate-punctuation-space-after-ascii-p ()
      "If input a punctuation after a ascii charactor with whitespace."
      (and (rime-predicate-current-input-punctuation-p)
           (rime-predicate-space-after-ascii-p)))

    (setq rime-inline-predicates ;; 还是不好用，用disable来处理中英文切换足够了，这个切换过去就切换不回来了，只能手动按rime-inline-ascii，如果需要输入空格，就直接按tab在active状态触发rime-inline-ascii来处理就好了
	  '(
	    ;; rime-predicate-space-after-ascii-p
	    ;; rws-rime-predicate-space-after-most-cc-p
	    ;; rime-predicate-punctuation-after-space-en-p
	    ;; rws-rime-predicate-current-lowercase-letter-and-space-after-most-cc-p
	    ;; rws-rime-predicate-after-most-ascii-char-p
	    ;; rws-rime-predicate-space-after-most-cc-p
	    )
	  )
    (defun l/rime-predicate-prog-in-code-and-other-p ()
      (and (rime-predicate-prog-in-code-p)
	   (or (rws-rime-predicate-punctuation-space-after-ascii-p)
	       (rws-rime-predicate-space-after-most-cc-p)
	       (rws-rime-predicate-after-most-ascii-char-p)
	       (rime-predicate-punctuation-line-begin-p)
	       (rime-predicate-after-alphabet-char-p))
	  )
      )
    (setq rime-disable-predicates
          '(
	    ;; l/rime-predicate-prog-in-code-and-other-p
	    rime-predicate-prog-in-code-p
	    rws-rime-predicate-after-most-ascii-char-p
	    ;; rime-predicate-after-alphabet-char-p
	    
            ;; rime-predicate-current-uppercase-letter-p ;; 这个使用rime自己的英文候选吧
	    ;; rime-predicate-space-after-ascii-p ;; 让 在字母后存在空格时 disable
	    ;; rime-predicate-punctuation-after-space-en-p
	    ))
    ;; 
    (setq
     rime-inline-ascii-holder ?∷
     rime-inline-ascii-trigger 'control-r
     )
    (add-to-list 'rime-translate-keybindings "C-v")
    (add-to-list 'rime-translate-keybindings "M-v")
    ;;
    (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
    ;; (define-key rime-active-mode-map (kbd "C-v") 'rime-send-keybinding)
    ;; (define-key rime-active-mode-map (kbd "M-v") 'rime-send-keybinding)
    (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
    (define-key rime-mode-map (kbd "M-S-j") 'rime-force-enable)
    (define-key rime-mode-map (kbd "M-J") 'rime-force-enable)
    (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
    ;; (setq-default mode-line-mule-info (add-to-list 'mode-line-mule-info '(:eval (rime-lighter)) t))
    (l/safe-insert-to-list 'mode-line-mule-info 1 '(:eval (rime-lighter)))
    (set-face-attribute 'rime-indicator-face nil :foreground "#ff00ff" :background "#ffffff")
    (set-face-attribute 'rime-indicator-dim-face nil :foreground "#00ff00" :background "#000000")
    (set-face-attribute 'rime-default-face nil :foreground "#dcdccc" :background "#333333")
    (set-face-attribute 'rime-preedit-face nil :foreground "#333333")
    ;; (set-face-attribute 'rime-preedit-face nil :foreground "#000000" :background "#ffffff")
    ;; (setq rime-show-preedit 'inline)
    (set-face-attribute 'rime-highlight-candidate-face nil :background "#000000" :foreground "#ffffff")
    
    
    )
  
  )
(use-package pyim :ensure t
  ;; :commands toggle-input-method
  ;; :defer t
  :init
  ;; (defun l/pyim-delay-init()
    ;; (require 'pyim)
    ;; (remove-hook 'input-method-activate-hook 'l/pyim-delay-init)
    ;; )
  (add-hook 'input-method-activate-hook 'l/pyim-delay-init)
  :config
  (use-package pyim-basedict :ensure t
    :config
    (pyim-basedict-enable))

  (use-package pyim-cregexp-utils
    :config
    (setq ivy-re-builders-alist '((t . pyim-cregexp-ivy))))

  (use-package pyim-cstring-utils
    :bind
    (("M-f" . #'pyim-forward-word)
     ("M-b" . #'pyim-backward-word)
     ;; ("C-<left>" . #'pyim-backward-word)
     ;; ("C-<right>" . #'pyim-forward-word)
     ))

  (use-package posframe :ensure t)

  (pyim-scheme-add
   '(ziranma
     :document "自然码双拼（不含形码）方案"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars  "abcdefghijklmnopqrstuvwxyz"
     :prefer-triggers nil
     :cregexp-support-p t
     :keymaps
     (("a" "a" "a")
      ("b" "b" "ou")
      ("c" "c" "iao")
      ("d" "d" "uang" "iang")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("m" "m" "ian")
      ("n" "n" "in")
      ("o" "o" "uo" "o")
      ("p" "p" "un")
      ("q" "q" "iu")
      ("r" "r" "uan")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ia" "ua")
      ("x" "x" "ie")
      ("y" "y" "ing" "uai")
      ("z" "z" "ei")
      ("aa" "a")
      ("an" "an")
      ("aj" "an")
      ("ai" "ai")
      ("al" "ai")
      ("ao" "ao")
      ("ak" "ao")
      ("ah" "ang")
      ("ee" "e")
      ("ei" "ei")
      ("ez" "ei")
      ("en" "en")
      ("ef" "en")
      ("er" "er")
      ("eg" "eng")
      ("oo" "o")
      ("ou" "ou")
      ("ob" "ou"))))

  (setq pyim-default-scheme 'ziranma
        pyim-page-tooltip 'posframe
        pyim-page-length 9
        pyim-fuzzy-pinyin-alist nil)
;; 
  (pyim-isearch-mode t)

  (add-hook 'isearch-mode-hook
            (lambda ()
              (progn
                (setq rws-input-method-before-isearch current-input-method
                      rws-default-input-method-before-isearch default-input-method)
                (deactivate-input-method)
                (setq default-input-method "pyim")
                (setq input-method-history '("pyim")))))

  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (progn
                (activate-input-method rws-input-method-before-isearch)
                (setq default-input-method rws-default-input-method-before-isearch)
                (setq input-method-history (list default-input-method)))))
  )

(setq l/plugin-start t)

(setq completion-styles (seq-difference completion-styles '(partial-completion))) ;; 移除partial-completion,防到最后
(add-to-list 'completion-styles 'partial-completion t) ;; 补充默认的补全功能


