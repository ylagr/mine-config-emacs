;;; early-init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:
(defconst l:+i-am-in-china +1)
(defvar l:+after-init-hook 'after-init-hook)
(setq gc-cons-threshold 67108864) ;; 64m
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216))) ;; 16m
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defgroup l:custom nil "local custom" :prefix "l:")
(defconst l:+only-tty (and (not (daemonp)) (not (display-graphic-p))))
(defconst l:+mac (eq system-type 'darwin))
(defconst l:+windows (eq system-type 'windows-nt))
(defconst l:+linux (eq system-type 'gnu/linux))
(defconst l:+daemonp (daemonp))
(add-hook 'after-init-hook '(lambda () (require 'init-builtin)))
(defcustom l:leader-key "C-z" "Leader key." :type 'string :group 'l:custom)
(keymap-unset global-map l:leader-key t)
(defvar l:theme:default-background-color "grey94")
(defvar l:theme:default-foreground-color "black")
(defvar l:theme:default-fringe-color "gainsboro")
(defvar l:theme:default-modeline-color "#a1afc9")
(set-face-attribute 'mode-line nil  :box `(:line-width -1 :color ,l:theme:default-modeline-color))
(set-face-attribute 'mode-line-active nil :box `(:line-width -1 :color ,l:theme:default-modeline-color))
(set-face-attribute 'default nil :background l:theme:default-background-color :foreground l:theme:default-foreground-color )
(set-face-attribute 'fringe nil :background l:theme:default-fringe-color)

(defvar l:default-font "Iosevka Ylagr" "custom default font of Iosevka")
(defvar l:fixed-font "Iosevka Ylagr" "temp, may change to other")
;;(defvar l:variable-font "Iosevka Ylagr")
(defvar l:cjk-font "黑体")
(defvar l:kana-font "黑体")
(defvar l:emoji-font "Noto Color Emoji")
(defvar l:egyptian-font "Noto")

(defun l:func:load-face-font ()
  (let ((fontset (frame-parameter nil 'font)))
    (set-frame-parameter nil 'font fontset)
    (set-face-attribute 'fixed-pitch nil :family l:fixed-font :height 1.0)
    ;; (set-face-attribute 'variable-pitch nil :family l:variable-font :height 1.0)
    (dolist (charset '(han hangul cjk-misc bopomofo))
      (set-fontset-font fontset charset l:cjk-font))
    (set-fontset-font fontset 'kana l:kana-font)
    (set-fontset-font fontset 'emoji l:emoji-font)
    (set-fontset-font fontset 'egyptian l:egyptian-font)
    ))

(letrec ((setup-on-first-frame
	      (lambda (frame)
                (when (display-graphic-p frame)
		  (with-selected-frame frame
                    (l:func:load-face-font)
                    (remove-hook 'after-make-frame-functions setup-on-first-frame)))))
	 )
  (add-hook 'after-make-frame-functions setup-on-first-frame))
(defun l:func:safe-create-frame-with-faces (orig-fn &optional parameters)
  "尝试使用指定字体创建 GUI Frame；若因字体不存在触发错误，
自动剔除 `font' 参数并无缝回退到系统默认字体重试。"
  (condition-case err
      (funcall orig-fn parameters)
    (error
     (let ((err-msg (error-message-string err)))
       ;; 捕获字体不存在或 Fontset 'tty' does not exist 错误
       (if (or (string-match-p "Fontset .* does not exist" err-msg)
               (string-match-p "Font .* is not defined" err-msg)
               (string-match-p "Invalid font" err-msg))
           (progn
             (message "警告: 预设字体加载失败 (%s)，已自动回退到默认字体" err-msg)
             ;; 清理 parameters 和全局 default-frame-alist 中的失效 font 配置
             (setq parameters (assq-delete-all 'font (copy-alist parameters)))
             (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
             ;; 使用清理后的参数重新创建 Frame
             (funcall orig-fn parameters))
         ;; 如果是其他无关错误，原样向上抛出
         (signal (car err) (cdr err)))))))
(advice-add 'x-create-frame-with-faces :around #'l:func:safe-create-frame-with-faces)

(setq user-lisp-auto-scrape nil
      initial-frame-alist '((fullscreen . maximized))
      frame-resize-pixelwise t
      default-frame-alist `(
			    (tab-bar-lines . 1)
			    (foreground-color . ,l:theme:default-foreground-color)
			    (background-color . ,l:theme:default-background-color)
			    (tool-bar-lines)
			    (menu-bar-lines . 1)
			    (inhibit-double-buffering)
			    (vertical-scroll-bars)
			    (horizontal-scroll-bars)
			    (inhibit-double-buffering . nil)
			    ;; (fullscreen . maximized)
			    (font . ,l:default-font)
			    )
      use-short-answers t
      ;; use-dialog-box nil
      use-file-dialog nil
      delete-by-moving-to-trash t
      delete-pair-blink-delay 0
      ;; sgml-validate-command nil
      load-suffixes '(".elc" ".el" ".dll")
      duplicate-line-final-position 1
      browse-url-text-browser nil
      browse-url-firefox-program nil
      browse-url-chrome-program nil
      browse-url-chromium-program nil
      browse-url-browser-function 'eww-browse-url)

(setq use-package-expand-minimally t
      package-quickstart t
      package-check-signature nil)
(with-eval-after-load 'package
  (if l:+i-am-in-china
      (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
			       ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
			       ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives '("etaf" . "https://etaf.top/packages/"))
  )
(remove-hook 'find-file-hook #'vc-refresh-state)
(remove-hook 'find-file-hook #'epa-file-find-file-hook)
;; (setq load-suffixes '(".elc" ".el"))

;; statistics , close default
;; (setq use-package-compute-statistics t)

;;(setq package-enable-at-startup nil)	;; 禁止package 自动初始化
;; ui
;;(setq default-frame-alist '((height . 48) (width . 100)))
(when l:+windows
  ;; (unless (getenv "HOME")
  (setq exec-suffixes '("" ".bat" ".exe"))
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (defvar l:executable-find-cache (make-hash-table :test #'equal))
  (define-advice executable-find
      (:around (orig-fun command &optional remote) my-advice)
    (if remote
        (funcall orig-fun command remote)
      (or (gethash command l:executable-find-cache)
          (puthash command (funcall orig-fun command) l:executable-find-cache))))
  
  (when-let* ((home (getenv "USERPROFILE")))
    (setenv "HOME" home)
    (setq abbreviated-home-dir nil)
    (setq-default default-directory (file-name-as-directory home))
    )
  ;; )
  )

;;; old
;; (if t
;;     ""
;;   (defun l/func/normal-font ()
;;     (interactive)
;;     (set-face-attribute 'default nil :font (font-spec :family "Fantasque Sans Mono" :height 180))
;;     )

;;   (defun l/func/resize-frame(frame width height position-x position-y &optional pixel-wise)
;;     (interactive)
;;     (set-frame-position frame position-x position-y)
;;     (set-frame-size frame width height pixel-wise)
;;     )

;;   (defun l/func/get-monitor-scale-factor (&optional frame)
;;     "获取当前显示器的实际缩放比例 (如 1.0, 1.25, 1.5)。"
;;     (let* ((f (or frame (selected-frame)))
;;            (attrs (frame-monitor-attributes f))
;;            (geom (alist-get 'geometry attrs))
;;            (geom-w (nth 2 geom))                    ; 硬件物理像素宽
;;            (disp-w (display-pixel-width f)))        ; Emacs 逻辑像素宽
;;       ;; 如果物理宽度大于逻辑宽度，说明存在系统缩放
;;       (if (and geom-w disp-w (> geom-w 0) (> disp-w 0))
;;           (max 1.0 (/ (float geom-w) (float disp-w)))
;; 	1.0)))
;;   (defun l/func/normal-frame-action (&optional frame)
;;     (let ((f (or frame (selected-frame))))
;;       (when (display-graphic-p f)
;; 	;;(l/func/normal-font)
;; 	(let* ((scale (l/func/get-monitor-scale-factor f))
;;                (attrs (frame-monitor-attributes f))
;;                (geo (alist-get 'geometry attrs))
;;                ;; 将硬件物理分辨率转换为 Emacs 逻辑像素
;;                (disp-w (round (/ (nth 2 geo) scale)))
;;                (disp-h (round (/ (nth 3 geo) scale))))
;;           (l/func/resize-frame
;;            f
;;            (round (* disp-w 0.90))
;;            (round (* disp-h 0.80))
;;            (round (* disp-w 0.02))
;;            (round (* disp-h 0.01))
;;            t)
;;           "normal-frame"))))

;;   (defun l/func/mini-frame-action (&optional frame)
;;     (let ((f (or frame (selected-frame))))
;;       (when (display-graphic-p f)
;; 	;;(l/func/normal-font)
;; 	(let* ((scale (l/func/get-monitor-scale-factor f))
;;                (attrs (frame-monitor-attributes f))
;;                (geo (alist-get 'geometry attrs))
;;                ;; 逻辑像素转换
;;                (disp-w (round (/ (nth 2 geo) scale)))
;;                (disp-h (round (/ (nth 3 geo) scale))))
;;           (l/func/resize-frame
;;            f
;;            (round (* disp-w 0.75))
;;            (round (* disp-h 0.80))
;;            (round (* disp-w 0.10))
;;            (round (* disp-h 0.01))
;;            t)
;;           "mini-frame"))))

;;   (defun l/func/normal-frame () (interactive) (l/func/normal-frame-action))
;;   (defun l/func/mini-frame () (interactive) (l/func/mini-frame-action))

;;   (defun l/func/focus-frame()
;;     (select-frame-set-input-focus (selected-frame))
;;     )

;; ;;; 6. 终端/Daemon 钩子优化
;;   (add-hook 'server-after-make-frame-hook #'select-frame-set-input-focus)
;;   (add-hook 'server-after-make-frame-hook
;;             (lambda ()
;;               (unless (display-graphic-p)
;; 		(global-auto-composition-mode -1))))

;;   (defun l/func/tty-fix()
;;     (unless (display-graphic-p)
;;       (global-auto-composition-mode -1))
;;     )
;;   (add-hook 'server-after-make-frame-hook #'l/func/tty-fix)


;; ;;; 1. 字体候选列表（按优先级从高到低）
;;   (defvar l/candidate-latin-fonts
;;     '(
;;       "Iosevka Ylagr"
;;       "Consolas"
;;       "Monospace"
;;       "JetBrains Mono"
;;       "Fantasque Sans Mono"
;;       )
;;     "候选英文字体列表。")

;;   (defvar l/candidate-cjk-fonts
;;     '(
;;       ;; "SimHei"             ; 黑体
;;       ;;"思源黑体"

;;       "Sarasa Mono SC"
;;       ;; "更纱黑体"
;;       "Microsoft YaHei"       ; 微软雅黑
;;       "文泉驿微米黑"          ; 微米黑
;;       "Sarasa Gothic SC"      ; 更纱黑体
;;       )  
;;     "候选 CJK 中文字体列表。")

;;   (defvar l/default-font-pt 12
;;     "默认字号（点数 pt）。")
;;   (defvar l/ui-font-scale 0.9
;;     "界面组件（tab-line, mode-line 等）字体相对默认字号的缩放比例。")
;;   (defvar l/cjk-fallback-initialized nil
;;     "标记 CJK Fallback 是否已全局初始化。")
;; ;;; 直接在 default-frame-alist 中以优先级字符串写入


;;   ;; 1. 窗口最大化参数（保留系统任务栏）
;;   (setq initial-frame-alist '((fullscreen . maximized)
;; 			      ;; (visibility . nil)
;; 			      )
;; 	)

;;   ;; 初始界面尺寸及布局（杜绝创建后再拉伸）
;;   (setq default-frame-alist
;; 	(append
;; 	 '(
;; 	   ;;(height . 44)
;;            ;;(width . 160)
;;            ;;(left . 200)
;;            ;;(top . 50)
;; 	   (fullscreen . maximized)
;; 	   ;;	 (font . "Iosevka Ylagr")
;; 	   ;;	 (visibility . nil)
;;            (vertical-scroll-bars . nil)
;;            (horizontal-scroll-bars . nil)
;;            (tool-bar-lines . 0)
;;            (menu-bar-lines . 1)
;;            (inhibit-double-buffering . nil))
;; 	 default-frame-alist))


;; ;;; 2. 毫秒级安全探测函数
;;   (defun l/func/find-first-available-family (candidates fallback)
;;     "从 CANDIDATES 中查找系统已安装的首个字族名称；若均无则返回 FALLBACK。"
;;     (let ((installed (font-family-list)))
;;       (or (seq-find (lambda (f) (member-ignore-case f installed)) candidates)
;;           fallback)))

;;   (defun l/func/init-font ()
;; ;;; --- 2. 快速存在性检测 ---
;;     (defun l/find-first-available-font (font-list)
;;       "查找列表中第一个系统存在的字体。"
;;       (cl-find-if (lambda (font-name)
;;                     (and (font-info font-name) font-name))
;; 		  font-list))
;;     (defun l/setup-all-fonts (&optional frame)
;;       "配置英文字体与 CJK Fallback。"
;;       (when (display-graphic-p frame)
;; 	;; 1. 设置英文字体
;; 	(when-let* ((latin (l/find-first-available-font l/candidate-latin-fonts)))
;; 	  (let ((font-str (format "%s-%d" latin l/default-font-pt)))
;;             ;; 同步写入 default-frame-alist，供新建窗口继承
;;             (setf (alist-get 'font default-frame-alist) font-str)
;; 	    (message font-str)
;;             ;; 应用到当前 frame，第三个参数 t 保证窗口大小不被拉扯
;;             (set-frame-font font-str nil t)))

;; 	;; 2. 设置中文字体（全局仅需注入一次 fontset-default）
;; 	(when-let* ((cjk (l/find-first-available-font l/candidate-cjk-fonts)))
;; 	  (let ((cjk-spec (font-spec :family cjk)))
;;             (dolist (script '(han kana cjk-misc bopomofo))
;;               (set-fontset-font "fontset-default" script cjk-spec nil 'prepend))))))

    
;;   ;;; --- 4. 挂载入口（兼顾常规启动与 Daemon 模式） ---
;;     (if (daemonp)
;; 	(letrec ((setup-first-gui-frame
;; 		  (lambda (frame)
;;                     (when (display-graphic-p frame)
;;                       (l/setup-all-fonts frame)
;;                       (remove-hook 'after-make-frame-functions setup-first-gui-frame)))))
;; 	  (add-hook 'after-make-frame-functions setup-first-gui-frame))
;;       (add-hook 'window-setup-hook #'l/setup-all-fonts))
;;     )

;;   (defun l/func/init-font-v2 ()
;; ;;; 2. 快速字体检测工具
;;     (defun l/find-first-available-font (font-list)
;;       "从 FONT-LIST 中按顺序寻找系统中首个可用的字体。不存在则返回 nil。"
;;       (cl-find-if (lambda (font-name)
;;                     (and (font-info font-name)
;; 			 font-name))
;; 		  font-list))

;; ;;; 3. 字体设置主函数（英文字体 + 中文字体回退）
;;     (defun l/setup-all-fonts (&optional frame)
;;       "为 FRAME 或全局配置英文字体及 CJK Fallback 中文字体。"
;;       (when (display-graphic-p frame)
;; 	;; --- A. 配置英文字体 ---
;; 	(when-let* ((target-latin (l/find-first-available-font l/candidate-latin-fonts)))
;; 	  ;; 设置 default face
;; 	  (set-face-attribute 'default frame
;;                               :font (font-spec :family target-latin :size 12.0))
;; 	  ;; 写入 default-frame-alist，供后续新建 client/frame 直接继承
;; 	  (setf (alist-get 'font default-frame-alist) target-latin))

;; 	;; --- B. 配置 CJK 中文字体 ---
;; 	(when-let* ((target-cjk (l/find-first-available-font l/candidate-cjk-fonts)))
;; 	  (let ((cjk-font (font-spec :family target-cjk)))
;;             ;; 设置到全局 "fontset-default"，frame 参数传 nil 表示全局生效
;;             (dolist (script '(han kana cjk-misc bopomofo))
;;               (set-fontset-font "fontset-default" script cjk-font nil 'prepend))))))

;; ;;; 4. 挂载入口：兼顾常规 GUI 启动与 Daemon (emacsclient) 模式
;;     (if (daemonp)
;; 	;; daemon 模式：首个 GUI 客户端连入时配置一次，随后自注销
;; 	(letrec ((setup-on-first-frame
;; 		  (lambda (frame)
;;                     (when (display-graphic-p frame)
;;                       (l/setup-all-fonts frame)
;;                       (remove-hook 'after-make-frame-functions setup-on-first-frame)))))
;; 	  (add-hook 'after-make-frame-functions setup-on-first-frame))
;;       ;; 普通非 daemon GUI 启动模式
;;       (add-hook 'window-setup-hook #'l/setup-all-fonts))
;;     )
;;   )
;;(l/func/init-font-v2)
;; (l/func/init-font)

;;; early-init.el ends here
