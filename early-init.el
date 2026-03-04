; early-init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:
(setq user-lisp-auto-scrape nil)
(setq package-quickstart t) ;; 使用quickstart 文件来处理初始化
;;(setq package-enable-at-startup nil)	;; 禁止package 自动初始化
;; ui
;;(setq default-frame-alist '((height . 48) (width . 100)))
(setq default-frame-alist
      '((height . 44)
	(width . 180)
	(left . 220)
	(top . 50)
	;; (font . "Fantasque Sans Mono")
	;; (font . "Iosevka Ylagr")
	(font . "Monospace")
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(tool-bar-lines . 0)
	)
      )
(setq use-package-compute-statistics t) ;; use-package计算耗时


(defun l/normal-font ()
  (interactive)
  (set-face-attribute 'default nil :font (font-spec :family "Fantasque Sans Mono" :size 16))
  )

(defun l/resize-frame(frame width height position-x position-y &optional pixel-wise)
  (interactive)
  (set-frame-position frame position-x position-y)
  (set-frame-size frame width height pixel-wise)
  )
(defun l/normal-frame-action (&optional l/frame)
  (unless l/frame
    (setq-local l/frame (selected-frame))
    )
  (when (display-graphic-p)
    (select-frame l/frame)
    (l/normal-font)
    (setq-local attrs (frame-monitor-attributes (selected-frame)))
    (setq-local l/geometry (alist-get 'geometry attrs))
    (setq-local l/display-pixel-width (nth 2 l/geometry))
    (setq-local l/display-pixel-height (nth 3 l/geometry))
    (l/resize-frame
     l/frame
     (round (* l/display-pixel-width 0.9));;(- (/ (display-pixel-width) (frame-char-width)) 20) ;;220
     (round (* l/display-pixel-height 0.80));;(- (/ (display-pixel-height) (frame-char-height)) 20)
     (round (* l/display-pixel-width 0.02));;15
     (round (* l/display-pixel-height 0.01));;10
     t)
    "normal-frame"
    )
  )
(defun l/mini-frame-action(&optional l/frame)
  (unless l/frame
    (setq-local l/frame (selected-frame))
    )
  (when (display-graphic-p)
    (select-frame l/frame)
    (l/normal-font)
    (setq-local attrs (frame-monitor-attributes (selected-frame)))
    (setq-local l/geometry (alist-get 'geometry attrs))
    (setq-local l/display-pixel-width (nth 2 l/geometry))
    (setq-local l/display-pixel-height (nth 3 l/geometry))
    (l/resize-frame
     l/frame
     (round (* l/display-pixel-width 0.75));;(- (/ (display-pixel-width) (frame-char-width)) 20) ;;220
     (round (* l/display-pixel-height 0.8));;(- (/ (display-pixel-height) (frame-char-height)) 20)
     (round (* l/display-pixel-width 0.1));;15
     (round (* l/display-pixel-height 0.01));;10
     t)
    "mini-frame"
    )
  )
(defun l/normal-frame()
  (interactive)
  (l/normal-frame-action (selected-frame))
  )
(defun l/mini-frame()
  (interactive)
  (l/mini-frame-action (selected-frame))
  )

(defun l/focus-frame()
  (select-frame-set-input-focus (selected-frame))
  )
(add-hook 'server-after-make-frame-hook #'l/focus-frame)
(defun l/tty-fix()
  (unless (display-graphic-p)
   (global-auto-composition-mode -1))
  )
(add-hook 'server-after-make-frame-hook #'l/tty-fix)

(if nil
    ""
    (defun l/setup-cjk-fallback (frame)
      ;; (message "l/setup-cjk")
    "为新创建的 FRAME 设置 CJK 字体 Fallback。"
    (with-selected-frame frame
      (when (display-graphic-p);
	;; 设置 Unifont 为 CJK 字符集的备选字体
	
	(let (
	      ;; (cjk-font (font-spec :family "Unifont" :baseline-offset -1 :registry "ios10646-1"))
	      ;; (cjk-font (font-spec :family "Unifont"))
	      (fallback (font-spec :family "Sarasa Gothic SC"))
	      (fallback1 (font-spec :family "文泉驿微米黑" ))
	      ;; (fallback2 (font-spec :family "Noto Serif CJK" ))
	      )
	  (setq-local font-not-set t)
	  (dolist (script '(han
			    kana
			    cjk-misc
			    bopomofo
			    ))
	    ;; (if cjk-font		
		;; (set-fontset-font "fontset-default" script cjk-font nil 'prepend)
		;; (set-fontset-font "fontset-startup" script cjk-font)
	      ;; )
	    (when (and fallback font-not-set)
		;; (set-fontset-font t script fallback )
	      (set-fontset-font "fontset-default" script fallback)
	      (setq-local font-not-set nil)
	      ;; (message "test")
	      )
	    (when (and fallback1 font-not-set)
	      (set-fontset-font "fontset-default" script fallback1)
	      (setq-local font-not-set nil)
	      )
	    ;; (if fallback2
	    ;; 	(set-fontset-font "fontset-startup" script fallback2)
	    ;;   )
	    ;; (set-fontset-font t 'han (font-spec :family "Sarasa Gothic SC"))
	    ;; (set-fontset-font "fontset-startup" 'han (font-spec :family "Sarasa Gothic SC") nil 'prepend)
	    ;; (set-fontset-font "fontset-default" 'han (font-spec :family "Sarasa Gothic SC"))
	    ;; (set-fontset-font "fontset-default" 'han (font-spec :family "文泉驿微米黑"))
	    ;; (set-fontset-font "fontset-default" 'han (font-spec :family "Noto Serif CJK") )
	    ;; (set-fontset-font "fontset-startup" 'han (font-spec :family "Noto Serif CJK") )
	    ;; (frame-parameter nil 'fontset)
	    ;; (face-attribute 'default :fontset)
	    ;; (face-attribute 'default :font)
	    ;; (describe-fontset (frame-parameter nil 'font))
	    ;; (face-attribute 'variable-pitch :family)
	    ;; (face-attribute 'variable-pitch-text :family)
	    ;; (face-attribute 'fixed-pitch :family)
	    ;; (message (format "%s" fallback))
	    ;; (message (format "%s" fallback1))
	    ;; (message (format "%s" fallback2))
	    ;; (message (format "%s" cjk-font))
	    ;; (set-fontset-font "fontset-default" '(#x4E00 . #x9FFF) cjk-font)
	    )
	  )
	;; 如果你希望只在第一个窗口创建时执行一次，可以取消注释下面这行
	(remove-hook 'after-make-frame-functions #'l/setup-cjk-fallback)
	;; (remove-hook 'window-setup-hook #'l/setup-cjk-fallback)
	;; (face-attribute 'variable-pitch :font )
	)
      )
    
    )
    ;; (l/setup-cjk-fallback (selected-frame))

  ;; 针对GUI 启动
  ;;这个daemon也可以用
  (add-hook 'after-make-frame-functions #'l/setup-cjk-fallback)
  ;; (add-hook 'after-make-frame-functions #'l/setup-cjk-fallback)
  ;; (add-hook 'focus-in-hook #'l/setup-cjk-fallback)
  )

;; early-init.el ends here.
