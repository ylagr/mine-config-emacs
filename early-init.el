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
      '((height . 47)
	(width . 180)
	(left . 220)
	(top . 50)
	(font . "Fantasque Sans Mono")
	;; (font . "monospace")
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
;;	(tool-bar-mode . +1)
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
;; early-init.el ends here.
