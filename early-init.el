; early-init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; code:

;; ui
;;(setq default-frame-alist '((height . 48) (width . 100)))
(setq default-frame-alist
      '((height . 47)
	(width . 180)
	(left . 220)
	(top . 50)
	(font . "Fantasque Sans Mono")
;;	(vertical-scroll-bars . +1)
	(horizontal-scroll-bars . +1)
;;	(tool-bar-mode . +1)
	)
      )


(defun normal-font ()
  (interactive)
  (set-face-attribute 'default nil :font (font-spec :family "Fantasque Sans Mono" :size 16))
  )

(defun resize-frame(frame width height position-x position-y &optional pixel-wise)
  (interactive)
  (set-frame-position frame position-x position-y)
  (set-frame-size frame width height pixel-wise)
  )
(defun normal-frame-action (&optional frame)
  (unless frame
    (setq-local frame (selected-frame))
    )
  (when (display-graphic-p)
    (select-frame frame)
    (normal-font)
    (setq-local attrs (frame-monitor-attributes (selected-frame)))
    (setq-local geometry (alist-get 'geometry attrs))
    (setq-local l/display-pixel-width (nth 2 geometry))
    (setq-local l/display-pixel-height (nth 3 geometry))
    (resize-frame
     (selected-frame)
     (round (* l/display-pixel-width 0.9));;(- (/ (display-pixel-width) (frame-char-width)) 20) ;;220
     (round (* l/display-pixel-height 0.80));;(- (/ (display-pixel-height) (frame-char-height)) 20)
     (round (* l/display-pixel-width 0.02));;15
     (round (* l/display-pixel-height 0.01));;10
     t)
    "normal-frame"
    )
  )
(defun mini-frame-action(&optional frame)
  (unless frame
    (setq-local frame (selected-frame))
    )
  (when (display-graphic-p)
    (select-frame frame)
    (normal-font)
    (setq-local attrs (frame-monitor-attributes (selected-frame)))
    (setq-local geometry (alist-get 'geometry attrs))
    (setq-local l/display-pixel-width (nth 2 geometry))
    (setq-local l/display-pixel-height (nth 3 geometry))
    (resize-frame
     frame
     (round (* l/display-pixel-width 0.75));;(- (/ (display-pixel-width) (frame-char-width)) 20) ;;220
     (round (* l/display-pixel-height 0.8));;(- (/ (display-pixel-height) (frame-char-height)) 20)
     (round (* l/display-pixel-width 0.1));;15
     (round (* l/display-pixel-height 0.01));;10
     t)
    (raise-frame frame)
    (select-frame frame)
    "mini-frame"
    )
  )
(defun normal-frame()
  (interactive)
  (normal-frame-action (selected-frame))
  )
(defun mini-frame()
  (interactive)
  (mini-frame-action (selected-frame))
  )

(defun focus-frame()
  (select-frame-set-input-focus (selected-frame))
  )
(add-hook 'server-after-make-frame-hook #'focus-frame)

